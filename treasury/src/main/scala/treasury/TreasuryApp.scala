package treasury

import akka.actor.{ActorRef, Props}
import examples.commons.SimpleBoxTransaction
import examples.hybrid.api.http.{DebugApiRoute, StatsApiRoute, WalletApiRoute}
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HybridSyncInfo, HybridSyncInfoMessageSpec}
import examples.hybrid.mining.HybridSettings
import examples.hybrid.wallet.SimpleBoxTransactionGenerator
import examples.hybrid.wallet.SimpleBoxTransactionGenerator.StartGeneration
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import treasury.api.http.{TDebugApiRoute, TStatsApiRoute, TWalletApiRoute}
import treasury.mining.{TPosForger, TPowMiner}
import treasury.wallet.TSimpleBoxTransactionGenerator

import scala.concurrent.duration._
import scala.language.postfixOps

class TreasuryApp(val settingsFilename: String) extends Application {

  override type P = PublicKey25519Proposition
  override type TX = SimpleBoxTransaction
  override type PMOD = HybridBlock
  override type NVHT = TreasuryNodeViewHolder

  protected val hybridSettings = HybridSettings.read(Some(settingsFilename))
  implicit override lazy val settings: ScorexSettings = HybridSettings.read(Some(settingsFilename)).scorexSettings

  log.debug(s"Starting application with settings \n$settings")

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(HybridSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new TreasuryNodeViewHolder(settings, hybridSettings.mining)))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    TDebugApiRoute(settings.restApi, nodeViewHolderRef),
    TWalletApiRoute(settings.restApi, nodeViewHolderRef),
    TStatsApiRoute(settings.restApi, nodeViewHolderRef),
    UtilsApiRoute(settings.restApi),
    NodeViewApiRoute[P, TX](settings.restApi, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkController, settings.restApi)
  )

  override val apiTypes: Set[Class[_]] = Set(classOf[UtilsApiRoute], classOf[DebugApiRoute], classOf[WalletApiRoute],
    classOf[NodeViewApiRoute[P, TX]], classOf[PeersApiRoute], classOf[StatsApiRoute])

  val miner = actorSystem.actorOf(Props(new TPowMiner(nodeViewHolderRef, hybridSettings.mining)))
  val forger = actorSystem.actorOf(Props(new TPosForger(hybridSettings, nodeViewHolderRef)))

  override val localInterface: ActorRef = actorSystem.actorOf(Props(new TLocalInterface(nodeViewHolderRef, miner, forger, hybridSettings.mining)))

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(new NodeViewSynchronizer[P, TX, HybridSyncInfo, HybridSyncInfoMessageSpec.type]
    (networkController, nodeViewHolderRef, localInterface, HybridSyncInfoMessageSpec, settings.network)))

  //touching lazy vals
  miner
  localInterface
  nodeViewSynchronizer

  if (settings.network.nodeName.startsWith("generatorNode")) {
    log.info("Starting transactions generation")
    val generator: ActorRef = actorSystem.actorOf(Props(new TSimpleBoxTransactionGenerator(nodeViewHolderRef)))
    generator ! StartGeneration(10 seconds)
  }
}

object TreasuryApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.conf")
  new TreasuryApp(settingsFilename).run()
}
