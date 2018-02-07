package examples.hybrid

import akka.actor.{ActorRef, Props}
import examples.commons.{SimpleBoxTransaction, TreasuryMemPool}
import examples.hybrid.api.http.{DebugApiRoute, StatsApiRoute, WalletApiRoute, TreasuryApiRoute}
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HybridHistory, HybridSyncInfo, HybridSyncInfoMessageSpec}
import examples.hybrid.mining.{PosForger, PowMiner}
import examples.hybrid.settings.HybridSettings
import examples.hybrid.transaction.TreasuryTxForger
import examples.hybrid.wallet.TreasuryTransactionGenerator
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

class HybridApp(val settingsFilename: String) extends Application {

  override type P = PublicKey25519Proposition
  override type TX = SimpleBoxTransaction
  override type PMOD = HybridBlock
  override type NVHT = HybridNodeViewHolder

  private val hybridSettings = HybridSettings.read(Some(settingsFilename))
  implicit override lazy val settings: ScorexSettings = HybridSettings.read(Some(settingsFilename)).scorexSettings

  log.debug(s"Starting application with settings \n$settings")

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(HybridSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new HybridNodeViewHolder(settings, hybridSettings.mining, timeProvider)))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings.restApi, nodeViewHolderRef),
    WalletApiRoute(settings.restApi, nodeViewHolderRef),
    StatsApiRoute(settings.restApi, nodeViewHolderRef),
    UtilsApiRoute(settings.restApi),
    NodeViewApiRoute[P, TX](settings.restApi, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkControllerRef, settings.restApi),
    TreasuryApiRoute(settings.restApi, nodeViewHolderRef),
  )

  override val swaggerConfig: String = Source.fromResource("api/testApi.yaml").getLines.mkString("\n")

  val miner = actorSystem.actorOf(Props(new PowMiner(nodeViewHolderRef, hybridSettings.mining)))
  val forger = actorSystem.actorOf(Props(new PosForger(hybridSettings, nodeViewHolderRef)))
  val treasuryTxsForger: ActorRef = actorSystem.actorOf(Props(new TreasuryTxForger(nodeViewHolderRef, hybridSettings.treasurySettings)))

  override val localInterface: ActorRef = actorSystem.actorOf(Props(
    new HLocalInterface(nodeViewHolderRef, miner, forger, treasuryTxsForger, hybridSettings.mining)))

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(new NodeViewSynchronizer[P, TX, HybridSyncInfo, HybridSyncInfoMessageSpec.type, PMOD, HybridHistory, TreasuryMemPool]
    (networkControllerRef, nodeViewHolderRef, localInterface, HybridSyncInfoMessageSpec, settings.network, timeProvider)))

  //touching lazy vals
  miner
  localInterface
  nodeViewSynchronizer

//  if (settings.network.nodeName.startsWith("node1")) {
//    log.info("Starting simple transactions generation")
//    val generator: ActorRef = actorSystem.actorOf(Props(new SimpleBoxTransactionGenerator(nodeViewHolderRef)))
//    generator ! SimpleBoxTransactionGenerator.StartGeneration(10 seconds)
//  }

  if (settings.network.nodeName.startsWith("node1")) {
    log.info("Starting treasury transactions generation")
    val generator: ActorRef = actorSystem.actorOf(Props(new TreasuryTransactionGenerator(nodeViewHolderRef)))
    generator ! TreasuryTransactionGenerator.StartGeneration(15 seconds)
  }
}

object HybridApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.conf")
  new HybridApp(settingsFilename).run()
}
