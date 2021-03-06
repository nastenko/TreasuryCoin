package hybrid

import akka.actor.{ActorRef, ActorSystem, Props}
import examples.commons.SimpleBoxTransactionMemPool
import examples.hybrid.HybridNodeViewHolder
import examples.hybrid.wallet.HWallet
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.VersionTag
import scorex.core.utils.ByteStr

trait NodeViewHolderGenerators { this: ModifierGenerators with StateGenerators with HistoryGenerators with HybridTypes =>

  class NodeViewHolderForTests(h: HT, s: ST) extends HybridNodeViewHolder(settings.scorexSettings, settings.mining) {

    override protected def genesisState: (HIS, MS, VL, MP) = {
      val store = lsmStoreGen.sample.get
      val byteStr = ByteStr(Array.fill(10)(1:Byte))
      val gw = new HWallet(byteStr, store)
      (h, s, gw, SimpleBoxTransactionMemPool.emptyPool)
    }

    override def restoreState(): Option[(HIS, MS, VL, MP)] = None
  }

  object NodeViewHolderForTests {
    def props(h: HT, s: ST): Props = Props(new NodeViewHolderForTests(h, s))
  }

  def nodeViewHolder(implicit system: ActorSystem): (ActorRef, PM, ST, HT) = {
    val h = historyGen.sample.get
    val sRaw = stateGen.sample.get
    val v = h.openSurfaceIds().last
    sRaw.store.update(ByteArrayWrapper(v), Seq(), Seq())
    val s = sRaw.copy(version = VersionTag @@ v)
    val ref = system.actorOf(NodeViewHolderForTests.props(h, s))
    val m = totallyValidModifier(h, s)
    (ref, m, s, h)
  }
}