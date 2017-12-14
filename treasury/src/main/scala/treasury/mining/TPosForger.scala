package treasury.mining

import akka.actor.{Actor, ActorRef}
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridBlock, PowBlock}
import examples.hybrid.mining.HybridSettings
import examples.hybrid.wallet.HWallet
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.{CurrentView, GetDataFromCurrentView}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256
import scorex.utils.Random
import treasury.block.TPosBlock
import treasury.history.TreasuryHistory
import treasury.state.TBoxStoredState
import treasury.wallet.TWallet


class TPosForger(settings: HybridSettings, viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import TPosForger._

  var forging = false

  override def receive: Receive = {
    case StartForging =>
      forging = true
      viewHolderRef ! getRequiredData

    case pfi: TPosForgingInfo =>
      val target = settings.mining.MaxTarget / pfi.diff

      val boxKeys = pfi.boxKeys

      //last check on whether to forge at all
      if (pfi.pairCompleted) {
        self ! StopForging
      } else {
        val powBlock = pfi.bestPowBlock
        log.debug(s"Trying to generate PoS block on top of ${powBlock.encodedId} with balance " +
          s"${boxKeys.map(_._1.value.toLong).sum}")
        val attachment = Random.randomBytes(settings.mining.posAttachmentSize)
        posIteration(powBlock, boxKeys, pfi.txsToInclude, attachment, target) match {
          case Some(posBlock) =>
            log.debug(s"Locally generated PoS block: $posBlock")
            forging = false
            viewHolderRef !
              LocallyGeneratedModifier[HybridBlock](posBlock)
          case None =>
            log.debug(s"Failed to generate PoS block")
        }
      }

    case StopForging =>
      forging = false
  }
}

object TPosForger extends ScorexLogging {
  val InitialDifficuly = 150000000L

  case object StartForging

  case object StopForging

  def hit(pwb: PowBlock)(box: PublicKey25519NoncedBox): BigInt = {
    val h = Blake2b256(pwb.bytes ++ box.bytes)
    BigInt(1, h)
  }

  def posIteration(powBlock: PowBlock,
                   boxKeys: Seq[(PublicKey25519NoncedBox, PrivateKey25519)],
                   txsToInclude: Seq[SimpleBoxTransaction],
                   attachment: Array[Byte],
                   target: BigInt
                  ): Option[TPosBlock] = {
    val successfulHits = boxKeys.map { boxKey =>
      val h = hit(powBlock)(boxKey._1)
      (boxKey, h)
    }.filter(t => t._2 < t._1._1.value * target)

    log.info(s"Successful hits: ${successfulHits.size}")

    successfulHits.headOption.map { case (boxKey, _) =>
      TPosBlock.create(
        powBlock.id,
        System.currentTimeMillis(),
        txsToInclude,
        boxKey._1,
        attachment,
        boxKey._2)
    }
  }

  val getRequiredData: GetDataFromCurrentView[TreasuryHistory,
    TBoxStoredState,
    TWallet,
    SimpleBoxTransactionMemPool,
    TPosForgingInfo] = {
    val f: CurrentView[TreasuryHistory, TBoxStoredState, TWallet, SimpleBoxTransactionMemPool] => TPosForgingInfo = {
      view: CurrentView[TreasuryHistory, TBoxStoredState, TWallet, SimpleBoxTransactionMemPool] =>

        val diff = view.history.posDifficulty
        val pairCompleted = view.history.pairCompleted
        val bestPowBlock = view.history.bestPowBlock
        val boxes = view.vault.boxes().map(_.box).filter(box => view.state.closedBox(box.id).isDefined)
        val boxKeys = boxes.flatMap(b => view.vault.secretByPublicImage(b.proposition).map(s => (b, s)))

        val txs = view.pool.take(TransactionsPerBlock).foldLeft(Seq[SimpleBoxTransaction]()) { case (collected, tx) =>
          if (view.state.validate(tx).isSuccess &&
            tx.boxIdsToOpen.forall(id => !collected.flatMap(_.boxIdsToOpen)
              .exists(_ sameElements id))) collected :+ tx
          else collected
        }

        TPosForgingInfo(pairCompleted, bestPowBlock, diff, boxKeys, txs)
    }
    GetDataFromCurrentView[TreasuryHistory,
      TBoxStoredState,
      TWallet,
      SimpleBoxTransactionMemPool,
      TPosForgingInfo](f)

  }

  val TransactionsPerBlock = 50

}

case class TPosForgingInfo(pairCompleted: Boolean,
                          bestPowBlock: PowBlock,
                          diff: BigInt,
                          boxKeys: Seq[(PublicKey25519NoncedBox, PrivateKey25519)],
                          txsToInclude: Seq[SimpleBoxTransaction])
