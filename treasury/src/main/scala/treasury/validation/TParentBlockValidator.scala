package treasury.validation

import examples.hybrid.blocks.{HybridBlock, PowBlock}
import scorex.core.block.BlockValidator
import scorex.crypto.encode.Base58
import treasury.block.TPosBlock
import treasury.history.THistoryStorage

import scala.util.Try

class TParentBlockValidator(storage: THistoryStorage)
  extends BlockValidator[HybridBlock] {

  def validate(block: HybridBlock): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock => if (!storage.isGenesis(powBlock)) {
        //check PoW parent id ???
        require(storage.modifierById(powBlock.parentId).isDefined, s"Parent ${Base58.encode(powBlock.parentId)} missed")
        //check referenced PoS block exists as well
        val posBlock = storage.modifierById(powBlock.prevPosId).get

        //check referenced PoS block points to parent PoW block
        require(posBlock.parentId sameElements posBlock.parentId, "ref rule broken")
      }
      case posBlock: TPosBlock =>
        //check PoW block exists
        require(storage.modifierById(posBlock.parentId).isDefined)
    }
  }

}