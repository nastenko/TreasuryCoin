package treasury.validation

import examples.hybrid.blocks.{HybridBlock, PowBlock}
import scorex.core.block.BlockValidator
import scorex.crypto.hash.{CryptographicHash, Digest}
import treasury.block.TPosBlock

import scala.util.Try

class TSemanticBlockValidator(hash: CryptographicHash[_ <: Digest]) extends BlockValidator[HybridBlock] {

  def validate(block: HybridBlock): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock =>
        require(powBlock.brothersCount >= 0)
        require(powBlock.timestamp >= 0)

        //check brothers data
        require(powBlock.brothers.size == powBlock.brothersCount)
        if (powBlock.brothersCount > 0) {
          require(hash(powBlock.brotherBytes) sameElements powBlock.brothersHash)
        }
      case posBlock: TPosBlock =>
        require(posBlock.timestamp >= 0)
        require(TPosBlock.signatureValid(posBlock))
    }
  }

}
