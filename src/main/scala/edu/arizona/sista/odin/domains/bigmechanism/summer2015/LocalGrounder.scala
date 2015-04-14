package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import edu.arizona.sista.odin._

/**
  * Class which implements project internal methods to ground entities.
  *   Written by Tom Hicks. 4/6/2015.
  *   Last Modified: Use subcellular locations (cell components) KB.
  */
class LocalGrounder extends DarpaFlow {
  /** An exception in case we somehow fail to assign an ID during resolution. */
  case class NoFailSafe(message:String) extends Exception(message)

  /** Project local sequence for resolving entities: check local facade KBs in this order:
    * 1. Protein Families KB
    * 2. AZ Protein KB
    * 3. AZ SmallMolecule BK
    * 4. AZ Failsafe KB (failsafe: always generates an ID in a non-official, local namespace)
    */
  protected val searchSequence = Seq(
    new AzProteinFamilyKBAccessor,
    new AzProteinKBAccessor,
    new AzSmallMoleculeKBAccessor,
    new AzSubcellularLocationKBAccessor,
//    new AzTissueTypeKBAccessor,
    new AzFailsafeKBAccessor
  )


  /** Local implementation of trait: use project specific KBs to ground and augment given mentions. */
  def apply (mentions: Seq[Mention], state: State): Seq[Mention] = mentions map {
    case tm: TextBoundMention => resolveAndAugment(tm, state)
    case m => m
  }

  /** Search the KB accessors in sequence, use the first one which resolves the given mention. */
  private def resolveAndAugment (mention: Mention, state: State): Mention = {
    searchSequence.foreach { kbAccessor =>
      val resInfo = kbAccessor.resolve(mention)
      if (!resInfo.isEmpty) {
        return mention.ground(resInfo("namespace"), resInfo("referenceID"))
      }
    }
    // we should never get here because our accessors include a failsafe ID assignment
    throw NoFailSafe(s"LocalGrounder failed to assign an ID to ${mention.label} '${mention.text}' in S${mention.sentence}")
  }
}
