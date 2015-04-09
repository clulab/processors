package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import util.control.Breaks._
import scala.io.Source

import edu.arizona.sista.odin._
import edu.arizona.sista.odin.extern.inward._

/**
  * Class which implements project internal methods to ground entities.
  *   Written by Tom Hicks. 4/6/2015.
  *   Last Modified: Implemented small chemical lookup.
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
    new AzProteinFamiliesKBAccessor,
    new AzProteinKBAccessor,
    new AzSmallMoleculeKBAccessor,
    new AzFailsafeKBAccessor
  )


  /** Local implementation of trait: use project specific KBs to ground and augment given mentions. */
  def apply (mentions: Seq[Mention], state: State): Seq[Mention] = {
    mentions.foreach { _ match {
      case tm:TextBoundMention => resolveAndAugment(tm, state)
      case _ =>
    }}
    return mentions
  }

  /** Search the KB accessors in sequence, use the first one which resolves the given mention. */
  private def resolveAndAugment (mention: Mention, state: State): Unit = {
    breakable {
      searchSequence.foreach { kbAccessor =>
        val resInfo = kbAccessor.resolve(mention)
        if (!resInfo.isEmpty) {
          mention.ground(resInfo("namespace"), resInfo("referenceID"))
          break                             // resolved: exit out now
        }
      }
      // we should never get here because our accessors include a failsafe ID assignment
      throw NoFailSafe(s"LocalGrounder failed to assign an ID to ${mention.label} '${mention.text}' in S${mention.sentence}")
    }  // end breakable
  }
}


/** KB accessor implementation which always resolves the given mention with a local, fake ID. */
class AzFailsafeKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://edu.arizona.sista.odin/uazid/"
  def namespace = "uazid"
  def resourceID = "MIR:00000000"           // mock MIRIAM registration number

  private val idCntr = new IncrementingCounter() // counter sequence class
  private val seenIt = scala.collection.mutable.Map[String, Map[String,String]]()

  override def resolve (mention:Mention): Map[String,String] = {
    val key = getLookupKey(mention)
    seenIt.getOrElseUpdate(key, newResolution(key))
  }

  private def newResolution (key:String): Map[String,String] = {
    return Map(
      "referenceID" -> "UAZID:%05d".format(idCntr.next),
      "namespace" -> namespace,
      "baseURI" -> baseURI,
      "key" -> key
    )
  }
}


class AzProteinKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprotkb"
  def resourceID = "MIR:00100164"
  override def resolve (mention:Mention): Map[String,String] = Map.empty
}


class AzProteinFamiliesKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/pfam/"
  def namespace = "pfam"
  def resourceID = "MIR:00000028"
  override def resolve (mention:Mention): Map[String,String] = Map.empty
}


class AzSmallMoleculeKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/hmdb/"
  def namespace = "hmdb"
  def resourceID = "MIR:00000051"

  private val hmdb = scala.collection.mutable.Map[String, Map[String,String]]()

  override def resolve (mention:Mention): Map[String,String] = {
    val key = getLookupKey(mention).toLowerCase // lookup keys are all lowercase in this KB
    hmdb.getOrElseUpdate(key, Map.empty)
  }

  private def readAndFillKB = {
    val kbStream = this.getClass.getResourceAsStream(
      "/edu/arizona/sista/odin/domains/bigmechanism/summer2015/hmdb.kb")
    for (line <- Source.fromInputStream(kbStream).getLines) {
      val fields = line.split("\t").map(_.trim)
      if ((fields.size > 2) && fields(0).nonEmpty && fields(2).nonEmpty) {
        val storageKey = fields(0).toLowerCase // lookup keys are all lowercase in this KB
        hmdb(storageKey) = Map(             // create new entry in KB
          "referenceID" -> fields(2),
          "namespace" -> namespace,
          "baseURI" -> baseURI,
          "resourceID" -> resourceID,
          "key" -> storageKey,
          "text" -> fields(0)               // return original text
        )
      }
    }
  }

  // load KB to initialize class
  readAndFillKB
}


/** Class to implement an incrementing counter for generating unique IDs. */
class IncrementingCounter {
  protected var cntr:Int = 0
  def current(): Int = { cntr }
  def next(): Int = {
    cntr += 1
    return cntr
  }
}
