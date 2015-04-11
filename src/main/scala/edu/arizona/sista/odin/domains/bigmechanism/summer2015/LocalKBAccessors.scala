package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import scala.io.Source

import edu.arizona.sista.odin._
import edu.arizona.sista.odin.extern.inward._

/**
  * A collections of classes which implement project internal knowledge base accessors.
  *   Written by Tom Hicks. 4/10/2015.
  *   Last Modified: Abstract out the 2 column, TSV KB accessor.
  */

/**
  * Abstract class which reads two-column, tab-separated-value (TSV) text files
  * where the first column is the name string and the second is the ID string.
  * Several of our knowledge bases follow this pattern and can simply extend this class.
  */
abstract class AzNameIdKBAccessor extends ExternalKBAccessor {
  protected val theKB = scala.collection.mutable.Map[String, Map[String,String]]()

  override def resolve (mention:Mention): Map[String,String] = {
    val key = getLookupKey(mention).toLowerCase // lookup keys are all lowercase in this KB
    theKB.getOrElseUpdate(key, Map.empty)
  }

  protected def readAndFillKB (kbResourcePath:String) = {
    val kbStream = this.getClass.getResourceAsStream(kbResourcePath)
    for (line <- Source.fromInputStream(kbStream).getLines) {
      val fields = line.split("\t").map(_.trim)
      if ((fields.size == 2) && fields(0).nonEmpty && fields(1).nonEmpty) {
        val storageKey = fields(0).toLowerCase // lookup keys are all lowercase in this KB
        theKB(storageKey) = Map(               // create new entry in KB
          "referenceID" -> fields(1),
          "namespace" -> namespace,
          "baseURI" -> baseURI,
          "resourceID" -> resourceID,
          "key" -> storageKey,
          "text" -> fields(0)               // return original text
        )
      }
    }
    kbStream.close()
  }
}


/** KB accessor to resolve protein names in mentions. */
class AzProteinKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprotkb"
  def resourceID = "MIR:00100164"
  override def resolve (mention:Mention): Map[String,String] = Map.empty // TODO: IMPLEMENT
}


/** KB accessor to resolve protein family names in mentions. */
class AzProteinFamilyKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/pfam/"
  def namespace = "pfam"
  def resourceID = "MIR:00000028"
  override def resolve (mention:Mention): Map[String,String] = Map.empty // TODO: IMPLEMENT
}


/** KB accessor to resolve small molecule (chemical) names in mentions. */
class AzSmallMoleculeKBAccessor extends AzNameIdKBAccessor {
  def baseURI = "http://identifiers.org/hmdb/"
  def namespace = "hmdb"
  def resourceID = "MIR:00000051"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/hmdb.tsv")
}


/** KB accessor to resolve subcellular location names in mentions. */
class AzSubcellularLocationKBAccessor extends AzNameIdKBAccessor {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprot"
  def resourceID = "MIR:00000005"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/subcellular_locations.tsv")
}


/** KB accessor to resolve tissue type names in mentions. */
class AzTissueTypeKBAccessor extends AzNameIdKBAccessor {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprot"
  def resourceID = "MIR:00000005"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/tissue-type.tsv")
}


/** KB accessor implementation which always resolves each mention with a local, fake ID. */
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

/** Class to implement an incrementing counter for generating unique IDs. */
class IncrementingCounter {
  protected var cntr:Int = 0
  def current(): Int = { cntr }
  def next(): Int = {
    cntr += 1
    return cntr
  }
}
