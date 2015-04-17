package edu.arizona.sista.odin.domains.bigmechanism.summer2015

import scala.io.Source

import edu.arizona.sista.odin._
import edu.arizona.sista.odin.extern.inward._

/**
  * A collections of classes which implement project internal knowledge base accessors.
  *   Written by Tom Hicks. 4/10/2015.
  *   Last Modified: Use canonicalized storage and lookup keys.
  */

/**
  * Abstract class which reads two-column, tab-separated-value (TSV) text files
  * where the first column is the name string and the second is the ID string.
  * Several of our knowledge bases follow this pattern and can simply extend this class.
  */
abstract class AzNameIdKBAccessor extends ExternalKBAccessor {
  protected val theKB = scala.collection.mutable.Map[String, Map[String,String]]()

  override def getLookupKey (mention:Mention): String = {
    return makeKBCanonKey(mention.text)   // canonicalize text for KBs
  }

  override def resolve (mention:Mention): Map[String,String] = {
    val key = getLookupKey(mention)         // make a key from the mention
    theKB.getOrElseUpdate(key, Map.empty)   // lookup the key
  }

  protected def readAndFillKB (kbResourcePath:String) = {
    val source: Source = sourceFromResource(kbResourcePath)
    for (line <- source.getLines) {
      val fields = line.split("\t").map(_.trim)
      if ((fields.size == 2) && fields(0).nonEmpty && fields(1).nonEmpty) {
        val text = fields(0)
        val storageKey = makeKBCanonKey(text)
        theKB(storageKey) = Map(            // create new entry in KB
          "referenceID" -> fields(1),
          "namespace" -> namespace,
          "baseURI" -> baseURI,
          "resourceID" -> resourceID,
          "key" -> storageKey,
          "text" -> text                    // also return original text
        )
      }
    }
    source.close()
  }
}


/**
  * Abstract class which reads three-column, tab-separated-value (TSV) text files
  * where 1st column is the name string, 2nd column is the species, and 3rd column is the ID string.
  * Some of our knowledge bases follow this pattern and can simply extend this class.
  */
abstract class AzNameSpeciesIdKBAccessor extends ExternalKBAccessor {
  protected val theKB = scala.collection.mutable.Map[String, Map[String,String]]()

  override def getLookupKey (mention:Mention): String = {
    return makeKBCanonKey(mention.text)   // canonicalize text for KBs
  }

  override def resolve (mention:Mention): Map[String,String] = {
    val key = getLookupKey(mention)         // make a key from the mention
    theKB.getOrElseUpdate(key, Map.empty)   // lookup the key
  }

  protected def readAndFillKB (kbResourcePath:String) = {
    val source: Source = sourceFromResource(kbResourcePath)
    for (line <- source.getLines) {
      val fields = line.split("\t").map(_.trim)
      if ((fields.size == 3) && fields(0).nonEmpty && fields(2).nonEmpty) {
        val text = fields(0)
        val storageKey = makeKBCanonKey(text)
        theKB(storageKey) = Map(               // create new entry in KB
          "referenceID" -> fields(2),
          "namespace" -> namespace,
          "baseURI" -> baseURI,
          "resourceID" -> resourceID,
          "key" -> storageKey,
          "species" -> fields(1),
          "text" -> text                    // also return original text
        )
      }
    }
    source.close()
  }
}


/** KB accessor to resolve protein names in mentions. */
class AzProteinKBAccessor extends AzNameSpeciesIdKBAccessor {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprotkb"
  def resourceID = "MIR:00100164"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/uniprot-proteins.tsv.gz")
}


/** KB accessor to resolve protein family names in mentions. */
class AzProteinFamilyKBAccessor extends AzNameSpeciesIdKBAccessor {
  def baseURI = "http://identifiers.org/interpro/"
  def namespace = "interpro"
  def resourceID = "MIR:00000011"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/ProteinFamilies.tsv.gz")
}


/** KB accessor to resolve small molecule (chemical) names in mentions. */
class AzSmallMoleculeKBAccessor extends AzNameIdKBAccessor {
  def baseURI = "http://identifiers.org/hmdb/"
  def namespace = "hmdb"
  def resourceID = "MIR:00000051"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/hmdb.tsv.gz")
}


/** KB accessor to resolve subcellular location names in mentions using GeneOntology DB. */
class AzSubcellularLocationKBAccessor extends AzNameIdKBAccessor {
  def baseURI = "http://identifiers.org/go/"
  def namespace = "go"
  def resourceID = "MIR:00000022"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/GO-subcellular-locations.tsv")
}

/** KB accessor to resolve subcellular location names in mentions using Uniprot DB. */
class AzSubcellularLocationKBAccessor2 extends AzNameIdKBAccessor {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprot"
  def resourceID = "MIR:00000005"

  // MAIN: load KB to initialize class
  readAndFillKB("/edu/arizona/sista/odin/domains/bigmechanism/summer2015/uniprot-subcellular-locations.tsv")
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
      "referenceID" -> "UAZ%05d".format(idCntr.next),
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
