package edu.arizona.sista.odin.export.biopax

import edu.arizona.sista.odin.Mention

/**
  * Traits and classes used to access information from external knowledge bases.
  *   Written by Tom Hicks. 3/18/2015.
  *   Last Modified: Port to 5.3.
  */
trait ExternalKBAccessor {
  /** The primary URI of the external KB (e.g., http://identifiers.org/uniprot/). */
  def baseURI: String

  /** The namespace of the external KB (e.g., uniprot). */
  def namespace: String

  /** The Resource Identifier for the primary resource location for this
    * knowledge base (e.g., MIR:00100164). */
  def resourceID: String


  /** Extract and return a key string, from the given Mention, which can be used
    * to access an external knowledge base or a local object cache. */
  def getLookupKey (mention:Mention): String = {
    return mention.text                     // default: lookup by mention name
  }

  /**
    * Using the given ID string, generate a URI which references an entry
    * in the namespace of external knowledge base
    * (e.g., given "P2345" => "http://identifiers.org/uniprot/P2345").
    */
  def referenceURI (id:String): String = {
    return s"${baseURI}${id}"
  }

  /** Using the given Mention, generate a URI which references an entry
    * in the namespace of external knowledge base. */
  def referenceURI (mention:Mention): String = {
    return referenceURI(getLookupKey(mention))
  }


  /** Resolve the given Mention to an entry in an external knowledge base
    * and return a map of keys and property values from that entry. */
  // def resolve (mention:Mention): Map[String,String] = {
  //   return Map[String,String]()
  // }

  // Mock method until we do real resolution against the knowledge bases
  def resolve (mention:Mention): Map[String,String] = {
    return Map(
      "key" -> getLookupKey(mention),
      "baseURI" -> baseURI,
      "namespace" -> namespace,
      "referenceURI" -> referenceURI(mention),
      "referenceID" -> "MOCK-referenceId",   // the real resolver must return real value!
      "standardName" -> "MOCK-standardName", // standard nomenclature might be available
      "resourceID" -> resourceID
    )
  }
}


class UniprotKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprotkb"
  def resourceID = "MIR:00100164"
}


class ChEBIKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/chebi/"
  def namespace = "chebi"
  def resourceID = "MIR:00100565"
}


class GeneOntologyKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/go/"
  def namespace = "go"
  def resourceID = "MIR:00100013"
}
