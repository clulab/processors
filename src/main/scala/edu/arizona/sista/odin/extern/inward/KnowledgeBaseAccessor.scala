package edu.arizona.sista.odin.extern.inward

import edu.arizona.sista.odin._

/**
  * Traits and classes used to access information from external knowledge bases.
  *   Written by Tom Hicks. 3/18/2015.
  *   Last Modified: Port to odin branch: adapt for grounding scheme.
  */
trait ExternalKBAccessor {
  /** The primary URI of the external KB (e.g., http://identifiers.org/uniprot/). */
  def baseURI: String

  /** The namespace of the external KB (e.g., uniprot). */
  def namespace: String

  /** The Resource Identifier for the primary resource location for this
    * knowledge base (e.g., MIR:00100164).
    * NB: This is MIRIAM registration ID of the external knowledge base, NOT an entity ID. */
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
    * and return a map of keys and property values from that entry.
    * Default method to be overridden by each child knowledge base accessor.
    */
  def resolve (mention:Mention): Map[String,String] = {
    return Map(
      "referenceID" -> "UNRESOLVED ID",     // the real resolver must return real value!
      "alternateIDs" -> "",                 // a list of alternate IDs might be available
      "baseURI" -> baseURI,
      "definition" -> "",                   // a term definition might be available
      "key" -> getLookupKey(mention),
      "namespace" -> namespace,             // the namespace string of this accessor
      "referenceURI" -> referenceURI(mention),
      "resourceID" -> resourceID,           // MIRIAM registration ID
      "standardName" -> ""                  // standard nomenclature might be available
    )
  }

}
