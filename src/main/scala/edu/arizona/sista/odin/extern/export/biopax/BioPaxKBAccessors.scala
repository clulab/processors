package edu.arizona.sista.odin.extern.export.biopax

import edu.arizona.sista.odin._
import edu.arizona.sista.odin.extern.inward._

/**
  * BioPAX specific classes used to access information from external knowledge bases.
  *   Written by Tom Hicks. 3/18/2015.
j  *   Last Modified: Split accessor implementations from refactored parent trait.
  */
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

class MolecularInteractionsKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/psimi/"
  def namespace = "psimi"
  def resourceID = "MIR:00000109"
}

class UniprotKBAccessor extends ExternalKBAccessor {
  def baseURI = "http://identifiers.org/uniprot/"
  def namespace = "uniprotkb"
  def resourceID = "MIR:00100164"
}
