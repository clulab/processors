package edu.arizona.sista.odin

/** Represents an entry in an external database. Used for grounding mentions */
case class Xref(namespace: String, id: String) {
  /** Return a printable string representation of this Xref. */
  def printString (): String = { s"${namespace}:${id}" }
}
