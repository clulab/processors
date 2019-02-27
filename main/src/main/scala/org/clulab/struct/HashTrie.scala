package org.clulab.struct

@SerialVersionUID(1000L)
class HashTrie(caseInsensitive: Boolean = true, internStrings: Boolean = true)
    extends BooleanHashTrie("", caseInsensitive, internStrings) {

  def find(sequence:Array[String], label: String, outsideLabel: String): Array[String] =
      if (caseInsensitive) findNormalized(sequence.map(_.toLowerCase), label, outsideLabel)
      else findNormalized(sequence, label, outsideLabel)

  protected def findNormalized(tokens: Array[String], label: String, outsideLabel: String): Array[String] = {
    val labels = new Array[String](tokens.length)
    lazy val bLabel = "B-" + label // lazy thinking that most calls will not use it
    lazy val iLabel = "I-" + label
    var offset = 0

    def setNextLabel(label: String): Unit = {
      labels(offset) = label
      offset += 1
    }

    while (offset < tokens.length) {
      val span = findAt(tokens, offset).length

      if (span > 0) {
        setNextLabel(bLabel)
        for (i <- 1 until span)
          setNextLabel(iLabel)
      }
      else
        setNextLabel(outsideLabel)
    }
    labels
  }
}
