package org.clulab.scaladynet.builders

import org.clulab.scaladynet.expressions.Expression

class Builder {
  // So this should be false for read only
  def newGraph(update: Boolean = true): Unit = ???
  def startNewSequence(): Unit = ???
  def addInput(x: Expression): Expression = ???
  def setDropout(dropout: Float): Unit = ???
  def disableDropout(): Unit = ???
}
