package org.clulab.scaladynet.trainers

abstract class Trainer {

  def clippingEnabled_=(b: Boolean): Unit = ???

  def clipThreshold_=(x: Float): Unit = ???

  def update(): Unit = ???
}
