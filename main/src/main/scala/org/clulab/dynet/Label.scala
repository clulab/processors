package org.clulab.dynet

trait Label {
  def label: String
}

case class PrimalLabel(label: String) extends Label

/**
 * Label information for a dual task that classifies pairs of words (modifier and head)
 * Note: offsets for modifier and head start at 0. "root" heads have index -1
 */
case class DualLabel(modifier: Int, head: Int, label: String) extends Label {
  def modifierHeadPair: (Int, Int) = Tuple2(modifier, head)
}

/**
 * Indexes for pairs of words (modifier and head)
 * Note: offsets for modifier and head start at 0. "root" heads have index -1
 */
case class ModifierHeadPair(modifier: Int, head: Int)