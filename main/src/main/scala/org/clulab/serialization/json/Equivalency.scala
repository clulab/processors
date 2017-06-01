package org.clulab.serialization.json


trait Equivalency {
  /** Custom hash used to establish equivalence */
  def equivalenceHash: Int

  /** string used to denote the class */
  val stringCode: String

  /** id encoding class info and equivalence */
  def id: String = s"$stringCode:$equivalenceHash"

}