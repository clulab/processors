package org.clulab.odin.impl

/**
 * Custom Exceptions for Odin
 */

/**
 * This is the basic Exception for Odin
 * @param msg a String describing the Exception
 */
class OdinException(val msg: String) extends RuntimeException(msg)
// unapply method must live in the companion object
object  OdinException {
  def apply(msg: String) = new OdinException(msg)
  def unapply(e: OdinException): Option[String] = Some(e.msg)
}

/**
 * This is the basic compile Exception for Odin
 * @param msg a description of the compilation error encountered
 */
class OdinCompileException(msg: String) extends OdinException(msg)

object  OdinCompileException {
  def apply(msg: String) = new OdinCompileException(msg)
  def unapply(e: OdinCompileException): Option[String] = Some(e.msg)
}

/**
 * This is the compile Exception for Odin when the failing rule name is available
 * @param msg a description of the compilation error encountered
 * @param ruleName the name of the failing rule
 */
class OdinNamedCompileException(msg: String, val ruleName: String) extends OdinCompileException(msg)

object OdinNamedCompileException {
  def apply(msg: String, ruleName: String) = new OdinNamedCompileException(msg, ruleName)
  def unapply(e: OdinNamedCompileException): Option[(String, String)] = Some((e.msg, e.ruleName))
}

