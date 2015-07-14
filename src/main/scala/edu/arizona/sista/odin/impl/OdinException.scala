package edu.arizona.sista.odin.impl

/**
 * Custom Exceptions for Odin
 */
class OdinException(msg: String) extends RuntimeException(msg)

case class OdinCompileException(msg: String, ruleName: Option[String] = None) extends OdinException(msg)
