package org.clulab.utils

trait BeforeAndAfter {
  def before(): Unit
  def after(): Unit

  def perform[T](f: => T): T = {
    before()
    try {
      f
    }
    finally {
      after()
    }
  }
}
