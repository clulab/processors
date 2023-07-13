package org.clulab.utils

import org.scalatest._

import java.io.Closeable
import scala.io.Source
import scala.util.Using

class TestAutoClosing extends Test {

  class Closing(exception: Option[Throwable] = None) extends Closeable {
    var closed: Boolean = false // test

    def close(): Unit = {
      closed = true

      exception.map(exception => throw exception)
    }
  }

  behavior of "Closing"

  it should "be able to produce a simple result" in {
    val closing = new Closing()
    val result = Using.resource(closing) { _ =>
      5
    }
    result should be (5)
    closing.closed should be (true)
  }

  it should "be able to produce a null result" in {
    val closing = new Closing()
    val result: AnyRef = Using.resource(closing) { _ =>
      null
    }

    result should be (null)
    closing.closed should be (true)
  }

  it should "be able to produce a None result" in {
    val closing = new Closing()
    val result = Using.resource(closing) { _ =>
      None
    }
    result should be (None)
    closing.closed should be (true)
  }

  it should "be able to produce a Some result" in {
    val closing = new Closing()
    val result = Using.resource(closing) { _ =>
      Some(5)
    }
    result should be (Some(5))
    closing.closed should be (true)
  }

  it should "close on a nonfatal exception" in {
    val closing = new Closing()

    an [IllegalStateException] should be thrownBy {
      Using.resource(closing)(_ => throw new IllegalStateException("Boom!"))
    }
    closing.closed should be (true)
  }

  it should "close on a fatal exception" in {
    val closing = new Closing()

    an [StackOverflowError] should be thrownBy {
      Using.resource(closing)(_ => throw new StackOverflowError("Boom!"))
    }
    closing.closed should be (true)
  }

  it should "close on a nonfatal close exception" in {
    val closing = new Closing(Some(new IllegalStateException("Boom!")))

    an [IllegalStateException] should be thrownBy {
      Using.resource(closing)(_ => "Hello")
    }
    closing.closed should be (true)
  }

  it should "close on a fatal close exception" in {
    val closing = new Closing(Some(new StackOverflowError("Boom!")))

    an [StackOverflowError] should be thrownBy {
      Using.resource(closing)(_ => "Hello")
    }
    closing.closed should be (true)
  }

  it should "close on multiple nonfatal exceptions" in {
    val closing = new Closing(Some(new IllegalStateException("Boom!")))

    an [RuntimeException] should be thrownBy {
      Using.resource(closing)(_ => throw new RuntimeException("Boom!"))
    }
    closing.closed should be (true)
  }

  it should "close on multiple fatal exceptions" in {
    val closing = new Closing(Some(new OutOfMemoryError("Boom!")))

    an [StackOverflowError] should be thrownBy {
      Using.resource(closing)(_ => throw new StackOverflowError("Boom!"))
    }
    closing.closed should be (true)
  }

  it should "close on one nonfatal and one fatal exception" in {
    val closing = new Closing(Some(new IllegalStateException("Boom!")))

    an [StackOverflowError] should be thrownBy {
      Using.resource(closing)(_ => throw new StackOverflowError("Boom!"))
    }
    closing.closed should be (true)
  }

  it should "close on one fatal and one nonfatal exception" in {
    val closing = new Closing(Some(new OutOfMemoryError("Boom!")))

    an [OutOfMemoryError] should be thrownBy {
      Using.resource(closing)(_ => throw new IllegalStateException("Boom!"))
    }
    closing.closed should be (true)
  }

  it should "not close if argument throws an exception" in {
    val closing = new Closing(None)

    def getClosing: Closing = {
      throw new RuntimeException("Boom!")
    }

    an [RuntimeException] should be thrownBy {
      Using.resource(getClosing)( _ => 5)
    }
    closing.closed should be (false)
  }

  it should "work with a plain Source, even in Scala 2.11" in {
    Using.resource(Source.fromString("foo\nbar\n")) { source =>
      source.getLines().toList
    }
  }
}
