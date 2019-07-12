package org.clulab.struct

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.serialization.DocumentSerializer
import org.clulab.serialization.json._
import org.clulab.struct.test.CaseClass
import org.clulab.struct.test.ObjectNameDocumentAttachment
import org.clulab.struct.test.NameDocumentAttachment
import org.clulab.struct.test.TextNameDocumentAttachment
import org.clulab.utils.Closer.AutoCloser

import org.json4s.jackson.parseJson
import org.json4s.jackson.prettyJson
import org.json4s.jackson.renderJValue

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestDocumentAttachment extends FlatSpec with Matchers {
  protected val FIRST_KEY = "first"
  protected val MIDDLE_KEY = "middle"
  protected val LAST_KEY = "last"
  protected val ALIAS_KEY = "alias"

  protected val FIRST_NAME = "First"
  protected val MIDDLE_NAME = "Middle"
  protected val LAST_NAME = "Last"
  protected val ALIAS_NAME = "Alias"

  def serialize(any: Any): Array[Byte] = {
    new ByteArrayOutputStream().autoClose { byteArrayOutputStream =>
      new ObjectOutputStream(byteArrayOutputStream).autoClose { objectOutputStream =>
        try {
          objectOutputStream.writeObject(any)
        }
        catch {
          case exception: Exception =>
            exception.printStackTrace()
            throw exception
        }
      }
      byteArrayOutputStream.toByteArray
    }
  }

  def deserialize[T](byteArray: Array[Byte]): T = {
    new ByteArrayInputStream(byteArray).autoClose { byteArrayInputStream =>
      new ObjectInputStream(byteArrayInputStream).autoClose { objectInputStream =>
        try {
          val res1 = objectInputStream.readObject()
          val res2 = res1.asInstanceOf[T]
          res2
        }
        catch {
          case exception: Exception =>
            exception.printStackTrace()
            throw exception
        }
      }
    }
  }

  "String" should "serialize" in {
    val oldString = "This is a test"
    val bytes = serialize(oldString)
    val newString: String = deserialize(bytes)

    newString should be (oldString)
  }

  "CaseClass" should "serialize" in {
    val oldCaseClass = CaseClass("This is a test")
    val bytes = serialize(oldCaseClass)
    val newCaseClass: CaseClass = deserialize(bytes)

    newCaseClass should be (oldCaseClass)
  }

  "DocumentAttachment" should "serialize" in {
    val oldDocumentAttachment = new TextNameDocumentAttachment("First Middle Last")
    val bytes = serialize(oldDocumentAttachment)

    oldDocumentAttachment.serialized should be (true)

    val newDocumentAttachment: TextNameDocumentAttachment = deserialize(bytes)
    newDocumentAttachment should be (oldDocumentAttachment)
  }

  // This test fails in SBT.  It may be related to things like https://github.com/sbt/sbt/issues/89.
  // I am disabling it for now.  It does run in IntelliJ.

//  "Document with TextNameDocumentAttachments" should "serialize" in {
//    val oldDocument = new Document(Array.empty[Sentence])
//
//    oldDocument.addAttachment(FIRST_KEY, new TextNameDocumentAttachment(FIRST_NAME))
//    oldDocument.addAttachment(MIDDLE_KEY, new TextNameDocumentAttachment(MIDDLE_NAME))
//    oldDocument.addAttachment(LAST_KEY, new TextNameDocumentAttachment(LAST_NAME))
//    oldDocument.addAttachment(ALIAS_KEY, new NameDocumentAttachment(ALIAS_NAME))
//    // This shouldn't compile.
//    /*oldDocument.addAttachment("wrong", new NameMethodAttachment("name"))*/
//
//    val documentByteArray = serialize(oldDocument)
//    oldDocument.getAttachment(FIRST_KEY).asInstanceOf[Option[TextNameDocumentAttachment]].get.serialized should be (true)
//    oldDocument.getAttachment(MIDDLE_KEY).asInstanceOf[Option[TextNameDocumentAttachment]].get.serialized should be (true)
//    oldDocument.getAttachment(LAST_KEY).asInstanceOf[Option[TextNameDocumentAttachment]].get.serialized should be (true)
//
//    // SBT has problems with this line, giving ClassNotFoundException.  The document is in the
//    // main code while the attachment is in the test code.  Can that be the problem?
//    // The first complaint is about NameDocumentAttachment, but if that is removed,
//    // it goes on to complain about TextNameDocumentAttachment.
//    val newDocument: Document = deserialize(documentByteArray)
//    newDocument.getAttachment(FIRST_KEY) should be (oldDocument.getAttachment(FIRST_KEY))
//    newDocument.getAttachment(MIDDLE_KEY) should be (oldDocument.getAttachment(MIDDLE_KEY))
//    newDocument.getAttachment(LAST_KEY) should be (oldDocument.getAttachment(LAST_KEY))
//    newDocument.getAttachment(ALIAS_KEY).asInstanceOf[Option[NameDocumentAttachment]].get.name should be (
//      oldDocument.getAttachment(ALIAS_KEY).asInstanceOf[Option[NameDocumentAttachment]].get.name
//    )
//
//    // This one must be avoided.
//    /*require(newDocument == oldDocument)*/
//  }

  "Document with TextNameDocumentAttachment" should "serialize as text" in {
    val oldDocument = new Document(Array.empty[Sentence])

    oldDocument.addAttachment(FIRST_KEY, new TextNameDocumentAttachment(FIRST_NAME))
    oldDocument.addAttachment(MIDDLE_KEY, new TextNameDocumentAttachment(MIDDLE_NAME))
    oldDocument.addAttachment(LAST_KEY, new TextNameDocumentAttachment(LAST_NAME))
    oldDocument.addAttachment(ALIAS_KEY, new NameDocumentAttachment(ALIAS_NAME))

    val documentSerializer = new DocumentSerializer()
    val documentString = documentSerializer.save(oldDocument)

    val newDocument = documentSerializer.load(documentString)
    require(newDocument.getAttachment(FIRST_KEY) == oldDocument.getAttachment(FIRST_KEY))
    require(newDocument.getAttachment(MIDDLE_KEY) == oldDocument.getAttachment(MIDDLE_KEY))
    require(newDocument.getAttachment(LAST_KEY) == oldDocument.getAttachment(LAST_KEY))
    require(newDocument.getAttachment(ALIAS_KEY).get.asInstanceOf[NameDocumentAttachment].name ==
        oldDocument.getAttachment(ALIAS_KEY).get.asInstanceOf[NameDocumentAttachment].name)

    // This one must be avoided.
    /*require(newDocument == oldDocument)*/
  }

  "Document with ObjectNameDocumentAttachment" should "serialize as text" in {
    val oldDocument = new Document(Array.empty[Sentence])

    oldDocument.addAttachment(FIRST_KEY, new ObjectNameDocumentAttachment(FIRST_NAME))
    oldDocument.addAttachment(MIDDLE_KEY, new ObjectNameDocumentAttachment(MIDDLE_NAME))
    oldDocument.addAttachment(LAST_KEY, new ObjectNameDocumentAttachment(LAST_NAME))
    oldDocument.addAttachment(ALIAS_KEY, new NameDocumentAttachment(ALIAS_NAME))

    val documentSerializer = new DocumentSerializer()
    // This should be a messy string.
    val documentString = documentSerializer.save(oldDocument)

    val newDocument = documentSerializer.load(documentString)
    require(newDocument.getAttachment(FIRST_KEY) == oldDocument.getAttachment(FIRST_KEY))
    require(newDocument.getAttachment(MIDDLE_KEY) == oldDocument.getAttachment(MIDDLE_KEY))
    require(newDocument.getAttachment(LAST_KEY) == oldDocument.getAttachment(LAST_KEY))
    require(newDocument.getAttachment(ALIAS_KEY).get.asInstanceOf[NameDocumentAttachment].name ==
        oldDocument.getAttachment(ALIAS_KEY).get.asInstanceOf[NameDocumentAttachment].name)

    // This one must be avoided.
    /*require(newDocument == oldDocument)*/
  }

  "Document with TextNameDocumentAttachments" should "serialize as json" in {
    val oldDocument = new Document(Array.empty[Sentence])

    oldDocument.addAttachment(FIRST_KEY, new TextNameDocumentAttachment(FIRST_NAME))
    oldDocument.addAttachment(MIDDLE_KEY, new TextNameDocumentAttachment(MIDDLE_NAME))
    oldDocument.addAttachment(LAST_KEY, new TextNameDocumentAttachment(LAST_NAME))
    oldDocument.addAttachment(ALIAS_KEY, new NameDocumentAttachment(ALIAS_NAME))
    // This shouldn't compile.
    /*oldDocument.addAttachment("wrong", new NameMethodAttachment("name"))*/

    val documentString = prettyJson(renderJValue(oldDocument.jsonAST))

    val newDocument: Document = JSONSerializer.toDocument(parseJson(documentString))
    newDocument.getAttachment(FIRST_KEY) should be (oldDocument.getAttachment(FIRST_KEY))
    newDocument.getAttachment(MIDDLE_KEY) should be (oldDocument.getAttachment(MIDDLE_KEY))
    newDocument.getAttachment(LAST_KEY) should be (oldDocument.getAttachment(LAST_KEY))
    newDocument.getAttachment(ALIAS_KEY).asInstanceOf[Option[NameDocumentAttachment]].get.name should be (
      oldDocument.getAttachment(ALIAS_KEY).asInstanceOf[Option[NameDocumentAttachment]].get.name
    )

    // This one must be avoided.
    /*require(newDocument == oldDocument)*/
  }

  "Document with ObjectNameDocumentAttachment" should "serialize as json" in {
    val oldDocument = new Document(Array.empty[Sentence])

    oldDocument.addAttachment(FIRST_KEY, new ObjectNameDocumentAttachment(FIRST_NAME))
    oldDocument.addAttachment(MIDDLE_KEY, new ObjectNameDocumentAttachment(MIDDLE_NAME))
    oldDocument.addAttachment(LAST_KEY, new ObjectNameDocumentAttachment(LAST_NAME))
    oldDocument.addAttachment(ALIAS_KEY, new NameDocumentAttachment(ALIAS_NAME))

    // This should be a messy string.
    val documentString = prettyJson(renderJValue(oldDocument.jsonAST))

    val newDocument: Document = JSONSerializer.toDocument(parseJson(documentString))
    require(newDocument.getAttachment(FIRST_KEY) == oldDocument.getAttachment(FIRST_KEY))
    require(newDocument.getAttachment(MIDDLE_KEY) == oldDocument.getAttachment(MIDDLE_KEY))
    require(newDocument.getAttachment(LAST_KEY) == oldDocument.getAttachment(LAST_KEY))
    require(newDocument.getAttachment(ALIAS_KEY).get.asInstanceOf[NameDocumentAttachment].name ==
        oldDocument.getAttachment(ALIAS_KEY).get.asInstanceOf[NameDocumentAttachment].name)

    // This one must be avoided.
    /*require(newDocument == oldDocument)*/
  }
}

