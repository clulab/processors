package org.clulab.struct

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import org.clulab.processors.Document
import org.clulab.processors.DocumentAttachment
import org.clulab.processors.DocumentAttachmentBuilder
import org.clulab.processors.Sentence
import org.clulab.serialization.DocumentSerializer
import org.clulab.utils.Closer.AutoCloser

object TestDocumentAttachmentApp extends App { // FlatSpec with Matchers {
  protected val FIRST = "first"
  protected val MIDDLE = "middle"
  protected val LAST = "last"
  protected val ALIAS = "alias"

  case class CaseClass(name: String)

  // DocumentAttachment provides serialization itself now, so this doesn't need Serializable.
  // This is an example of the minimum functionality needed for a DocumentAttachment.
  class SerializableDocumentAttachment(val name: String) extends DocumentAttachment

  // These methods are only there for testing.  Production classes do not need them.
  // This one doesn't provide nice serialization for DocumentSerializer.
  class ObjectNameDocumentAttachment(name: String) extends SerializableDocumentAttachment(name) {
    var serialized: Boolean = false

    private def writeObject(out: ObjectOutputStream): Unit = {
      serialized = true
      // If this isn't done, the stream will EOF prematurely on deserialization.
      out.defaultWriteObject()
    }

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[ObjectNameDocumentAttachment]

      this.name == that.name && this.serialized == that.serialized
    }
  }

  // See the superclass for documentation.
  class NameDocumentBuilder extends DocumentAttachmentBuilder {

    def mkDocumentAttachment(text: String): TextNameDocumentAttachment = {
      new TextNameDocumentAttachment(text)
    }
  }

  class TextNameDocumentAttachment(name: String) extends SerializableDocumentAttachment(name) {
    var serialized: Boolean = false

    override val documentAttachmentBuilderClassName: String = classOf[NameDocumentBuilder].getName

    private def writeObject(out: ObjectOutputStream): Unit = {
      serialized = true
      // If this isn't done, the stream will EOF prematurely on deserialization.
      out.defaultWriteObject()
    }

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[TextNameDocumentAttachment]

      this.name == that.name && this.serialized == that.serialized
    }

    override def toDocumentSerializer: String = {
      name
    }
  }

  trait MethodAttachment

  class NameMethodAttachment(val name: String) extends MethodAttachment {

  }

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
        objectInputStream.readObject().asInstanceOf[T]
      }
    }
  }

  /*"String" should "serialize" in*/ {
    val oldString = "This is a test"
    val bytes = serialize(oldString)
    val newString: String = deserialize(bytes)

    require(newString == oldString)
    /*newString should be (oldString)*/
  }

  /*"CaseClass" should "serialize" in*/ {
    val oldCaseClass = CaseClass("This is a test")
    val bytes = serialize(oldCaseClass)
    val newCaseClass: CaseClass = deserialize(bytes)

    require(newCaseClass == oldCaseClass)
    /*newCaseClass should be (oldCaseClass)*/
  }

  /*"DocumentAttachment" should "serialize" in*/ {
    val oldDocumentAttachment = new TextNameDocumentAttachment("First Middle Last")
    val bytes = serialize(oldDocumentAttachment)

    require(oldDocumentAttachment.serialized)
    /*oldDocumentAttachment.serialized should be (true)*/

    val newDocumentAttachment: TextNameDocumentAttachment = deserialize(bytes)
    require(newDocumentAttachment == oldDocumentAttachment)
    /*newDocumentAttachment should be (oldDocumentAttachment)*/
  }

  /*"Document with TextNameDocumentAttachments" should "serialize" in*/ {
    val oldDocument = new Document(Array.empty[Sentence])

    oldDocument.addAttachment(FIRST, new TextNameDocumentAttachment("First"))
    oldDocument.addAttachment(MIDDLE, new TextNameDocumentAttachment("M."))
    oldDocument.addAttachment(LAST, new TextNameDocumentAttachment("Last"))
    oldDocument.addAttachment(ALIAS, new SerializableDocumentAttachment("Alias"))
    // This shouldn't compile.
    /*oldDocument.addAttachment("wrong", new NameMethodAttachment("name"))*/

    val documentByteArray = serialize(oldDocument)
    require(oldDocument.getAttachment(FIRST).asInstanceOf[Option[TextNameDocumentAttachment]].get.serialized)
    /*oldDocument.getAttachment(FIRST).asInstanceOf[Option[NameDocumentAttachment]].get.serialized should be (true)*/
    require(oldDocument.getAttachment(MIDDLE).asInstanceOf[Option[TextNameDocumentAttachment]].get.serialized)
    /*oldDocument.getAttachment(MIDDLE).asInstanceOf[Option[NameDocumentAttachment]].get.serialized should be (true)*/
    require(oldDocument.getAttachment(LAST).asInstanceOf[Option[TextNameDocumentAttachment]].get.serialized)
    /*oldDocument.getAttachment(LAST).asInstanceOf[Option[NameDocumentAttachment]].get.serialized should be (true)*/

    val newDocument: Document = deserialize(documentByteArray)
    require(newDocument.getAttachment(FIRST) == oldDocument.getAttachment(FIRST))
    /*newDocument.getAttachment(FIRST) should be (oldDocument.getAttachment(FIRST))*/
    require(newDocument.getAttachment(MIDDLE) == oldDocument.getAttachment(MIDDLE))
    /*newDocument.getAttachment(MIDDLE) should be (oldDocument.getAttachment(MIDDLE))*/
    require(newDocument.getAttachment(LAST) == oldDocument.getAttachment(LAST))
    /*newDocument.getAttachment(LAST) should be (oldDocument.getAttachment(LAST))*/
    require(newDocument.getAttachment(ALIAS).get.asInstanceOf[SerializableDocumentAttachment].name == "Alias")
    /*newDocument.getAttachment(ALIAS) should be (oldDocument.getAttachment(ALIAS))*/

    // This one must be avoided.
    /*require(newDocument == oldDocument)*/
  }

  {
    val oldDocument = new Document(Array.empty[Sentence])

    oldDocument.addAttachment(FIRST, new TextNameDocumentAttachment("First"))
    oldDocument.addAttachment(MIDDLE, new TextNameDocumentAttachment("M."))
    oldDocument.addAttachment(LAST, new TextNameDocumentAttachment("Last"))

    val documentSerializer = new DocumentSerializer()
    // This should be a nice string
    val documentString = documentSerializer.save(oldDocument)

    val newDocument = documentSerializer.load(documentString)
    require(newDocument.getAttachment(FIRST) == oldDocument.getAttachment(FIRST))
    require(newDocument.getAttachment(MIDDLE) == oldDocument.getAttachment(MIDDLE))
    require(newDocument.getAttachment(LAST) == oldDocument.getAttachment(LAST))

    // This one must be avoided.
    /*require(newDocument == oldDocument)*/
  }

  {
    val oldDocument = new Document(Array.empty[Sentence])

    oldDocument.addAttachment(FIRST, new ObjectNameDocumentAttachment("First"))
    oldDocument.addAttachment(MIDDLE, new ObjectNameDocumentAttachment("M."))
    oldDocument.addAttachment(LAST, new ObjectNameDocumentAttachment("Last"))
    oldDocument.addAttachment(ALIAS, new SerializableDocumentAttachment("Alias"))

    val documentSerializer = new DocumentSerializer()
    // This should be a messy string.
    val documentString = documentSerializer.save(oldDocument)

    val newDocument = documentSerializer.load(documentString)
    require(newDocument.getAttachment(FIRST) == oldDocument.getAttachment(FIRST))
    require(newDocument.getAttachment(MIDDLE) == oldDocument.getAttachment(MIDDLE))
    require(newDocument.getAttachment(LAST) == oldDocument.getAttachment(LAST))
    require(newDocument.getAttachment(ALIAS).get.asInstanceOf[SerializableDocumentAttachment].name ==
        oldDocument.getAttachment(ALIAS).get.asInstanceOf[SerializableDocumentAttachment].name)

    // This one must be avoided.
    /*require(newDocument == oldDocument)*/
  }
}
