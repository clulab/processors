package org.clulab.struct.test

import java.io.ObjectOutputStream

import org.clulab.processors.DocumentAttachment
import org.clulab.processors.DocumentAttachmentBuilderFromText

// These classes are all outside the testing class because the test framework
// interferes with serialization.  It seems to extend classes with exception
// mechanisms which themselves cannot be serialized, causing failures.
// Heed this, as it was a hard lesson learned.

// Furthermore, these classes are in a completely separate file because
// IntelliJ cannot find the test suite if they are included in that file.

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
class NameDocumentBuilder extends DocumentAttachmentBuilderFromText {

  def mkDocumentAttachment(text: String): TextNameDocumentAttachment = {
    new TextNameDocumentAttachment(text)
  }
}

class TextNameDocumentAttachment(name: String) extends SerializableDocumentAttachment(name) {
  var serialized: Boolean = false

  override val documentAttachmentBuilderFromTextClassName: String = classOf[NameDocumentBuilder].getName

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


