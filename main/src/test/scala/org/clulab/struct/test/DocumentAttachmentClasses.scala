package org.clulab.struct.test

import java.io.ObjectOutputStream

import org.clulab.processors.DocumentAttachment
import org.clulab.processors.DocumentAttachmentBuilderFromJson
import org.clulab.processors.DocumentAttachmentBuilderFromText
import org.json4s.JString
import org.json4s.JValue
import org.json4s.jackson.prettyJson

// These classes are all outside the testing class because the test framework
// interferes with serialization.  It seems to extend classes with exception
// mechanisms which themselves cannot be serialized, causing failures.
// Heed this, as it was a hard lesson learned.
// See maybe https://github.com/sbt/sbt/issues/2824.

// Furthermore, these classes are in a completely separate file because
// IntelliJ cannot find the test suite if they are included in that file.

case class CaseClass(name: String)

// DocumentAttachment provides serialization itself now, so this doesn't need Serializable.
// This is an example of the minimum functionality needed for a DocumentAttachment.
// The attachment simply stores a name, but it could be anything else.
@SerialVersionUID(100L)
class NameDocumentAttachment(val name: String) extends DocumentAttachment

// These methods are only there for testing.  Production classes do not need them.
// This one doesn't provide nice serialization for DocumentSerializer.
@SerialVersionUID(100L)
class ObjectNameDocumentAttachment(name: String) extends NameDocumentAttachment(name) {
  var serialize: Boolean = false

  private def writeObject(out: ObjectOutputStream): Unit = {
    serialize = true
    // If this isn't done, the stream will EOF prematurely on deserialization.
    out.defaultWriteObject()
  }

  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[ObjectNameDocumentAttachment]

    this.name == that.name && this.serialize == that.serialize
  }
}

@SerialVersionUID(100L)
class NameDocumentAttachmentBuilderFromText extends DocumentAttachmentBuilderFromText {

  def mkDocumentAttachment(text: String): TextNameDocumentAttachment = {
    new TextNameDocumentAttachment(text)
  }
}

@SerialVersionUID(100L)
class NameDocumentAttachmentBuilderFromJson extends DocumentAttachmentBuilderFromJson {

  def mkDocumentAttachment(json: JValue): TextNameDocumentAttachment = {
    json match {
      case JString(text) => new TextNameDocumentAttachment(text)
      case _ =>
        val text = prettyJson(json)
        throw new RuntimeException(s"ERROR: While deserializing TextNameDocumentAttachment expected JString but found this: $text")
    }
  }
}

@SerialVersionUID(100L)
class TextNameDocumentAttachment(name: String) extends NameDocumentAttachment(name) {
  var serialized: Boolean = false

  override def documentAttachmentBuilderFromTextClassName: String = classOf[NameDocumentAttachmentBuilderFromText].getName
  override def documentAttachmentBuilderFromJsonClassName: String = classOf[NameDocumentAttachmentBuilderFromJson].getName

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

  override def toJsonSerializer: JValue = {
    new JString(name)
  }
}

trait MethodAttachment

@SerialVersionUID(100L)
class NameMethodAttachment(val name: String) extends MethodAttachment {
}
