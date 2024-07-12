package org.clulab.processors.apps

import org.clulab.processors.{Document, Processor}
import org.clulab.processors.clu.{BalaurProcessor, VeiledDocument, VeiledText}
import org.clulab.serialization.DocumentSerializer

import java.io.PrintWriter
import scala.util.Using

/** Demonstrate how either parts of the text or Document can be veiled.
  */
object VeilApp extends App {

  /** Treat this text as if the letters "(Hahn-Powell, 2012)" did not exist
    *  for the purpose of mkDocument, but do include them in the text.
    */
  def veilText(processsor: Processor): Unit = {
    val text = "To be loved by unicorns is the greatest gift of all (Hahn-Powell, 2012)."
    val veiledLetters = Seq(Range.inclusive(text.indexOf('('), text.indexOf(')')))
    val veiledText = new VeiledText(text, veiledLetters)
    val document: Document = veiledText.mkDocument(processor)

    Using.resource(new PrintWriter("veiledLetters.out")) { printWriter =>
      val documentSerializer = new DocumentSerializer()

      documentSerializer.save(document, printWriter)
    }
  }

  /** Treat this text as if the words "( Hahn-Powell , 2012 )" did not exist
    * for the purpose of annotate, but do include them in the document.
    */
  def veilDocument(processor: Processor): Unit = {
    val text = "To be loved by unicorns is the greatest gift of all (Hahn-Powell, 2012)."
    val document: Document = processor.mkDocument(text)
    val veiledWords = Seq((0, Range.inclusive(document.sentences(0).raw.indexOf("("), document.sentences(0).raw.indexOf(")"))))
    val veiledDocument = new VeiledDocument(document, veiledWords)
    val annotatedDocument: Document = veiledDocument.annotate(processor)

    Using.resource(new PrintWriter("veiledWords.out")) { printWriter =>
      val documentSerializer = new DocumentSerializer()

      documentSerializer.save(annotatedDocument, printWriter)
    }
  }

  val processor = new BalaurProcessor()

  veilText(processor)
  veilDocument(processor)
}
