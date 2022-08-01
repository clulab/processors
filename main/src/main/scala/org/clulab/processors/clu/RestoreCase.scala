package org.clulab.processors.clu

import org.clulab.sequences.ColumnReader
import java.io.PrintWriter
import org.clulab.dynet.Utils
import org.clulab.processors.Document
import org.clulab.sequences.Row

/** Restores the case for tokens stored in the first column in a CoNLL-formatted file */
object RestoreCase extends App {
  val inputFileName = args(0)
  val outputFileName = inputFileName + ".restored"
  val pw = new PrintWriter(outputFileName)

  Utils.initializeDyNet()
  val proc = new CluProcessor

  val sentences = ColumnReader.readColumns(inputFileName)
  val words = sentences.map(_.map(_.tokens(0)).toSeq)
  val doc = proc.mkDocumentFromTokens(words)
  proc.restoreCase(doc)
  saveOutput(pw, doc, sentences)
  pw.close()

  private def saveOutput(pw: PrintWriter, doc: Document, sentences: Array[Array[Row]]): Unit = {
    assert(doc.sentences.size == sentences.length)

    for(i <- doc.sentences.indices) {
      val rs = doc.sentences(i)
      val s = sentences(i)
      assert(s.size == rs.size)

      for(j <- rs.indices) {
        // print the restored word
        pw.print(rs.words(j))
        // print the other columns from the original CoNLL file
        for(k <- 1 until s(j).length) {
          pw.print("\t" + s(j).get(k))
        }
        pw.println()
      }
      pw.println()
    }
  }

}
