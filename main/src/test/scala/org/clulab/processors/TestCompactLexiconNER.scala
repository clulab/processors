package org.clulab.processors

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.nio.charset.StandardCharsets

import org.clulab.processors.clu.BioCluProcessor
import org.clulab.sequences.ColumnsToDocument
import org.clulab.sequences.LexiconNER
import org.clulab.struct.DebugBooleanHashTrie
import org.clulab.struct.ReadableHashTrie
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.io.Source

class TestCompactLexiconNER extends FlatSpec with Matchers {
  val proc = new BioCluProcessor()
  val start = System.currentTimeMillis
  val ner = proc.ner.get.asInstanceOf[LexiconNER]
//  val matchers = ner.matchers
  val nerLoadStop = System.currentTimeMillis
  val nerLoadTime = nerLoadStop - start

  val filename = "serialized.dat"

  def fileSave(hashTrie: ReadableHashTrie): Long = {
    val outputStream = new FileOutputStream(filename)
    val bufferedOutputStream = new BufferedOutputStream(outputStream)
    val objectOutputStream = new ObjectOutputStream(bufferedOutputStream)
    val start = System.currentTimeMillis

//    if (hashTrie.isInstanceOf[CompactIntHashTrie])
//      hashTrie.asInstanceOf[CompactIntHashTrie].save(objectOutputStream)
//    else
      objectOutputStream.writeObject(hashTrie)
    bufferedOutputStream.close

    val stop = System.currentTimeMillis
    stop - start
  }

  def bufferSave(hashTrie: ReadableHashTrie): (Array[Byte], Long) = {
    val outputStream = new ByteArrayOutputStream
    val objectOutputStream = new ObjectOutputStream(outputStream)
    val start = System.currentTimeMillis

//    if (hashTrie.isInstanceOf[CompactIntHashTrie])
//      hashTrie.asInstanceOf[CompactIntHashTrie].save(objectOutputStream)
//    else
      objectOutputStream.writeObject(hashTrie)
    objectOutputStream.close

    val bytes = outputStream.toByteArray

    {
      val fileWriter = new FileOutputStream(new File(filename + ".bytes"))
      fileWriter.write(bytes)
      fileWriter.close()
    }

    val stop = System.currentTimeMillis
    (bytes, stop - start)
  }

  def fileLoad(templateHashTrie: ReadableHashTrie): (ReadableHashTrie, Long) = {
    val inputStream = new FileInputStream(filename)
    val bufferedInputStream = new BufferedInputStream(inputStream)
    val objectInputStream = new ObjectInputStream(bufferedInputStream)
    val start = System.currentTimeMillis
    val hashTrie =
//        if (templateHashTrie.isInstanceOf[CompactIntHashTrie])
//          CompactIntHashTrie.load(objectInputStream).asInstanceOf[ReadableHashTrie]
//        else
          objectInputStream.readObject().asInstanceOf[ReadableHashTrie]

    bufferedInputStream.close
    val stop = System.currentTimeMillis
    (hashTrie, stop - start)
  }

  def bufferLoad(templateHashTrie: ReadableHashTrie, byteArray: Array[Byte]): (ReadableHashTrie, Long) = {
    val inputStream = new ByteArrayInputStream(byteArray)
    val objectInputStream = new ObjectInputStream(inputStream)
    val start = System.currentTimeMillis
    val hashTrie =
//        if (templateHashTrie.isInstanceOf[CompactIntHashTrie])
//          CompactIntHashTrie.load(objectInputStream).asInstanceOf[ReadableHashTrie]
//        else
           objectInputStream.readObject().asInstanceOf[ReadableHashTrie]

    objectInputStream.close
    val stop = System.currentTimeMillis
    (hashTrie, stop - start)
  }

  def processColumns(): Long = {
    val stream = getClass.getClassLoader.getResourceAsStream("org/clulab/processors/eng.testa")
    val doc: Document = ColumnsToDocument.readFromStream(stream,
        wordPos = 0, labelPos = 3,
        setLabels = ColumnsToDocument.setEntities,
        annotate = ColumnsToDocument.annotateLemmmaTags)
    val start = System.currentTimeMillis

    doc.sentences.foreach { sentence =>
      val namedEntities = ner.find(sentence)
//      println(namedEntities.mkString(" "))
    }

    val stop = System.currentTimeMillis
    stop - start
  }

  def processKnowledgebase(): Long = {
    val filename = "uniprot-proteins.tsv"
    val source = Source.fromFile(new File(filename), StandardCharsets.UTF_8.toString)
    val text = source.mkString.substring(0, 5000000) // 0000)
    source.close
    val doc: Document = proc.mkDocument(text)

    val start = System.currentTimeMillis
    doc.sentences.foreach { sentence =>
      val namedEntities = ner.find(sentence)
//        println(namedEntities.mkString(" "))
    }

    val stop = System.currentTimeMillis
    stop - start
  }

  println("Label\tAdded\tTokens\tStrings\tEntries\tTokenUniqueness\tEntryFullness\tSize\tBufferSaveTime\tBufferLoadTime\tFileSaveTime\tFileLoadTime")

  val matchers = Array[ReadableHashTrie]()

  matchers.foreach { matcher: ReadableHashTrie =>
    val isDebug = matcher.isInstanceOf[DebugBooleanHashTrie]
    val debugHashTrie = if (isDebug) Some(matcher.asInstanceOf[DebugBooleanHashTrie]) else None
    if (isDebug)
      debugHashTrie.get.uniqueStrings.clear()
    val label = matcher.label
    val addedCount =
        if (isDebug) debugHashTrie.get.addedCount
        else -1
    val addedTokensCount =
        if (isDebug) debugHashTrie.get.addedTokensCount
        else -1
    val uniqueness =
        if (isDebug) debugHashTrie.get.uniqueStringsSize.toFloat / addedTokensCount
        else 0
    val fullness =
      if (isDebug) debugHashTrie.get.entriesSize.toFloat / addedCount
      else 0
    val uniqueStringsSize =
      if (isDebug) debugHashTrie.get.uniqueStringsSize
      else -1
    val (byteArray, bufferSaveTime) = bufferSave(matcher)
    val (_, bufferLoadTime) = bufferLoad(matcher, byteArray)
    val fileSaveTime = fileSave(matcher)
    val (_, fileLoadTime) = fileLoad(matcher)
    print(label + "\t")
    print(addedCount + "\t")
    print(addedTokensCount + "\t")
    print(uniqueStringsSize + "\t")
    print(matcher.entriesSize + "\t")
    print(uniqueness + "\t")
    print(fullness + "\t")
    print(byteArray.length + "\t")
    print(bufferSaveTime + "\t")
    print(bufferLoadTime + "\t")
    print(fileSaveTime + "\t")
    println(fileLoadTime)
  }

  val startupStop = System.currentTimeMillis
  val startupTime = startupStop - start

  val processTime = processKnowledgebase()
//  val processTime = processColumns()
  println("ProcessTime\tNERLoadTime\tStartupTime")
  print(processTime + "\t")
  print(nerLoadTime + "\t")
  println(startupTime)
}
