package org.clulab.embeddings

import java.io.File

import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sinker
import org.clulab.utils.Sourcer

// Expect this to use lots of memory.
object CullVectors extends App {
  // This should be something like glove.840B.300d.txt.
  val inVectorFile = new File(args(0))
  // This should be something like gigawordDocFreq.sorted.freq.txt.
  val inFrequencyFile = new File(args(1))
  // Make something up.
  val outputFile = new File(args(2))
  // This is either the upper limit or lower limit, depending on how keep is set below.
  val limit = args(3).toInt

  class Counter(protected var value: Int, step: Int) {

    def inc(increment: Int = 1): Int = {
      value += (increment * step)
      value
    }

    def get: Int = value
  }

  object Counter {

    def apply(start: Int = 0, step: Int = 1): Counter = new Counter(start, step)
  }

  def l2(values: Array[Float]): Float =
    math.sqrt(values.foldLeft(0f) { case (sum, value) => sum + value * value }).toFloat

  // Caution: This normalization happens in place.
  def normalize(values: Array[Float]): Array[Float] = {
    val length = l2(values)

    require(length > 0)
    values.indices.foreach { index => values(index) /= length }
    values
  }

  def addWeightedVec(dest: Array[Float], freq: Int, src: Array[Float]): Unit = {
    dest.indices.foreach { index =>
      dest(index) += src(index) * freq
    }
  }

  def before(string: String, index: Int, all: Boolean): String = {
    if (index < 0)
      if (all) string
      else ""
    else string.substring(0, index)
  }

  def beforeFirst(string: String, char: Char, all: Boolean = true): String =
    before(string, string.indexOf(char), all)

  def after(string: String, index: Int, all: Boolean): String = {
    if (index < 0)
      if (all) string
      else ""
    else string.substring(index + 1)
  }

  def afterFirst(string: String, char: Char, all: Boolean = true): String =
    after(string, string.indexOf(char), all)

  def keepByIndex(index: Int, freq: Int): Boolean = 0 <= index && index < limit

  def keepByFreq(index: Int, freq: Int): Boolean = 0 <= index && limit <= freq

  val keep = keepByFreq _

  // The words in gigaword have been lowercased and include these substitutions.
  val substitutions = Seq(
    ("-lrb-", "("), ("-rrb-", ")"), // round
    ("-lsb-", "["), ("-rsb-", "]"), // square
    ("-lcb-", "{"), ("-rcb-", "}")  // curvy
  )
  // This is Map[word, (index, freq)].  The index is used for separating frequent from infrequent words.
  // The freq is used to eventually weight the vectors for each word when words are combined into single vectors.
  val wordFrequencies: Map[String, (Int, Int)] = Sourcer.sourceFromFile(inFrequencyFile).autoClose { source =>
    val counter = Counter(-1)
    val frequentWords = source
        .getLines()
        .map { line =>
          val Array(rawWord, freq) = line.split('\t')
          val cookedWord = substitutions.foldLeft(rawWord) { case (word, (remove, insert)) =>
            word.replace(remove, insert)
          }

          cookedWord -> (counter.inc(), freq.toInt)
        }.toMap

    frequentWords
  }
  val (columns, badFloats, goodLines) = Sourcer.sourceFromFile(inVectorFile).autoClose { source =>
    val bufferedLines = source.getLines().buffered
    val line = bufferedLines.head
    val columns = {
      val Array(_, columns) = line.split(' ')

      columns.toInt
    }
    val badFloats = new Array[Float](columns)
    val goodLines = bufferedLines.drop(1).filter { line =>
      val word = beforeFirst(line, ' ')
          .toLowerCase
      val (index, freq) = wordFrequencies.getOrElse(word, (-1, 0))
      // 0 <= index implies wordFrequencies.contains(word).
      val good = keep(index, freq)

      if (!good) { // Need to add to bad line
        val floats = afterFirst(line, ' ').split(' ').map(_.toFloat)

        addWeightedVec(badFloats, freq, floats)
      }
      good
    }.toVector
    (columns, badFloats, goodLines)
  }
  // This skips over some options that are available in other versions of this program.
  val betterLines = goodLines

  val count = betterLines.size + 1
  // Although the vectors are not normalized to begin with, we'll normalize it now.
  // Word2Vec normalizes all incoming vectors.  Doing it twice will not hurt.
  val normalizedFloats = normalize(badFloats)
  val badStrings = normalizedFloats.map(_.toString)
  val badLine = " " + badStrings.mkString(" ")

  // The \n is to force LF as eol even on Windows.
  Sinker.printWriterFromFile(outputFile, append = false).autoClose { printWriter =>
    printWriter.print(count.toString + " " + columns)
    printWriter.print("\n")
    printWriter.print(badLine)
    printWriter.print("\n")
    betterLines.foreach { line =>
      printWriter.print(line)
      printWriter.print("\n")
    }
  }
}
