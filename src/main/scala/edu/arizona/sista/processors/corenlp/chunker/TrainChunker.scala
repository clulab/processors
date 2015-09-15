package edu.arizona.sista.processors.corenlp.chunker

import java.io.File
import scala.collection.mutable
import edu.stanford.nlp.ling.{ CoreLabel, CoreAnnotations }

object TrainChunker extends App {

  val trainSentences = readData(args(0))
  val testSentences = readData(args(1))

  // train
  val chunker = CRFChunker.train(trainSentences)
  chunker.save("chunker.crf")

  // test
  test(chunker, testSentences)

  def test(chunker: CRFChunker, sentences: Array[Array[CoreLabel]]): Unit = {
    val tp = mutable.Map[String, Double]() withDefaultValue 0.0
    val fp = mutable.Map[String, Double]() withDefaultValue 0.0
    val fn = mutable.Map[String, Double]() withDefaultValue 0.0
    for (s <- sentences) {
      val trueChunks = s.map(_.getString(classOf[CoreAnnotations.AnswerAnnotation]))
      val predChunks = chunker.classify(s)
      for ((trueLabel, predLabel) <- trueChunks zip predChunks) {
        if (trueLabel == predLabel) {
          tp(trueLabel) += 1
        } else {
          fp(predLabel) += 1
          fn(trueLabel) += 1
        }
      }
    }
    printReport(tp, fp, fn)
  }

  def printReport(
      tp: mutable.Map[String, Double],
      fp: mutable.Map[String, Double],
      fn: mutable.Map[String, Double]
  ): Unit = {
    val labels = tp.keySet ++ fp.keySet ++ fn.keySet
    println("label\tprecision\trecall\tf1")
    for (label <- labels) {
      val precision = tp(label) / (tp(label) + fp(label))
      val recall = tp(label) / (tp(label) + fn(label))
      val f1 = 2 * ((precision * recall) / (precision + recall))
      println(f"$label\t$precision%.2f\t$recall%.2f\t$f1%.2f")
    }
    println("=" * 50)
    val ttp = tp.values.sum
    val tfp = fp.values.sum
    val tfn = fn.values.sum
    val totalPrecision = ttp / (ttp + tfp)
    val totalRecall = ttp / (ttp + tfn)
    val totalF1 = 2 * ((totalPrecision * totalRecall) / (totalPrecision + totalRecall))
    println(f"TOTAL\t$totalPrecision%.2f\t$totalRecall%.2f\t$totalF1%.2f")
  }

  def readData(path: String): Array[Array[CoreLabel]] = {
    val file = new File(path)
    val source = io.Source.fromFile(file)
    val text = source.mkString
    source.close()
    // sentences are separated by an empty line
    val sentences = text.split("\n\n")
    sentences.map { sent =>
      sent.split("\n").map { tok =>
        val Array(word, tag, chunk) = tok.split(" ")
        val label = new CoreLabel
        label.setWord(word)
        label.setTag(tag)
        label.set(classOf[CoreAnnotations.AnswerAnnotation], chunk)
        label
      }
    }
  }

}
