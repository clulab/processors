package org.clulab.processors.hexatagging

import org.clulab.sequences.ColumnReader
import scala.collection.mutable.Stack
import org.clulab.sequences.Row
import org.clulab.utils.FileUtils
import java.io.PrintWriter

object DepsToTags {
  def generateHexaTags(sent: Array[Row]): (Array[String], Array[String]) = {
    // creates the dependency tree data structure from the paper
    val depTree = DependencyTree.toTree(sent)
    println(depTree)
    // creates the BHT from the dependency tree
    @annotation.nowarn("cat=deprecation")
    val stack = new Stack[BHT]
    depTree.toBHT(stack, "root")
    val bht = stack.pop()
    println(bht)
    // creates the hexa tags from the BHT
    val (termTags, nonTermTags) = assignHexaTags(bht, sent.length)
    
    println("termTags = " + termTags.mkString(", "))
    println("nonTermTags = " + nonTermTags.mkString(", "))

    // we should have fully populated arrays of tags
    for(i <- termTags.indices) assert(termTags(i) != null)
    for(i <- nonTermTags.indices) assert(nonTermTags(i) != null)

    (termTags, nonTermTags)
  }

  def assignHexaTags(bht: BHT, length: Int): (Array[String], Array[String]) = {
    val termTags = new Array[String](length)
    val nonTermTags = new Array[String](length)
    nonTermTags(length - 1) = "eos"

    bht.setHexaTags(termTags, nonTermTags)

    (termTags, nonTermTags)
  }

  def printTags(writer: PrintWriter, sent: Array[Row], tags: Array[String]): Unit = {
    assert(tags.length == sent.length)
    for(i <- sent.indices) {
      for(j <- 0 until 1) {
        writer.print(sent(i).get(j))
        writer.print("\t")
      }
      writer.println(tags(i))
    }
    writer.println()
  }

  def main(args: Array[String]): Unit = {
    //val depsFileName = "dynet/en/deps/universal/wsj/train.labels"
    //val depsFileName = "dynet/en/deps/universal/wsj/dev.labels"
    //val depsFileName = "dynet/en/deps/universal/wsj/test.labels"
    val depsFileName = "dynet/en/deps/universal/combined/wsjtrain-wsjdev-geniatrain-geniadev.labels"
    val sents = ColumnReader.readColumns(depsFileName)
    println(s"Read ${sents.length} sentences.")

    val termTagsWriter = FileUtils.printWriterFromFile(depsFileName + ".hexaterms")
    val nonTermTagsWriter = FileUtils.printWriterFromFile(depsFileName + ".hexanonterms")

    var oneSents = 0
    for(sent <- sents) {
      if(sent.length > 1) {
        print("SENTENCE:")
        for(i <- sent.indices) print(s" ${sent(i).get(0)}")
        println()
        val (termTags, nonTermTags) = generateHexaTags(sent)
        printTags(termTagsWriter, sent, termTags)
        printTags(nonTermTagsWriter, sent, nonTermTags)
      } else {
        oneSents += 1
      }
      //System.exit(1)
    }

    println(s"Found $oneSents sentences with a single word.")
    termTagsWriter.close()
    nonTermTagsWriter.close()
  }
}

