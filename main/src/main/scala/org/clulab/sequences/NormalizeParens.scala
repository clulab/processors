package org.clulab.sequences

import java.io.{FileWriter, PrintWriter}

import scala.io.Source

/**
  * Transforms -LRB-, -LCB-, etc. tokens back into "(", "{", etc.
  * This is necessary because the POS WSJ dataset uses the -LRB- conventions to replace words in the dataset,
  *   whereas all the others datasets we use (NER, syntax) do not.
  * Note that we continue to keep the *POS tags* as -LRB-, -LCB-, etc., because these are standard Penn Treebank tags.
  *   We just replace the words.
  */
object NormalizeParens {
  def main(args: Array[String]): Unit = {
    val isConll = args(1) == "conll"
    val pw = new PrintWriter(new FileWriter(args(0) + ".parens"))
    for(line <- Source.fromFile(args(0)).getLines()){
      if(line.trim.isEmpty) {
        pw.println(line)
      } else {
        val tokens = line.split("\\s+")
        if(isConll) {
          assert(tokens.length > 3)
          tokens(1) = norm(tokens(1))
          tokens(2) = norm(tokens(2))
          pw.println(tokens.mkString("\t"))
        } else {
          assert(tokens.length == 2)
          pw.println(norm(tokens(0)) + "\t" + tokens(1))
        }
      }
    }
    pw.close()
  }

  def norm(s:String): String = {
    var ns = s.replaceAll("-LRB-", "(")
    ns = ns.replaceAll("-RRB-", ")")
    ns = ns.replaceAll("-LCB-", "{")
    ns = ns.replaceAll("-RCB-", "}")
    ns = ns.replaceAll("-lrb-", "(")
    ns = ns.replaceAll("-rrb-", ")")
    ns = ns.replaceAll("-lcb-", "{")
    ns = ns.replaceAll("-rcb-", "}")
    ns = ns.replaceAll("-LSB-", "[")
    ns = ns.replaceAll("-RSB-", "]")
    ns = ns.replaceAll("-lsb-", "[")
    ns = ns.replaceAll("-rsb-", "]")
    ns
  }
}
