package org.clulab.dynet

/**
 * Diffs 2 DyNet models
 * Necessary to
 */
object ModelDiff extends App {
  val modelFileName1 = args(0)
  val modelFileName2 = args(1)

  val lines1 = io.Source.fromFile(modelFileName1).getLines()
  val lines2 = io.Source.fromFile(modelFileName2).getLines()

  var lineCount = 0
  while(lines1.hasNext) {
    println(s"Diffing line $lineCount")
    val line1 = lines1.next()
    val line2 = lines2.next()
    diff(line1, line2)
    lineCount += 1
  }

  def diff(l1: String, l2: String): Unit = {
    if(l1.length != l2.length) {
      println(s"L1 has length ${l1.length} while L2 has length ${l2.length}")
    }

    val minLen = math.min(l1.length, l2.length)
    for(i <- 0 until minLen) {
      if(l1.charAt(i) != l2.charAt(i)) {
        println("Found different characters at position $i:")
        println(l1.substring(math.max(0, i - 10), i) + s"[[${l1.charAt(i)}]]" + l1.substring(i + 1, math.min(l1.length, i + 11)))
        println(l2.substring(math.max(0, i - 10), i) + s"[[${l2.charAt(i)}]]" + l2.substring(i + 1, math.min(l2.length, i + 11)))
        return
      }
    }
  }
}
