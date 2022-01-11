package org.clulab.utils

import java.io.PrintStream
import scala.annotation.tailrec

case class Edit(
  name: String,
  sourceString: String, targetString: String,
  prevSourceIndex: Int, prevTargetIndex: Int,
  nextSourceIndex: Int, nextTargetIndex: Int
) {

  def getIndexes(prevIndex: Int, nextIndex: Int): String = {
    val length = nextIndex - prevIndex

    length match {
      case 0 => "-"
      case 1 => prevIndex.toString
      case _ => s"$prevIndex-${nextIndex - 1}"
    }
  }

  def getSourceIndexes: String = getIndexes(prevSourceIndex, nextSourceIndex)

  def getTargetIndexes: String = getIndexes(prevTargetIndex, nextTargetIndex)

  def getChars(string: String, prevIndex: Int, nextIndex: Int): String = {
    val substring = string.substring(prevIndex, nextIndex)

    if (substring.isEmpty) "_" else substring.flatMap(Escaper.escape)
  }

  def getSourceChars: String = getChars(sourceString, prevSourceIndex, nextSourceIndex)

  def getTargetChars: String = getChars(targetString, prevTargetIndex, nextTargetIndex)

  def print(printStream: PrintStream): Unit =
    Edit.printRow(printStream, name, getSourceIndexes, getSourceChars, getTargetIndexes, getTargetChars)
}

object Edit {

  def printRow(printStream: PrintStream, col1: String, col2: String, col3: String, col4: String, col5: String): Unit =
    printStream.println(s"$col1\t$col2\t$col3\t$col4\t$col5")

  def printHeader(printStream: PrintStream): Unit =
    printRow(
      printStream, "Type",
      "Source Index", "Source Value",
      "Target Index", "Target Value"
    )
}

abstract class Editor(val name: String, sourceString: String, targetString: String) {
  def calcCost(costs: Array[Array[Int]], nextSourceIndex: Int, nextTargetIndex: Int): Int
  def getEdit(nextSourceIndex: Int, nextTargetIndex: Int): Edit

  def newEdit(prevSourceIndex: Int, prevTargetIndex: Int, nextSourceIndex: Int, nextTargetIndex: Int): Edit =
    new Edit(name, sourceString, targetString, prevSourceIndex, prevTargetIndex, nextSourceIndex, nextTargetIndex)

  // Integer.MAX_VALUE means that the Editor is not applicable.
  def getCost(condition: Boolean, cost: Int): Int = if (condition) cost else Integer.MAX_VALUE

  def calcCost(disqualifier: Boolean, cost: => Int, oldCost: => Int): Int =
    if (disqualifier || cost == Integer.MAX_VALUE) Integer.MAX_VALUE
    else oldCost + cost
}

// The source character and target character match.
class Confirmer(sourceString: String, targetString: String) extends Editor("Confirmation", sourceString, targetString) {

  def getCost(sourceChar: Char, targetChar: Char): Int = getCost(sourceChar == targetChar, 0)

  def calcCost(costs: Array[Array[Int]], nextSourceIndex: Int, nextTargetIndex: Int): Int = {
    if (nextSourceIndex == 0 && nextTargetIndex == 0) 0 // This placeholder edit will be ignored.
    else {
      val prevSourceIndex = nextSourceIndex - 1
      val prevTargetIndex = nextTargetIndex - 1

      calcCost(
        prevSourceIndex < 0 || prevTargetIndex < 0,
        getCost(sourceString.charAt(prevSourceIndex), targetString.charAt(prevTargetIndex)),
        costs(prevSourceIndex)(prevTargetIndex)
      )
    }
  }

  def getEdit(nextSourceIndex: Int, nextTargetIndex: Int): Edit = {
    val prevSourceIndex = nextSourceIndex - 1
    val prevTargetIndex = nextTargetIndex - 1

    newEdit(prevSourceIndex, prevTargetIndex, nextSourceIndex, nextTargetIndex)
  }
}

class Inserter(sourceString: String, targetString: String) extends Editor("Insertion", sourceString, targetString) {

  def getCost(targetChar: Char): Int = getCost(true, 1)

  def calcCost(costs: Array[Array[Int]], nextSourceIndex: Int, nextTargetIndex: Int): Int = {
    val prevTargetIndex = nextTargetIndex - 1

    calcCost(
      prevTargetIndex < 0,
      getCost(targetString.charAt(prevTargetIndex)),
      costs(nextSourceIndex)(prevTargetIndex)
    )
  }

  def getEdit(nextSourceIndex: Int, nextTargetIndex: Int): Edit = {
    val prevTargetIndex = nextTargetIndex - 1

    newEdit(nextSourceIndex, prevTargetIndex, nextSourceIndex, nextTargetIndex)
  }
}

class Deleter(sourceString: String, targetString: String) extends Editor("Deletion", sourceString, targetString) {

  def getCost(sourceChar: Char): Int = getCost(true, 1)

  def calcCost(costs: Array[Array[Int]], nextSourceIndex: Int, nextTargetIndex: Int): Int = {
    val prevSourceIndex = nextSourceIndex - 1

    calcCost(
      prevSourceIndex < 0,
      getCost(sourceString.charAt(prevSourceIndex)),
      costs(prevSourceIndex)(nextTargetIndex)
    )
  }

  def getEdit(nextSourceIndex: Int, nextTargetIndex: Int): Edit = {
    val prevSourceIndex = nextSourceIndex - 1

    newEdit(prevSourceIndex, nextTargetIndex, nextSourceIndex, nextTargetIndex)
  }
}

// The source character has been misinterpreted as the target character.
class Substituter(sourceString: String, targetString: String) extends Editor("Substitution", sourceString, targetString) {

  def getCost(sourceChar: Char, targetChar: Char): Int = getCost(sourceChar != targetChar, 1)

  def calcCost(costs: Array[Array[Int]], nextSourceIndex: Int, nextTargetIndex: Int): Int = {
    val prevSourceIndex = nextSourceIndex - 1
    val prevTargetIndex = nextTargetIndex - 1

    calcCost(
      prevSourceIndex < 0 || prevTargetIndex < 0,
      getCost(sourceString.charAt(prevSourceIndex), targetString.charAt(prevTargetIndex)),
      costs(prevSourceIndex)(prevTargetIndex)
    )
  }

  def getEdit(nextSourceIndex: Int, nextTargetIndex: Int): Edit = {
    val prevSourceIndex = nextSourceIndex - 1
    val prevTargetIndex = nextTargetIndex - 1

    newEdit(prevSourceIndex, prevTargetIndex, nextSourceIndex, nextTargetIndex)
  }
}

class Transposer(sourceString: String, targetString: String) extends Editor("Transposition", sourceString, targetString) {

  def getCost(leftSourceChar: Char, leftTargetChar: Char, rightSourceChar: Char, rightTargetChar: Char): Int =
    getCost(leftSourceChar == rightTargetChar && rightSourceChar == leftTargetChar, 1)

  override def calcCost(costs: Array[Array[Int]], nextSourceIndex: Int, nextTargetIndex: Int): Int = {
    val rightSourceIndex = nextSourceIndex - 1
    val rightTargetIndex = nextTargetIndex - 1
    val leftSourceIndex = nextSourceIndex - 2
    val leftTargetIndex = nextTargetIndex - 2

    calcCost(
      leftSourceIndex < 0 || leftTargetIndex < 0,
      getCost(
        sourceString.charAt(leftSourceIndex), targetString.charAt(leftTargetIndex),
        sourceString.charAt(rightSourceIndex), targetString.charAt(rightTargetIndex)
      ),
      costs(leftSourceIndex)(leftTargetIndex)
    )
  }

  override def getEdit(nextSourceIndex: Int, nextTargetIndex: Int): Edit = {
    val leftSourceIndex = nextSourceIndex - 2
    val leftTargetIndex = nextTargetIndex - 2

    newEdit(leftSourceIndex, leftTargetIndex, nextSourceIndex, nextTargetIndex)
  }
}

// The source and target characters differ by case at a cost of 0, resulting in case-insensitive matches.
class Capitalizer(sourceString: String, targetString: String) extends Editor("Capitalization", sourceString, targetString) {

  def getCost(sourceChar: Char, targetChar: Char): Int =
    getCost(sourceChar != targetChar && sourceChar.toLower == targetChar.toLower, 0)

  def calcCost(costs: Array[Array[Int]], nextSourceIndex: Int, nextTargetIndex: Int): Int = {
    val prevSourceIndex = nextSourceIndex - 1
    val prevTargetIndex = nextTargetIndex - 1

    calcCost(
      prevSourceIndex < 0 || prevTargetIndex < 0,
      getCost(sourceString.charAt(prevSourceIndex), targetString.charAt(prevTargetIndex)),
      costs(prevSourceIndex)(prevTargetIndex)
    )
  }

  def getEdit(nextSourceIndex: Int, nextTargetIndex: Int): Edit = {
    val prevSourceIndex = nextSourceIndex - 1
    val prevTargetIndex = nextTargetIndex - 1

    newEdit(prevSourceIndex, prevTargetIndex, nextSourceIndex, nextTargetIndex)
  }
}

object Escaper {

  def escape(c: Char): String = c match {
    case '\r' => "\\r"
    case '\n' => "\\n"
    case '\t' => "\\t"
    case ' ' => "\\s"
    case c => Character.toString(c)
  }
}

class MED(sourceString: String, targetString: String, allowSubstitute: Boolean = true,
  allowTranspose: Boolean = false, allowCapitalize: Boolean = false) {
  // These are recorded now in the order of preference for tie breaking where we want
  // deletions to win when the target text is shorter than the source.
  protected val editors: Array[Editor] =
  Array(
    new Deleter(sourceString, targetString),
    new Confirmer(sourceString, targetString),
    new Inserter(sourceString, targetString)
  ) ++ {
    if (allowSubstitute) Array(new Substituter(sourceString, targetString))
    else Array.empty[Editor]
  } ++ {
    if (allowTranspose) Array(new Transposer(sourceString, targetString))
    else Array.empty[Editor]
  } ++ {
    if (allowCapitalize) Array(new Capitalizer(sourceString, targetString))
    else Array.empty[Editor]
  }

  protected val distances: Array[Array[Int]] = Array.ofDim[Int](sourceString.length + 1, targetString.length + 1)
  // This keeps track of the index of the editor used at each position.
  protected val editorIndexes: Array[Array[Int]] = Array.ofDim[Int](sourceString.length + 1, targetString.length + 1)
  protected val sourceRange = Range(0, sourceString.length + 1)
  protected val targetRange = Range(0, targetString.length + 1)
  protected val distance: Int = measure()
  protected lazy val edits: Array[Edit] = mkEdits()

  def getDistance: Int = distance

  protected def measure(): Int = {
    val costs = new Array[Int](editors.length)

    sourceRange.foreach { sourceIndex =>
      targetRange.foreach { targetIndex =>
        editors.zipWithIndex.foreach { case (editor, index) =>
          costs(index) = editor.calcCost(distances, sourceIndex, targetIndex)
        }

        val minCost = costs.min
        val editorIndex = costs.indexOf(minCost)

        distances(sourceIndex)(targetIndex) = minCost
        editorIndexes(sourceIndex)(targetIndex) = editorIndex
      }
    }
    distances(sourceString.length)(targetString.length)
  }

  def printDistancesOn(printStream: PrintStream): Unit = {
    printStream.print("\t")
    sourceRange.foreach { sourceIndex =>
      if (sourceIndex > 0)
        printStream.print(sourceString.charAt(sourceIndex - 1))
      printStream.print("\t")
    }
    printStream.println()

    targetRange.foreach { targetIndex =>
      sourceRange.foreach { sourceIndex =>
        if (sourceIndex == 0) {
          if (targetIndex > 0)
            printStream.print(targetString.charAt(targetIndex - 1))
          printStream.print("\t")
        }
        printStream.print(distances(sourceIndex)(targetIndex))
        printStream.print("\t")
      }
      printStream.println()
    }
  }

  protected def mkEdits(): Array[Edit] = {

    @tailrec
    def recMkEdits(edits: List[Edit], sourceIndex: Int, targetIndex: Int): List[Edit] = {
      if (sourceIndex == 0 && targetIndex == 0) edits
      else {
        val edit = editors(editorIndexes(sourceIndex)(targetIndex)).getEdit(sourceIndex, targetIndex)

        recMkEdits(edit :: edits, edit.prevSourceIndex, edit.prevTargetIndex)
      }
    }

    val edits = recMkEdits(Nil, sourceString.length, targetString.length)

    edits.toArray
  }

  def printEditsOn(printStream: PrintStream): Unit = {
    Edit.printHeader(printStream)
    edits.foreach(_.print(printStream))
  }

  def printSummaryOn(printStream: PrintStream): Unit = {
    val keys = editors
        .map(_.name)
    val counts = edits
        .groupBy(_.name)
        .mapValues(_.length)
    val headers = keys
        .mkString("\t")
    val values = keys
        .map { key => counts.getOrElse(key, 0) }
        .mkString("\t")

    printStream.println(headers)
    printStream.println(values)
  }
}

object MED {

  def apply(sourceString: String, targetString: String, allowSubstitute: Boolean = true,
    allowTranspose: Boolean = false, allowCapitalize: Boolean = false): MED =
    new MED(sourceString, targetString, allowSubstitute, allowTranspose, allowCapitalize)
}

object MEDApp extends App {
  val med = MED("Sunday?", "saturady", allowSubstitute = true, allowTranspose = true, allowCapitalize = true)

  println(med.getDistance)
  med.printDistancesOn(System.out)
  med.printEditsOn(System.out)
  med.printSummaryOn(System.out)
}
