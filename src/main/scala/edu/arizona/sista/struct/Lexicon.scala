package edu.arizona.sista.struct

import java.io._

import edu.arizona.sista.struct.Lexicon.logger
import edu.arizona.sista.utils.Files
import org.slf4j.LoggerFactory

import scala.Serializable

/**
 * Generic lexicon: maps objects of type T to Ints, both ways
 * User: mihais
 * Date: 3/1/13
 */
class Lexicon[T] extends Serializable {
  private var lexicon = new collection.mutable.HashMap[T, Int]
  private var index = new collection.mutable.ArrayBuffer[T]()

  /** Counts the number of objects that were saved by not storing strings repeatedly */
  private var savedMemory = 0

  /**
   * Adds a string to the lexicon without adding it twice if it already exists
   */
  def add(s:T):Int = synchronized {
    if (lexicon.contains(s)) {
      savedMemory += 1
      lexicon.get(s).get
    } else {
      val i = index.size
      lexicon += s -> i
      index += s
      i
    }
  }

  def exists(i:Int):Boolean = i < index.size

  /**
   * Fetches the string with the given index from the lexicon
   * This deliberately does NOT check for bounds for fast access.
   * Use exists() if you want to make sure that your index exists.
   * @param i Index of the string in the lexicon
   * @return The string corresponding to the index
   */
  def get(i:Int):T = index(i)

  def get(w:T):Option[Int] = lexicon.get(w)

  def size = lexicon.size

  override def toString:String = {
    val os = new StringBuilder
    for(w <- lexicon.keySet) {
      os.append(w + " -> " + lexicon.get(w) + "\n")
    }
    os.toString()
  }

  def keySet = lexicon.keySet

  def stats() {
    logger.info("Stored objects: " + index.size)
    logger.info(" Saved objects: " + savedMemory + " (" + (100.0 * savedMemory / (savedMemory + lexicon.size)) + "%)")
  }

  def saveTo(fileName:String) {
    val w = new BufferedWriter(new FileWriter(fileName))
    saveTo(w)
    w.close()
  }

  def saveTo(w:Writer) {
    val p = Files.toPrintWriter(w)
    p.println(index.size)
    if(index.size > 0) {
      val first = index(0)
      first match {
        // TODO: kinda hacky, but don't know how to recover from type erasure in loadFrom()...
        case i: Int => p.println("I")
        case d: Double => p.println("D")
        case s: String => p.println("S")
        case _ => throw new RuntimeException("ERROR: unknown type in lexicon!")
      }
    } else {
      p.println("S") // this does not matter
    }
    for(i <- 0 until index.size) {
      p.println(s"$i ${index(i)}")
    }
  }
}

object Lexicon {
  val logger = LoggerFactory.getLogger(classOf[Lexicon[String]])

  /** Copy constructor for Lexicon */
  def apply[T](other:Lexicon[T]):Lexicon[T] = {
    val lex = new Lexicon[T]
    for(w <- other.lexicon.keySet)
      lex.lexicon += w -> other.lexicon.get(w).get
    for(i <- 0 until other.index.size)
      lex.index += other.index(i)
    lex
  }

  /** Loads a lexicon saved by Lexicon.saveTo */
  def loadFrom[F](fileName:String):Lexicon[F] = {
    val is = new BufferedReader(new FileReader(fileName))
    val lex = loadFrom[F](is)
    is.close()
    lex
  }

  def loadFrom[F](r:Reader):Lexicon[F] = {
    val reader = Files.toBufferedReader(r)
    val lex = new Lexicon[F]
    var line = reader.readLine()
    val size = line.trim.toInt
    val ftype = reader.readLine() // type of features in lexicon
    //println(s"SIZE = $size FTYPE = $ftype")
    assert(size >= 0)
    for(i <- 0 until size) {
      line = reader.readLine().trim
      //println("LEX LINE = " + line)
      val space = line.indexOf(' ')
      assert(space > 0)
      val index = line.substring(0, space).toInt
      assert(index == i)
      val f = line.substring(space + 1)
      ftype match {
        case "S" => lex.add(f.asInstanceOf[F])
        case "I" => lex.add(f.toInt.asInstanceOf[F])
        case "D" => lex.add(f.toDouble.asInstanceOf[F])
        case _ => throw new RuntimeException("ERROR: unknown type in lexicon!")
      }
    }
    lex
  }
}

