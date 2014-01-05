package edu.arizona.sista.processor.struct

import org.slf4j.LoggerFactory
import Lexicon.logger
import java.io._
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
   * @param s
   * @return
   */
  def add(s:T):Int = {
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

  def exists(i:Int):Boolean = (i < index.size)

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

  def saveTo[F](fileName:String) {
    val os = new ObjectOutputStream(new FileOutputStream(fileName))
    os.writeObject(this)
    os.close()
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
    val is = new ObjectInputStream(new FileInputStream(fileName))
    val c = is.readObject().asInstanceOf[Lexicon[F]]
    is.close()
    c
  }
}

