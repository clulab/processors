package edu.arizona.sista.utils

/**
 * Generic lexicon: maps objects of type T to Ints, both ways
 * User: mihais
 * Date: 3/1/13
 */
class Lexicon[T] {
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

  def stats() {
    println("Stored objects: " + index.size)
    println(" Saved objects: " + savedMemory + " (" + (100.0 * savedMemory / (savedMemory + lexicon.size)) + "%)")
  }
}
