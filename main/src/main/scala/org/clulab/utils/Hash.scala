package org.clulab.utils

import scala.util.hashing.MurmurHash3

object Hash {
  val symmetricSeed = 0xb592f7ae

  def apply(string: String): Int = stringHash(string)

  def apply(seed: Int, data: Int*): Int = {
    finalizeHash(data.foldLeft(seed)(mix), data.length)
  }

  // TODO: This count should probably not be used.  The caller is probably messed up.
  def withLast(count: Int)(seed: Int, data: Int*): Int = withLastCount(count)(seed, data)

  def withLast(seed: Int, data: Int*): Int = withLastCount(data.length)(seed, data)

  def withLastCount(count: Int)(seed: Int, data: Seq[Int]): Int = {
    val iterator = data.iterator

    def loop(value: Int, remaining: Int): Int = {
      val result = remaining match {
        case 0 => finalizeHash(value, count)
        case 1 => loop(mixLast(value, iterator.next()), 0)
        case _ => loop(mix(value, iterator.next()), remaining - 1)
      }

      result
    }

    loop(seed, data.length)
  }

  def ordered(xs: TraversableOnce[Any]): Int = orderedHash(xs)

  def unordered(xs: TraversableOnce[Any]): Int = unorderedHash(xs)

  def stringHash(x: String): Int = MurmurHash3.stringHash(x)

  def orderedHash(xs: TraversableOnce[Any]): Int = MurmurHash3.orderedHash(xs)

  def unorderedHash(xs: TraversableOnce[Any]): Int = MurmurHash3.unorderedHash(xs)

  def finalizeHash(hash: Int, length: Int): Int = MurmurHash3.finalizeHash(hash, length)

  def mix(hash: Int, data: Int): Int = MurmurHash3.mix(hash, data)

  def mixLast(hash: Int, data: Int): Int = MurmurHash3.mixLast(hash, data)
}
