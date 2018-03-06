package org.clulab.struct

import java.io.{Reader, Writer}
import java.text.DecimalFormat

import scala.collection.breakOut
import scala.collection.mutable
import scala.math.Ordering.{ Double => DoubleSortOrder }

import org.clulab.utils.Files

/**
 * Counts elements of type T
 * User: mihais
 * Date: 3/18/13
 */
class Counter[T] (
  private val counts:mutable.HashMap[T, MutableNumber[Double]],
  private val defaultReturnValue:Double = 0.0,
  private var total:Double = 0.0
) extends Serializable {

  def this(elements:Iterable[T]) = this(Counter.mkCounts(elements), 0.0, elements.size)

  def this(defaultReturnValue:Double) =
    this(new mutable.HashMap[T, MutableNumber[Double]], defaultReturnValue, total = 0.0)

  def this() = this(0.0)

  def getCount(key:T):Double = {
    counts.get(key).map(c => c.value).getOrElse(defaultReturnValue)
  }

  def proportion(key:T):Double = getCount(key) / total
  def getTotal: Double = total

  def incrementCount(key:T, inc:Double = 1.0):Double = {
    val v = Counter.incrementCount(counts, key, inc)
    total += inc // keep total up to date!
    v
  }
  def setCount(key:T, value:Double) {
    val prev = getCount(key)
    Counter.setCount(counts, key, value)
    total += value - prev // keep total up to date!
  }

  def decrementCount(key:T, inc:Double):Double = incrementCount(key, - inc)
  def decrementCount(key:T):Double = incrementCount(key, -1.0)

  def keySet = counts.keySet
  def size: Int = counts.size
  def contains(key:T):Boolean = keySet.contains(key)

  override def hashCode = counts.hashCode()
  override def equals(other:Any):Boolean = {
    other match {
      case that:Counter[T] => counts.size == that.counts.size && counts == that.counts
      case _ => false
    }
  }

  /** Equivalent to incrementCount with inc = 1 */
  def += (key:T) = incrementCount(key, 1)

  /** Equivalent to decrementCount */
  def -= (key:T) = decrementCount(key, 1)

  def += (toAdd:Counter[T]) {
    for(key <- toAdd.keySet)
      incrementCount(key, toAdd.getCount(key))
  }

  def -(toSub:Counter[T]):Counter[T] = {
    val out = new Counter[T]()
    // Step 1: Subtract all keys that are just in toSub
    for (key <- toSub.keySet) {
      if (getCount(key) == defaultReturnValue) out.setCount(key, -toSub.getCount(key))
    }

    // Step 2: Subtract all keys that are common between this and toSub
    for (key <- keySet) out.setCount(key, getCount(key) - toSub.getCount(key))

    out
  }

  def +(toAdd:Counter[T]):Counter[T] = {
    val out = new Counter[T]()
    // Step 1: Add all keys that are just in toAdd
    for (key <- toAdd.keySet) {
      if (getCount(key) == defaultReturnValue) out.setCount(key, toAdd.getCount(key))
    }

    // Step 2: Add all keys that are common between this and toSub, or unique to this
    for (key <- keySet) out.setCount(key, getCount(key) + toAdd.getCount(key))

    out
  }

  def *(toMult:Double):Counter[T] = {
    val out = new Counter[T]()
    for (key <- keySet) out.setCount(key, getCount(key) * toMult)
    out
  }

  def /(toDiv:Double):Counter[T] = {
    val out = new Counter[T]()
    for (key <- keySet) out.setCount(key, getCount(key) / toDiv)
    out
  }

  def dotProduct(toDot:Counter[T]):Double = {
    keySet.foldLeft(0.0) { (acc, key) => acc + (getCount(key) * toDot.getCount(key)) }
  }

  def sorted:List[(T, Double)] = sorted(true)

  /** Sorts counts in descending order, if argument is true. */
  def sorted(descending:Boolean):List[(T, Double)] = {
    val vs:List[(T,Double)] = keySet.map(k => Tuple2(k, getCount(k)))(breakOut)
    val sortOrder = if (descending) DoubleSortOrder.reverse else DoubleSortOrder
    vs.sortBy(_._2)(sortOrder)
  }

  override def toString:String = {
    val os = new StringBuilder
    os.append ("[")
    var first = true
    val keys = keySet
    for (key <- keys) {
      if(! first) os.append(", ")
      os.append (key + ":" + getCount(key).formatted("%3.3f"))
      first = false
    }
    os.append ("]")
    os.toString
  }

  // use counters in for comprehensions
  def map(f: ((T, Double)) => Double): Counter[T] = {
    val out = new Counter[T](defaultReturnValue)
    for (key <- keySet) out.setCount(key, f((key, getCount(key))))
    out
  }

  def mapValues(f: Double => Double): Counter[T] =
    map { case (_, x) => f(x) }

  def filter(p: ((T, Double))  => Boolean): Counter[T] = {
    val out = new Counter[T]()
    for (key <- keySet) {
      val count = getCount(key)
      if (p((key, count))) out.setCount(key, count)
    }
    out
  }

  def filterValues(p: Double => Boolean): Counter[T] =
    filter { case (_, x) => p(x)}

  def flatMap(f: ((T, Double)) => Counter[T]): Counter[T] = {
    // this one's gonna be inefficient
    val counters = keySet.map { key => f((key, getCount(key))) }
    counters.reduce(_ + _)
  }

  def toShortString: String = {
    val df = new DecimalFormat("0.####")
    val n = 10
    val base = sorted.take(n).map({ case (key, cnt) => s"$key -> ${df.format(cnt)}"}).mkString(", ")
    if (size > n)
      base + " ..."
    else
      base
  }

  def toSeq: Seq[(T, Double)] = for (key <- keySet.toSeq) yield (key, getCount(key))

  def values: Seq[Double] = toSeq.map(_._2)

  def toJSON: String = scala.util.parsing.json.JSONObject(toSeq.toMap.map(
  {
    case (k, v) => k.toString -> v
  })).toString()

  def zipWith(f: (Double,  Double) => Double)(other:Counter[T]): Counter[T] = {
    val out = new Counter[T](defaultReturnValue)
    for (key <- keySet.union(other.keySet)) {
      val x = f(getCount(key), other.getCount(key))
      out.setCount(key, x)
    }
    out
  }

  def *(other:Counter[T]): Counter[T] = zipWith(_*_)(other)

  def /(other:Counter[T]):  Counter[T] = zipWith(_/_)(other)

  def +(x: Double) = mapValues(_+x)

  def -(x: Double) = mapValues(_-x)

  def l1Norm = values.sum

  def l2Norm = Math.sqrt(values.map(x => x*x).sum)

  def topKeys(n: Int) = sorted.take(n).map(_._1)

  def argMax: (T, Double) = keySet.map(key => (key, getCount(key))).maxBy(_._2)

  def argMin: (T, Double) = keySet.map(key => (key, getCount(key))).minBy(_._2)

  def saveTo(w:Writer) {
    val writer = Files.toPrintWriter(w)
    writer.println(s"$defaultReturnValue ${counts.size}")
    if(counts.nonEmpty) {
      val first = counts.keys.head
      first match {
        // TODO: kinda hacky, but don't know how to recover from type erasure in loadFrom()...
        case i: Int => writer.println("I")
        case d: Double => writer.println("D")
        case s: String => writer.println("S")
        case _ => throw new RuntimeException("ERROR: unknown type in lexicon!")
      }
    } else {
      writer.println("S") // this does not matter
    }
    for(k <- counts.keySet) {
      writer.println(s"${counts(k).value} $k")
    }
  }
}

object Counter {
  private def incrementCount[T](map:mutable.HashMap[T, MutableNumber[Double]], key:T, inc:Double): Double = {
    val incVal = inc + map.get(key).map(_.value).getOrElse(0.0)
    setCount(map, key, incVal)
    incVal
  }

  private def setCount[T](map:mutable.HashMap[T, MutableNumber[Double]], key:T, value:Double) {
    map.get(key) match {
      case Some(c) =>
        c.value = value
      case None =>
        map.put(key, new MutableNumber[Double](value))
    }
  }

  private def mkCounts[T](elements:Iterable[T]):mutable.HashMap[T, MutableNumber[Double]] = {
    val c = new mutable.HashMap[T, MutableNumber[Double]]
    for (element <- elements) incrementCount(c, element, 1.0)
    c
  }

  /**
   * warning: this shares memory between the old and new counters, don't use if you mutate state
   */
  def withDefault[T](defaultValue: Double)(counter: Counter[T]) = new Counter[T](counter.counts, defaultValue)

  def binarize[S](xs: Counter[S]) = {
    withDefault(0)(xs.mapValues(_ => 1))
  }

  def loadFrom[T](r:Reader):Counter[T] = {
    val reader = Files.toBufferedReader(r)
    var hline = reader.readLine()

    // sometimes, the input comes with an empty head line
    // (this happens when the counter is serialized in the same file with a LibLinear model; see SRL)
    if(hline.trim.isEmpty) hline = reader.readLine()

    // println("COUNTER HLINE: " + hline)
    val bits = hline.split("\\s+")

    val defaultReturnValue = bits(0).toDouble
    val c = new Counter[T](defaultReturnValue)
    val size = bits(1).toInt
    assert(size >= 0)
    val ftype = reader.readLine() // type of features in this counter
    for(i <- 0 until size) {
      val line = reader.readLine()
      val space = line.indexOf(' ')
      assert(space > 0)
      val v = line.substring(0, space).toDouble
      val f = line.substring(space + 1)
      ftype match {
        case "S" => c.setCount(f.asInstanceOf[T], v)
        case "I" => c.setCount(f.toInt.asInstanceOf[T], v)
        case "D" => c.setCount(f.toDouble.asInstanceOf[T], v)
        case _ => throw new RuntimeException("ERROR: unknown type in Counter!")
      }
    }
    c
  }
}
