package edu.arizona.sista.struct

import collection.mutable
import scala.collection.mutable.ListBuffer
import java.text.DecimalFormat

/**
 * Counts elements of type T
 * User: mihais
 * Date: 3/18/13
 */
class Counter[T](
                  private val counts:mutable.HashMap[T, MutableNumber[Double]],
                  private val defaultReturnValue:Double = 0.0) extends Serializable {

  def this(elements:Iterable[T]) = this(Counter.mkCounts(elements), 0.0)

  def this(defaultReturnValue:Double = 0.0) =
    this(new mutable.HashMap[T, MutableNumber[Double]], defaultReturnValue)

  def getCount(key:T):Double = {
    counts.get(key) match {
      case Some(c) => c.value
      case None => defaultReturnValue
    }
  }

  def incrementCount(key:T, inc:Double = 1.0):Double = Counter.incrementCount(counts, key, inc)
  def decrementCount(key:T, inc:Double):Double = incrementCount(key, - inc)
  def decrementCount(key:T):Double = incrementCount(key, -1.0)
  def setCount(key:T, value:Double) { Counter.setCount(counts, key, value) }
  def keySet = counts.keySet
  def size = counts.size
  def contains(key:T) = keySet.contains(key)

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

    // Step 2: Add all keys that are common between this and toSub
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
    var dotproduct:Double = 0.0f
    for (key <- keySet) dotproduct += getCount(key) * toDot.getCount(key)
    dotproduct
  }

  def sorted:List[(T, Double)] = {
    val vs = new ListBuffer[(T, Double)]
    for(k <- keySet) vs += new Tuple2(k, getCount(k))
    vs.toList.sortBy(0 - _._2)
  }
  override def toString:String = {
    val os = new StringBuilder
    os.append ("[")
    for (key <- keySet) {
      os.append (key + ":" + getCount(key).formatted("%3.3f") + ", ")
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
    val counters = for {
      key <- keySet
      count = getCount(key)
    } yield f((key, count))
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

  def argMax : (T, Double) = {
    var maxValue = Double.MinValue
    var maxKey = keySet.head
    for(k <- keySet) {
      val v = getCount(k)
      if(v > maxValue) {
        maxKey = k
        maxValue = v
      }
    }
    (maxKey, maxValue)
  }

  def argMin : (T, Double) = {
    var minValue = Double.MaxValue
    var minKey = keySet.head
    for(k <- keySet) {
      val v = getCount(k)
      if(v < minValue) {
        minKey = k
        minValue = v
      }
    }
    (minKey, minValue)
  }
}

object Counter {
  private def incrementCount[T](map:mutable.HashMap[T, MutableNumber[Double]], key:T, inc:Double): Double = {
    map.get(key) match {
      case Some(c) => {
        c.value += inc
        c.value
      }
      case None => {
        map.put(key, new MutableNumber[Double](inc))
        inc
      }
    }
  }

  private def setCount[T](map:mutable.HashMap[T, MutableNumber[Double]], key:T, value:Double) {
    map.get(key) match {
      case Some(c) => {
        c.value = value
      }
      case None => {
        map.put(key, new MutableNumber[Double](value))
      }
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
}
