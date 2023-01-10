package org.clulab.utils

import scala.collection.immutable.{ArraySeq => ImmutableArraySeq}
import scala.collection.mutable.{ArraySeq => MutableArraySeq}

class TestArray extends Test {

  def processSeq(seq: Seq[Int]): Seq[Int] = seq
  def processIndexedSeq(indexedSeq: IndexedSeq[Int]): IndexedSeq[Int] = indexedSeq

  // Notes
  // readableSeq, immutable.ArraySeq
  // readableIndexedSeq, immutable.ArraySeq
  // writeableSeq, mutable.ArraySeq()
  // writeableIndexedSeq, mutable.ArraySeq()

  // new ArrayOps(xs).toIndexedSeq
  // immutable.ArraySeq.unsafeWrapArray(Array.copyOf(xs, xs.length))
  // This means that a copy is produced.  We should instead say
  // ArraySeq.unsafeWrapArray(array) so that no copy is produced.

  val standard = "work as expected without import"
  val custom = "work as expected with import"
  val array = Array(1, 2, 3, 4, 5)

  behavior of "Array.toSeq"
  it should standard in {
    // @`inline` final def ArrayOps.toSeq: immutable.Seq[A] = toIndexedSeq
    // def ArrayOps.toIndexedSeq: immutable.IndexedSeq[A] =
    //   immutable.ArraySeq.unsafeWrapArray(Array.copyOf(xs, xs.length))
    val seq = array.toSeq
    seq shouldNot be theSameInstanceAs array
    seq shouldBe an [ImmutableArraySeq[_]]
    seq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray shouldNot be theSameInstanceAs array
  }
  it should custom in {
    // Call is same as above.
    import org.clulab.scala.WrappedArray._
    val seq = array.toSeq // This goes through the custom, wrapped conversion in Scala 3.1!
    seq shouldNot be theSameInstanceAs array
    seq shouldBe an [ImmutableArraySeq[_]]
    seq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of "Array.toIndexedSeq"
  it should standard in {
    // def ArrayOps.toIndexedSeq: immutable.IndexedSeq[A] =
    //   immutable.ArraySeq.unsafeWrapArray(Array.copyOf(xs, xs.length))
    val indexedSeq = array.toIndexedSeq
    indexedSeq shouldNot be theSameInstanceAs array
    indexedSeq shouldBe an [ImmutableArraySeq[_]]
    indexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray shouldNot be theSameInstanceAs array
  }
  it should custom in {
    // Call is same as above.
    import org.clulab.scala.WrappedArray._
    val indexedSeq = array.toIndexedSeq // This goes through the custom, wrapped conversion in Scala 3.1!
    indexedSeq shouldNot be theSameInstanceAs array
    indexedSeq shouldBe an [ImmutableArraySeq[_]]
    indexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of "MutableArraySeq.from"
  it should standard in {
    val mutableArraySeqArray = MutableArraySeq.from(array).array
    mutableArraySeqArray shouldNot be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val mutableArraySeqArray = MutableArraySeq.from(array).array
    mutableArraySeqArray shouldNot be theSameInstanceAs array
  }

  behavior of "ImmutableArraySeq.unsafeWrapArray"
  it should standard in {
    val immutableArraySeqArray = ImmutableArraySeq.unsafeWrapArray(array).unsafeArray
    immutableArraySeqArray should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val immutableArraySeqArray = ImmutableArraySeq.unsafeWrapArray(array).unsafeArray
    immutableArraySeqArray should be theSameInstanceAs array
  }

  behavior of "processSeq(array)"
  it should standard in {
    @annotation.nowarn("cat=deprecation")
    // implicit def LowPriorityImplicits2.copyArrayToImmutableIndexedSeq[T](xs: Array[T]): IndexedSeq[T] =
    //    if (xs eq null) null
    //    else new ArrayOps(xs).toIndexedSeq
    val autoCopySeq = processSeq(array)
    autoCopySeq shouldNot be theSameInstanceAs array
    autoCopySeq shouldBe an [ImmutableArraySeq[_]]
    autoCopySeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray shouldNot be theSameInstanceAs array
  }
  it should custom in {
    // implicit def WrappedArray.copyArrayToImmutableIndexedSeq[T](xs: Array[T]): IndexedSeq[T]
    import org.clulab.scala.WrappedArray._
    val autoCopySeq = processSeq(array)
    autoCopySeq shouldNot be theSameInstanceAs array
    autoCopySeq shouldBe an [ImmutableArraySeq[_]]
    autoCopySeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of "processSeq(ImmutableArraySeq.unsafeWrapArray)"
  it should standard in {
    val autoCopySeq = processSeq(ImmutableArraySeq.unsafeWrapArray(array))
    autoCopySeq shouldNot be theSameInstanceAs array
    autoCopySeq shouldBe an [ImmutableArraySeq[_]]
    autoCopySeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val autoCopySeq = processSeq(ImmutableArraySeq.unsafeWrapArray(array))
    autoCopySeq shouldNot be theSameInstanceAs array
    autoCopySeq shouldBe an [ImmutableArraySeq[_]]
    autoCopySeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of "processIndexedSeq(array)"
  it should standard in {
    @annotation.nowarn("cat=deprecation")
    val autoCopyIndexedSeq = processIndexedSeq(array)
    autoCopyIndexedSeq shouldNot be theSameInstanceAs array
    autoCopyIndexedSeq shouldBe an [ImmutableArraySeq[_]]
    autoCopyIndexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray shouldNot be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val autoCopyIndexedSeq = processIndexedSeq(array)
    autoCopyIndexedSeq shouldNot be theSameInstanceAs array
    autoCopyIndexedSeq shouldBe an [ImmutableArraySeq[_]]
    autoCopyIndexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of "processIndexedSeq(ImmutableArraySeq.unsafeWrapArray)"
  it should standard in {
    val autoCopyIndexedSeq = processIndexedSeq(ImmutableArraySeq.unsafeWrapArray(array))
    autoCopyIndexedSeq shouldNot be theSameInstanceAs array
    autoCopyIndexedSeq shouldBe an [ImmutableArraySeq[_]]
    autoCopyIndexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val autoCopyIndexedSeq = processIndexedSeq(ImmutableArraySeq.unsafeWrapArray(array))
    autoCopyIndexedSeq shouldNot be theSameInstanceAs array
    autoCopyIndexedSeq shouldBe an [ImmutableArraySeq[_]]
    autoCopyIndexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of ":Seq[]"
  it should standard in {
    @annotation.nowarn("cat=deprecation")
    val seq: Seq[Int] = array
    seq.toArray shouldNot be theSameInstanceAs array
    seq shouldBe an [ImmutableArraySeq[_]]
    seq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray shouldNot be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val seq: Seq[Int] = array
    seq.toArray shouldNot be theSameInstanceAs array
    seq shouldBe an [ImmutableArraySeq[_]]
    seq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of ":IndexedSeq[]"
  it should standard in {
    @annotation.nowarn("cat=deprecation")
    val indexedSeq: IndexedSeq[Int] = array
    indexedSeq.toArray shouldNot be theSameInstanceAs array
    indexedSeq shouldBe an [ImmutableArraySeq[_]]
    indexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray shouldNot be theSameInstanceAs array
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    val indexedSeq: IndexedSeq[Int] = array
    indexedSeq.toArray shouldNot be theSameInstanceAs array
    indexedSeq shouldBe an [ImmutableArraySeq[_]]
    indexedSeq.asInstanceOf[ImmutableArraySeq[Int]].unsafeArray should be theSameInstanceAs array
  }

  behavior of "foreach"
  it should standard in {
    def foreach[T](array: Array[T]): Unit = {
      array.foreach { each => () }
    }

    foreach(array)
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    def foreach[T](array: Array[T]): Unit = {
      array.foreach { each => () }
    }

    foreach(array)
  }

  def isArray(obj: AnyRef): Boolean = {
    val name = obj.getClass.getName

    name.startsWith("[")
  }

  behavior of "zip"
  it should standard in {
    def zip[T](array: Array[T]): Array[(T, T)] = {
      array.zip(array)
    }

    val zippedArray = zip(array)
    zippedArray shouldNot be theSameInstanceAs array
    isArray(zippedArray) should be (true)
  }
  it should custom in {
//    import org.clulab.scala.WrappedArray._
//    def zip[T](array: Array[T]): Array[(T, T)] = {
//      val tmp = array.zip(array)

//      tmp.toArray
//    }

//    val zippedArray = zip(array)
//    zippedArray shouldNot be theSameInstanceAs array
//    isArray(zippedArray) should be (true)
  }

  behavior of "map"
  it should standard in {
    def map(array: Array[Int]): Array[Int] = {
      array.map(_ + 1)
    }

    val mappedArray = map(array)
    mappedArray shouldNot be theSameInstanceAs array
    val name = array.getClass.getName
    mappedArray.getClass.getName should be (array.getClass.getName)
  }
  it should custom in {
    import org.clulab.scala.WrappedArray._
    def map(array: Array[Int]): Array[Int] = {
      val tmp = array.map(_ + 1) // For this it is converted into an ArraySeq

      tmp.toArray
    }

    val mappedArray = map(array)
    mappedArray shouldNot be theSameInstanceAs array
    mappedArray.getClass.getName should be (array.getClass.getName)
  }
}
