package org.clulab.utils

import scala.collection.mutable.{HashMap => MutableHashMap}

class Timer(val description: String) {
  var elapsedTime: Long = 0L
  var startTimeOpt: Option[Long] = None

  def time[R](block: => R): R = {
    val startTime = System.nanoTime()
    val result: R = block // call-by-name
    val stopTime = System.nanoTime()

    elapsedTime += stopTime - startTime
    result
  }

  def start(): Unit = {
    val startTime = System.nanoTime()

    startTimeOpt = Some(startTime)
  }

  def stop(): Unit = {
    if (startTimeOpt.isDefined) {
      val stopTime = System.nanoTime()

      elapsedTime += stopTime - startTimeOpt.get
    }
  }

  override def toString: String = {
    s"Time\t$description\t${elapsedToString()}\t$elapsedTime"
  }

  def elapsedToString(): String = {
    import Timer._

    val days  = (elapsedTime)              /  dayDivisor
    val hours = (elapsedTime % dayDivisor) /   hrDivisor
    val mins  = (elapsedTime %  hrDivisor) /  minDivisor
    val secs  = (elapsedTime % minDivisor) /  secDivisor
    val msecs = (elapsedTime % secDivisor) / msecDivisor

    f"$days:$hours%02d:$mins%02d:$secs%02d.$msecs%03d"
  }
}

object Timer {
  val nsecDivisor: Long = 1
  val msecDivisor: Long = nsecDivisor * 1000000
  val  secDivisor: Long = msecDivisor * 1000
  val  minDivisor: Long =  secDivisor * 60
  val   hrDivisor: Long =  minDivisor * 60
  val  dayDivisor: Long =   hrDivisor * 24
  val daysDivisor: Long =  dayDivisor * 1
}

object Timers {
  val timers: MutableHashMap[String, Timer] = MutableHashMap.empty

  def getOrNew(description: String): Timer = synchronized {
    timers.getOrElseUpdate(description, new Timer(description))
  }

  def summarize(): Unit = {
    timers.keys.toVector.sorted.foreach { description =>
      println(timers(description))
    }
  }

  def clear(): Unit = {
    timers.clear()
  }
}
