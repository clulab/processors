package org.clulab.fatdynet.utils

class Header(val line: String, val lineNo: Int) {
  val Array(objectType, objectName, dimension, _, _) = line.split(" ")
  // Skip leading { and trailing }
  val dims = dimension.substring(1, dimension.length - 1).split(",").map(_.toInt)
}
