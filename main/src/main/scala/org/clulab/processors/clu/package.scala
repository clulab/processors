package org.clulab.processors

package object clu {

  // This comes from scala-transformers, so we can't make a class from it here.
  type PredictionScore = (String, Float)
  // This goes into the graph-generating methods.
  type HeadLabel = (Int, String)
}
