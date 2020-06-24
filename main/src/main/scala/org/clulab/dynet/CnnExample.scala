package org.clulab.dynet

import edu.cmu.dynet.{Dim, Expression, ParameterCollection, UnsignedVector}

//
// See also:
// https://dynet.readthedocs.io/en/latest/tutorials_notebooks/API.html
// https://github.com/neubig/nn4nlp-code/blob/970d91a51664b3d91a9822b61cd76abea20218cb/05-cnn/cnn-class.py#L45
//
object CnnExample extends App {
  Utils.initializeDyNet()
  val pc = new ParameterCollection()

  val embSize = 3
  val embeddings = pc.addLookupParameters(5, Dim(1, 1, embSize)) // embbedings of size 3 for 5 words

  val winSize = 2 // size of the CNN window
  val filterSize = 4 // size of the CNN filter
  val cnnParams = pc.addParameters(Dim(1, winSize, embSize, filterSize))
  val cnnBias = pc.addParameters(Dim(filterSize))

  val w1 = Expression.lookup(embeddings, 1)
  val w2 = Expression.lookup(embeddings, 2)
  val w3 = Expression.lookup(embeddings, 3)
  val w4 = Expression.lookup(embeddings, 4)
  val cnnIn = Expression.concatenateCols(w1, w2, w3, w4)
  println(cnnIn.dim())
  println(cnnIn.value().toVector().mkString(" "))

  val stride = new UnsignedVector(Seq(1L, 1L))

  val cnnOut = Expression.conv2d(cnnIn, Expression.parameter(cnnParams), Expression.parameter(cnnBias), stride)

  println(cnnOut.dim())
  println(cnnOut.value().toVector().mkString(" "))
}
