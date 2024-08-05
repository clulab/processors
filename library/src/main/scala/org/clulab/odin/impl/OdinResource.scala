package org.clulab.odin.impl

import java.io.InputStream

import org.clulab.scala.WrappedArray._


trait OdinResource

// for distributional similarity comparisons
class EmbeddingsResource(/* is: InputStream */) extends OdinResource {
  def similarity(w1: String, w2: String): Double = {
    // embedding similarity no longer supported starting with v10 
    // TODO: if we decide to add it back to Odin, this is the place
    if(w1.equalsIgnoreCase(w2)) 1.0
    else 0.0
  }
}