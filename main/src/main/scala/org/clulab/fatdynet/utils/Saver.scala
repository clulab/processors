package org.clulab.fatdynet.utils

import edu.cmu.dynet._

object Saver {

  class ClosableModelSaver(filename: String) extends ModelSaver(filename) {
    def close(): Unit = done
  }
}
