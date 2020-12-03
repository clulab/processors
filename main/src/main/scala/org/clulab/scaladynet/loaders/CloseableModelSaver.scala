package org.clulab.scaladynet.loaders

import org.clulab.scaladynet.parameters.ParameterCollection

class CloseableModelSaver(filename: String) {
  def autoClose(f: CloseableModelSaver => Unit): Unit = ???
  def addModel(pc: ParameterCollection, key: String): Unit = ???
}
