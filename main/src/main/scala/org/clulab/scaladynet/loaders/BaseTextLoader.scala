package org.clulab.scaladynet.loaders

import org.clulab.scaladynet.parameters.ParameterCollection

class BaseTextLoader {

  def populateModel(modelParameters: ParameterCollection, key: String): Unit = ???
  def newTextModelLoader(): BaseTextLoader = ???
}

object BaseTextLoader {
  def newTextLoader(filename: String): BaseTextLoader = ???
}