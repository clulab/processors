package org.clulab.utils

import org.clulab.processors.Processor

class ReloadableProcessor(constructor: () => Processor, impatient: Boolean = false) {
  // We would like a lazy var, but they don't exist.
  protected var processorOpt: Option[Processor] = if (impatient) construct() else None

  protected def construct(): Option[Processor] = {
    processorOpt = Option(constructor())
    processorOpt
  }

  def get: Processor = {
    if (processorOpt.isEmpty)
      construct()
    processorOpt.get
  }

  def reload(): Unit = construct()
}
