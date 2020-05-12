package org.clulab.dynet

/**
 * Implement multi-task learning (MTL) for sequence modeling
 * @author Mihai
 */
class SeqMTL(val taskManagerOpt: Option[TaskManager], modelOpt: Option[Array[Layers]]) {
  // One Layers for task; model(0) contains the Layers shared between all tasks (if any)
  protected val model: Array[Layers] = modelOpt.getOrElse(initialize())

  // Use this carefully. That is, only when taskManagerOpt.isDefined
  def taskManager: TaskManager = {
    assert(taskManagerOpt.isDefined)
    taskManagerOpt.get
  }

  protected def initialize(): Array[Layers] = {
    // this should only be called during training, when the task manager should be defined
    require(taskManagerOpt.isDefined)

    null // TODO
  }
}
