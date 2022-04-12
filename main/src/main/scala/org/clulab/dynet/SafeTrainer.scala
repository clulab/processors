package org.clulab.dynet

import edu.cmu.dynet.{ParameterCollection, Trainer}
import org.slf4j.{Logger, LoggerFactory}

class SafeTrainer(val trainer: Trainer, val clipThreshold:Float) {
  trainer.clippingEnabled_=(true)
  trainer.clipThreshold_=(clipThreshold)

  /**
   * Updates the model, catching vanishing/exploding gradients and trying to recover
   * @param parameters Model
   */
  def update(parameters: ParameterCollection): Unit = {
    try {
      trainer.update()
    } catch {
      case exception: RuntimeException if exception.getMessage.startsWith("Magnitude of gradient is bad") =>
        // aim to reset the gradient and continue training
        parameters.resetGradient()
        SafeTrainer.logger.info(s"Caught an invalid gradient exception: ${exception.getMessage}. Reset gradient L2 norm to: ${parameters.gradientL2Norm()}")
    }
  }
}

object SafeTrainer {
  val logger: Logger = LoggerFactory.getLogger(classOf[SafeTrainer])

  val CLIP_THRESHOLD = 5.0f

  def apply(trainer: Trainer, clipThreshold: Float = CLIP_THRESHOLD): SafeTrainer = {
    new SafeTrainer(trainer, clipThreshold)
  }
}