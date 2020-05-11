package org.clulab.dynet

import edu.cmu.dynet.ExpressionVector

trait ConstEmbeddings {
  /**
   * The generated Expressions must be const, i.e., they are not updated during the backprop phase
   * This is necessary so we can share the same (large) object between different tasks
   */
  def mkEmbeddings(words: Iterable[String]): ExpressionVector
}
