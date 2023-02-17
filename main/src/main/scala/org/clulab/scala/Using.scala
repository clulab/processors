package org.clulab.scala

import org.clulab.fatdynet.utils.CloseableModelSaver

import scala.io.Source
import scala.util.Using.Releasable

object Using {
  val Using = scala.util.Using

  implicit object SourceReleaser extends Releasable[Source] {
    override def release(resource: Source): Unit = resource.close
  }

  implicit object CloseableModelSaverReleaser extends Releasable[CloseableModelSaver] {
    override def release(resource: CloseableModelSaver): Unit = resource.close()
  }
}
