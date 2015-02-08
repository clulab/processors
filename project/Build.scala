import sbt._
import Keys._

object ProcessorsBuild extends Build {
  val modelsTask = TaskKey[File]("models-task")
}
