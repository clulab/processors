import sbt._
import Keys._

object Common {
  def name = "processors"
  def version = "4.0-SNAPSHOT"
  def classifier = "models"
  def organization = "edu.arizona.sista"
}

object ProcessorsBuild extends Build {
  val modelsTask = TaskKey[File]("models-task")
}
