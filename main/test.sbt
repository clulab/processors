import sbt.Tests.Group
import sbt.Tests.SubProcess

{
  def group(tests: Seq[TestDefinition]) = {
    def newRunPolicy = SubProcess(ForkOptions())

    val singletonTests = tests.filter(_.name.endsWith(".TestConstEmbeddingsGlove"))
    val otherTests = tests.filter(!singletonTests.contains(_))

    val singletonGroup = new Group("singleton", singletonTests, newRunPolicy)
    val otherGroup = new Group("others", otherTests, newRunPolicy)

    Seq(singletonGroup, otherGroup)
  }

  Test / testGrouping := group((Test / definedTests).value)
}

Test / unmanagedSourceDirectories ++= {
  val sharedSourceDir = (ThisBuild / baseDirectory).value / "main/src/test"
  println(s"sharedSourceDir: $sharedSourceDir")
  val additionalDirs = CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11) | (2, 12)) => Seq(sharedSourceDir / "scala-2.11_2.12")
    case _ => Seq.empty
  }
  println(s"additionalDirs: $additionalDirs")
  additionalDirs
}
