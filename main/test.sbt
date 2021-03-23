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
