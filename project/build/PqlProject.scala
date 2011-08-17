import sbt._

class PqlProject(info: ProjectInfo) extends DefaultProject(info) {
  val specs2 = "org.specs2" %% "specs2" % "1.5" % "test"

  def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
  override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)
}
