import sbt.{Build => SbtBuild, _}

object Build extends SbtBuild {
  val MONTAGUE_COMMIT_SHA = "bc4b7233cf4e7e85e93c2f3f35028a7bbec1e6aa"

  lazy val root = Project("root", file(".")) dependsOn montague

  lazy val montague = RootProject(uri(s"git://github.com/Workday/upshot-montague.git#$MONTAGUE_COMMIT_SHA"))
}