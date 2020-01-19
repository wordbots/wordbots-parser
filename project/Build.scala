import sbt.{Build => SbtBuild, _}

object Build extends SbtBuild {
  val MONTAGUE_COMMIT_SHA = "1020d9dd8ada539a0bb7b4189a5a23bbb6dc9718"

  lazy val root = Project("root", file(".")) dependsOn montague

  lazy val montague = RootProject(uri(s"git://github.com/Workday/upshot-montague.git#$MONTAGUE_COMMIT_SHA"))
}