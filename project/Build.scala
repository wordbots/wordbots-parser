import sbt.{Build => SbtBuild, _}

object Build extends SbtBuild {
  val MONTAGUE_COMMIT_SHA = "b451836235cee5d900ec5f578a54ac702587858b"

  lazy val root = Project("root", file(".")) dependsOn montague

  lazy val montague = RootProject(uri(s"git://github.com/Workday/upshot-montague.git#$MONTAGUE_COMMIT_SHA"))
}