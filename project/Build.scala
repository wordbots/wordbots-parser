import sbt.{Build => SbtBuild, _}

object Build extends SbtBuild {
  val MONTAGUE_COMMIT_SHA = "8d88f7b3a02248d5658ba372a36f3a23fa93948b"

  lazy val root = Project("root", file(".")) dependsOn montague

  lazy val montague = RootProject(uri(s"git://github.com/Workday/upshot-montague.git#$MONTAGUE_COMMIT_SHA"))
}