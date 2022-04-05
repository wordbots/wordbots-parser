import sbt.{Build => SbtBuild, _}

object Build extends SbtBuild {
  val MONTAGUE_COMMIT_SHA = "6da2aae67f7a899a65f969a1106822a8a4ff650e"

  lazy val root = Project("root", file(".")) dependsOn montague

  lazy val montague = RootProject(uri(s"https://github.com/AlexNisnevich/upshot-montague.git#$MONTAGUE_COMMIT_SHA"))
}