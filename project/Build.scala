import sbt.{Build => SbtBuild, _}

object Build extends SbtBuild {
  lazy val root = Project("root", file(".")) dependsOn montague

  lazy val montague = RootProject(uri("git://github.com/Workday/upshot-montague.git#c04d750"))
}