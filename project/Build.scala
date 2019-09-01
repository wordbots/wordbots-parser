import sbt.{Build => SbtBuild, _}

object Build extends SbtBuild {
  lazy val root = Project("root", file(".")) dependsOn montague

  lazy val montague = RootProject(uri("git://github.com/Workday/upshot-montague.git#583d3d1a8b5e61230aa3c6111fd8ef5365c6514a"))
}