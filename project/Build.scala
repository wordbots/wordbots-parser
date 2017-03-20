import sbt._

object MyBuild extends Build {
  lazy val root = Project("root", file(".")) dependsOn montague

  lazy val montague = RootProject(uri("git://github.com/Workday/upshot-montague.git#0686714"))
}