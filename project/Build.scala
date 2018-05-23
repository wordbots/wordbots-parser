import sbt.{Build => SbtBuild, _}

object Build extends SbtBuild {
  lazy val root = Project("root", file(".")) dependsOn montague

  lazy val montague = RootProject(uri("git://github.com/Workday/upshot-montague.git#0c37d3da966f57050e238cc612d7ddde15aeb37f"))
}