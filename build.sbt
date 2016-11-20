name := "wordbots-parser"

version := "0.0-SNAPSHOT"

val http4sVersion = "0.15.0a-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.log4s" %% "log4s" % "1.3.3"
)

enablePlugins(JavaAppPackaging)

mainClass in Compile := Some("wordbots.Server")
