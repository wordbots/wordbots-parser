name := "wordbots-parser"
version := "0.20.4-beta-SNAPSHOT" // wordbots-parser versions should correspond to the current wordbots-core release.

val http4sVersion = "0.15.3"
val circeVersion = "0.6.1"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  // "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime,  // to enable slf4j debug logging for the server
  "com.danielasfregola" %% "random-data-generator" % "2.3",
  "com.outr" %% "hasher" % "1.2.1",
  "io.circe" %% "circe-generic" % circeVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.log4s" %% "log4s" % "1.3.3",
  "org.mozilla" % "rhino" % "1.7R4",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

enablePlugins(JavaAppPackaging)
enablePlugins(BuildInfoPlugin)

mainClass in Compile := Some("wordbots.Server")

initialCommands in console := "import wordbots._; import wordbots.Semantics._; import com.workday.montague.semantics._;"

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "wordbots"
