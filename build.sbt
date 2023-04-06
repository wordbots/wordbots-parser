name := "wordbots-parser"
version := "0.19.0-alpha-SNAPSHOT" // wordbots-parser versions should correspond to the current wordbots-core release.

val http4sVersion = "0.15.3"
val circeVersion = "0.6.1"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "com.danielasfregola" %% "random-data-generator" % "2.3",
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
