name := "understanding-computation"

organization := "net.godcode"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.0",
  "org.specs2" %% "specs2" % "1.13" % "test"
)

initialCommands := "import understandingcomputation._"
