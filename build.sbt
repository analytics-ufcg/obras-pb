name := """obras-pb"""

version := "1.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.11"

libraryDependencies += jdbc
libraryDependencies += cache
libraryDependencies += ws
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0" % Test

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.41"
libraryDependencies += "ai.x" %% "play-json-extensions" % "0.10.0"

libraryDependencies ++= Seq(
    jdbc,
    "com.typesafe.play" %% "anorm" % "2.5.1"
)


