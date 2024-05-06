ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "final"
  )

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.2.0",
  "com.softwaremill.sttp.client3" %% "circe" % "3.2.0",
  "io.circe" %% "circe-generic" % "0.14.1",
  "io.circe" %% "circe-parser" % "0.14.1",
  "org.jfree" % "jfreechart" % "1.5.3"
)

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.2"

libraryDependencies += "org.jfree" % "jfreechart" % "1.5.3"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.8"