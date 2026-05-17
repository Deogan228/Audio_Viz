ThisBuild / scalaVersion := "3.3.4"
ThisBuild / version := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "audio-visualizer",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Wunused:all"
    )
  )
