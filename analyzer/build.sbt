ThisBuild / scalaVersion := "3.3.4"
ThisBuild / version := "0.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "audio-analyzer",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"           % "2.1.14",
      "dev.zio" %% "zio-streams"   % "2.1.14",
      "dev.zio" %% "zio-test"      % "2.1.14" % Test,
      "dev.zio" %% "zio-test-sbt"  % "2.1.14" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    )
  )
