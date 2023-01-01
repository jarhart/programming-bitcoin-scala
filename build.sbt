val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Programming Bitcoin",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.14",
      "org.scalatest" %% "scalatest" % "3.2.14" % "test"
    )
  )
