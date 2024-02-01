val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Programming Bitcoin",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.bouncycastle" % "bcprov-jdk18on" % "1.77",
      "org.playframework" %% "play-json" % "3.0.2",
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scalactic" %% "scalactic" % "3.2.17",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test,
    ),

    scalacOptions +=  "-deprecation",

    console / initialCommands := """
                                |import ecc._
                                |import helper._
                                |import tx._
                                |import play.api.libs.json._
                                |import java.util.HexFormat
                                |val hexFormat = HexFormat.of()
                                |import hexFormat.{formatHex, parseHex}
                                |""".stripMargin
  )
