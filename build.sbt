val dottyVersion = "0.20.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name    := "advent-of-code",
    version := "0.1.0",

    scalaVersion := dottyVersion
  )
