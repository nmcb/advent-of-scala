ThisBuild / scalaVersion := "3.7.1"
ThisBuild / version      := "0.1.0"

ThisBuild / libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "3.2.19" % "test"
)

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-feature",                 
  "-language:implicitConversions",
  "-language:existentials",
  "-unchecked",
  "-Werror",
  "-deprecation"
)

ThisBuild / Compile / run / fork := true
ThisBuild / Compile / run / javaOptions ++= Seq("-Xmx8G", "-Xss1G", "-XX:+UseG1GC")

ThisBuild / Test / fork := true
ThisBuild / Test / javaOptions ++= Seq("-Xmx8G", "-Xss1G", "-XX:+UseG1GC")


lazy val aoc2015 = project.in(file("2015"))
lazy val aoc2016 = project.in(file("2016"))
lazy val aoc2017 = project.in(file("2017"))
lazy val root    = (project in file(".")).aggregate(
  aoc2015,
  aoc2016,
  aoc2017
)