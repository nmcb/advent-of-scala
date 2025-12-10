ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version      := "0.1.0"

ThisBuild / libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "3.2.19" % "test",

  // 2025
  "tools.aqua" % "z3-turnkey" % "4.14.1",

  // 2023
  ("org.scala-graph" %  "graph-core" % "2.0.3").cross(CrossVersion.for3Use2_13),
  ("org.scala-graph" %  "graph-dot"  % "2.0.0").cross(CrossVersion.for3Use2_13)
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

ThisBuild / Compile / run / fork         := true
ThisBuild / Compile / run / javaOptions ++= Seq("-Xmx4G", "-Xss1G", "-XX:+UseG1GC")

ThisBuild / Test / fork         := true
ThisBuild / Test / javaOptions ++= Seq("-Xmx4G", "-Xss1G", "-XX:+UseG1GC")
ThisBuild / Test / testOptions  += Tests.Argument("-oD")

Global / onChangedBuildSource   := ReloadOnSourceChanges


lazy val input   = project.in(file("input"))
lazy val nmcb    = project.in(file("nmcb"))
lazy val aoc2015 = project.in(file("2015")).dependsOn(input, nmcb)
lazy val aoc2016 = project.in(file("2016")).dependsOn(input, nmcb)
lazy val aoc2017 = project.in(file("2017")).dependsOn(input, nmcb)
lazy val aoc2018 = project.in(file("2018")).dependsOn(input, nmcb)
lazy val aoc2019 = project.in(file("2019")).dependsOn(input, nmcb)
lazy val aoc2020 = project.in(file("2020")).dependsOn(input, nmcb)
lazy val aoc2021 = project.in(file("2021")).dependsOn(input, nmcb)
lazy val aoc2022 = project.in(file("2022")).dependsOn(input, nmcb)
lazy val aoc2023 = project.in(file("2023")).dependsOn(input, nmcb)
lazy val aoc2024 = project.in(file("2024")).dependsOn(input, nmcb)
lazy val aoc2025 = project.in(file("2025")).dependsOn(input, nmcb)

lazy val aoc = (project in file("."))
  .aggregate(
    aoc2015,
    aoc2016,
    aoc2017,
    aoc2018,
    aoc2019,
    aoc2020,
    aoc2021,
    aoc2022,
    aoc2023,
    aoc2024,
    aoc2025
  )
