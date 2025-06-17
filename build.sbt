lazy val aoc2015 = project.in(file("2015"))
lazy val aoc2016 = project.in(file("2016"))

lazy val root = (project in file("."))
  .aggregate(aoc2015, aoc2016)
  .settings(
    scalaVersion := "3.7.1",
    name         := "advent-of-scala",
    version      := "0.1.0",
    libraryDependencies ++= Seq(
      "org.scalatest"  %% "scalatest"  % "3.2.19" % "test"
    )
  )

scalacOptions ++= Seq(
  "-encoding", "utf8",        
  "-feature",                 
  "-language:implicitConversions",
  "-language:existentials",
  "-unchecked",
  "-Werror",
  "-deprecation"
)

Compile / run / fork := true
Compile / run / javaOptions ++= Seq("-Xmx8G", "-Xss1G", "-XX:+UseG1GC")

Test / fork := true
Test / javaOptions ++= Seq("-Xmx8G", "-Xss1G", "-XX:+UseG1GC")
