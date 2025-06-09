lazy val root =
  project
    .in(file("."))
    .settings( scalaVersion := "3.7.1"
             , name         := "aoc-2023"
             , version      := "0.1.0"
             , libraryDependencies ++= Seq(
             ("org.scala-graph" %  "graph-core" % "2.0.3").cross(CrossVersion.for3Use2_13),
             ("org.scala-graph" %  "graph-dot"  % "2.0.0").cross(CrossVersion.for3Use2_13),

             "org.scalatest"    %% "scalatest"  % "3.2.19" % "test"))

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
Compile / run / javaOptions ++= Seq("-Xmx8G", "-Xss1M", "-XX:+UseG1GC")

Test / fork := true
Test / javaOptions ++= Seq("-Xmx8G", "-Xss1M", "-XX:+UseG1GC")
