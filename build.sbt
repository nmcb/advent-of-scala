lazy val root =
  project
    .in(file("."))
    .settings( scalaVersion := "3.3.3"
             , name         := "aoc-2022"
             , version      := "0.1.0"
             , libraryDependencies ++= Seq(
                 "org.scalatest"  %% "scalatest"  % "3.2.18" % "test"
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
Compile / run / javaOptions += "-Xmx4G"
