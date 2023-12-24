lazy val root =
  project
    .in(file("."))
    .settings( scalaVersion := "3.3.1"
             , name         := "aoc-2023"
             , version      := "0.1.0"
             , libraryDependencies ++= Seq(
                 "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
                 "org.scalatest"          %% "scalatest"                  % "3.2.16" % "test"
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
Compile / run / javaOptions ++= Seq("-Xmx8G", "-Xss1G")

Test / fork := true
Test / javaOptions ++= Seq("-Xmx8G", "-Xss1G")

/** Future Day 24 Part 2 ? */
// Compile / unmanagedJars += {
//   baseDirectory.value / "unmanaged" / s"scalaz3_3-4.8.14.jar"
// }