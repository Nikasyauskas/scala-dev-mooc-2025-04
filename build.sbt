name := "scala-dev-mooc-2025-04"

lazy val root = (project in file("."))
  .aggregate(scala213Module, scala3Module)

lazy val scala213Module = (project in file("scala213Module"))
  .settings(
    name := "Scala 2.13 Module",
    scalaVersion := "2.13.16",
    libraryDependencies ++= Seq(Dependencies.CatsCore, Dependencies.ScalaTest, Dependencies.CatsEffect) ++
      Dependencies.AKKA ++
      Dependencies.ZIO ++ Dependencies.ZioConfig ++ Dependencies.fs2 ++ Dependencies.http4s
  )

lazy val scala3Module = (project in file("scala3Module"))
  .settings(
    name := "Scala 3 Module",
    scalaVersion := "3.3.6"
    // Add other settings and dependencies specific to this module
  )
