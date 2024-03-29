import Dependencies._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "scala-to",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.2.0",
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  )

