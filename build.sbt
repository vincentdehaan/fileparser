import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "nl.vindh"
ThisBuild / organizationName := "vindh"

lazy val root = (project in file("."))
  .settings(
    name := "fileparser",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.tpolecat" %% "atto-core"    % "0.6.5",
    libraryDependencies += "org.tpolecat" %% "atto-refined" % "0.6.5"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
