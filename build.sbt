import de.johoop.jacoco4sbt._
import JacocoPlugin._

name := "revisions"

version := ".1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "com.twitter" %% "util-core" % "6.12.1"
)

jacoco.settings
