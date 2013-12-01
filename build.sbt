import de.johoop.jacoco4sbt._
import JacocoPlugin._

name := "revisions"

version := ".1"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

jacoco.settings
