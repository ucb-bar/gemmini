// See README.md for license details.

name := "gemmini"

version := "3.1.0"

scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % "3.5.6",
  "edu.berkeley.cs" %% "rocketchip" % "1.2.+",
  "edu.berkeley.cs" %% "chisel-iotesters" % "2.5.6",
  "org.scalanlp" %% "breeze" % "1.1")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal)
