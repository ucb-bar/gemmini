// See README.md for license details.

name := "gemmini"

version := "3.1.0"

scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % "3.6.0",
  "edu.berkeley.cs" %% "rocketchip" % "1.2.+",
  "org.scalanlp" %% "breeze" % "1.1")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal)

// specified commit BEFORE scala bump to 2.13 for compatibility
// need this version for MulRecFN and fast divider
// lazy val newHardfloat = RootProject(uri("https://github.com/ucb-bar/berkeley-hardfloat.git#74cc28"))
