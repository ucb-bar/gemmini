// See README.md for license details.

name := "gemmini"

version := "3.1.0"

scalaVersion := "2.13.10"

// Belt-and-suspenders exclusion of files inside the mxgen submodule that sbt
// shouldn't compile as part of gemmini's main classpath:
//   - mxgen/test/**       : mill-style test sources (depend on scalatest/chiseltest)
//   - mxgen/out/**        : mill's transient build output (re-generates each mill invocation)
//   - mxgen/.../Main.scala: standalone Verilog emitter (App entry point)
def mxgenStaleFile(f: java.io.File): Boolean = {
  val p = f.getAbsolutePath.replace('\\', '/')
  p.contains("/mxgen/test/") ||
  p.contains("/mxgen/out/") ||
  p.endsWith("/mxgen/src/main/scala/mxgen/Main.scala")
}

Compile / unmanagedSources / excludeFilter ~= { prev =>
  prev || new sbt.io.SimpleFileFilter(f => mxgenStaleFile(f))
}

Compile / unmanagedSources := (Compile / unmanagedSources).value.filterNot(mxgenStaleFile)
Compile / sources          := (Compile / sources).value.filterNot(mxgenStaleFile)

// libraryDependencies ++= Seq(
//   "edu.berkeley.cs" %% "chisel3" % "3.6.0",
//   "edu.berkeley.cs" %% "rocketchip" % "1.2.+",
//   "org.scalanlp" %% "breeze" % "1.1")

// resolvers ++= Seq(
//   Resolver.sonatypeRepo("snapshots"),
//   Resolver.sonatypeRepo("releases"),
//   Resolver.mavenLocal)


// specified commit BEFORE scala bump to 2.13 for compatibility
// need this version for MulRecFN and fast divider
// lazy val newHardfloat = RootProject(uri("https://github.com/ucb-bar/berkeley-hardfloat.git#74cc28"))
