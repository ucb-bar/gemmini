// mxgen lives as a sibling of this directory (at <gemmini>/mxgen). Its sources
// are not under sbt's scalaSource walk, so they have to be added explicitly.
// Only src/main/scala is included — the mill workspace, tests, and Main.scala
// (in elab/) stay invisible to sbt.
Compile / unmanagedSourceDirectories +=
  baseDirectory.value.getParentFile / "mxgen" / "src" / "main" / "scala"
