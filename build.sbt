lazy val root = (project in file(".")).
  settings(
    name := "scala-fp"
  )

scalaOrganization := "org.typelevel"
scalaVersion := "2.11.8"

scalacOptions += "-Ypartial-unification" // enable fix for SI-2712

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.8.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
