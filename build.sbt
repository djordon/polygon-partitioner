import Dependencies._


val commonSettings = Seq(
  organization := "org.partitioner",
  scalaVersion in ThisBuild := "2.12.4",
  resolvers += Resolver.sonatypeRepo("releases"),
  fork in run := true,
  sourcesInBase := false,
  scalacOptions ++= Seq("-target:jvm-1.8", "-feature")
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(core, plot, db)
  .dependsOn(core, plot, db)

lazy val core = (project in file("core"))
  .settings(commonSettings: _*)
  .settings(
    Seq(
      name := "polygon-partitioner-core",
      version := "0.1.1",
      coverageEnabled := true
    ),
    libraryDependencies ++= coreDependencies
  )

lazy val plot = (project in file("plot"))
  .settings(commonSettings: _*)
  .settings(
    Seq(
      name := "polygon-partitioner-plot",
      version := "0.1.1",
      coverageEnabled := false
    ),
    libraryDependencies ++= plotDependencies)
  .dependsOn(core % "test->test;compile->compile")

lazy val db = (project in file("db"))
  .settings(commonSettings: _*)
  .settings(
    Seq(
      name := "polygon-partitioner-db",
      version := "0.1.1",
      coverageEnabled := false
    ),
    libraryDependencies ++= dbDependencies)
  .dependsOn(core % "test->test;compile->compile")
