import sbt._


object Dependencies {
  val coreDependencies = Seq(
    "org.locationtech.jts" % "jts-core" % "1.15.0",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
  val dbDependencies = Seq(
    "org.tpolecat" %% "doobie-core" % "0.4.0",
    "org.tpolecat" %% "doobie-postgres" % "0.4.0"
  )
  val plotDependencies = Seq(
    "org.plotly-scala" %% "plotly-render" % "0.3.1"
  )
}
