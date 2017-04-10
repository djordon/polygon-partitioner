val versions = new {
  val spark = "2.1.0"
  val jackson = "2.8.6"
  // can't use 3.3.0 because spark depends on 3.2.11 and they're incompatible
  val json4s = "3.5.0"
}

lazy val projectSettings = Seq(
  organization := "org",
  name := "jeom",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.8",

  fork in run := true,

  scalacOptions += "-target:jvm-1.8",
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),

  mainClass in (Compile, run) := Some("org.jeom.Polygon"),
  mainClass in assembly := Some("orm.jeom.Polygon"),

  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.url("GeoTools", url("http://download.osgeo.org/webdav/geotools/")),
  resolvers += "Sonatype OSS Snapshots" at
    "https://oss.sonatype.org/content/repositories/snapshots",

  libraryDependencies ++= Seq(
    // "org.locationtech.jts" % "jts-core" % "1.14.0",
    "com.vividsolutions" % "jts-core" % "1.14.0",
    "org.geotools.jdbc" % "gt-jdbc-postgis" % "16.2",

    // Profiling
    "com.storm-enroute" %% "scalameter-core" % "0.7",
    // "com.storm-enroute" %% "scalameter" % "0.7",

    // DB Libraries
    "org.tpolecat" %% "doobie-core" % "0.4.0",
    "org.tpolecat" %% "doobie-postgres" % "0.4.0",
    // "org.tpolecat" %% "doobie-contrib-postgresql" % "0.3.0",
    // "org.postgis" % "postgis-jdbc" % "1.3.3",

    // "org.geotools" % "gt-main" % "16.2",
    // "org.geotools" % "gt-graph" % "16.2",
    // "org.geotools" % "gt-geometry" % "16.2",
    // Others

    // Plotting
    "co.theasi" %% "plotly" % "0.2.0",

    // "org.json4s" %% "json4s-ext" % versions.json4s,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
    "com.typesafe" % "config" % "1.3.0",
    // Test
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  ),

  // dependencyOverrides ++= Set(
  //   "org.json4s" %% "json4s-jackson" % versions.json4s,
  //   "org.json4s" %% "json4s-ext" % versions.json4s
  // ),

  sourcesInBase := false,

  assemblyMergeStrategy in assembly := {
    case PathList("com", "sun", xs @ _*) => MergeStrategy.discard
    case PathList("javax", "ws", xs @ _*) => MergeStrategy.last
    case PathList("javax", "inject", xs @ _*) => MergeStrategy.last
    case PathList("javax", "servlet", xs @ _*) => MergeStrategy.last
    case PathList("javax", "activation", xs @ _*) => MergeStrategy.last
    case PathList("org", "apache", xs @ _*) => MergeStrategy.last
    case PathList("com", "google", xs @ _*) => MergeStrategy.last
    case PathList("com", "esotericsoftware", xs @ _*) => MergeStrategy.last
    case PathList("com", "codahale", xs @ _*) => MergeStrategy.last
    case PathList("com", "yammer", xs @ _*) => MergeStrategy.last
    case "about.html" => MergeStrategy.rename
    case "META-INF/ECLIPSEF.RSA" => MergeStrategy.last
    case "META-INF/mailcap" => MergeStrategy.last
    case "META-INF/mimetypes.default" => MergeStrategy.last
    case "plugin.properties" => MergeStrategy.last
    case "log4j.properties" => MergeStrategy.last
    case "overview.html" => MergeStrategy.discard
    case x => (assemblyMergeStrategy in assembly).value(x)
  }
)

lazy val root = (project in file("."))  // id = "jeom", base = file(".")
  .settings(projectSettings: _*)
