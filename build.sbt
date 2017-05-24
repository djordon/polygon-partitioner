lazy val projectSettings = Seq(
  organization := "org",
  name := "partitioner",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.8",

  fork in run := true,

  scalacOptions ++= Seq("-target:jvm-1.8", "-feature"),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),

  mainClass in (Compile, run) := Some("org.partitioner.OrthogonalPolygonPartitioner"),
  mainClass in assembly := Some("orm.partitioner.OrthogonalPolygonPartitioner"),

  resolvers += Resolver.sonatypeRepo("releases"),

  libraryDependencies ++= Seq(
    "com.vividsolutions" % "jts-core" % "1.14.0",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  ),

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

lazy val root = (project in file(".")).settings(projectSettings: _*)
