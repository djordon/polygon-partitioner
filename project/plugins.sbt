resolvers += Resolver.sonatypeRepo("releases")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5" exclude("org.apache.maven", "maven-plugin-api"))
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
//addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.1.0")
