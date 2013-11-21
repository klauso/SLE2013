lazy val root = project in file(".") dependsOn antlrPlugin
lazy val antlrPlugin = uri("git://github.com/Blaisorblade/sbt-antlr#sbt0.13")
/*
resolvers ++= Seq(
  "stefri" at "http://stefri.github.com/repo/releases"
)
addSbtPlugin("com.github.stefri" % "sbt-antlr" % "0.5")
 */
