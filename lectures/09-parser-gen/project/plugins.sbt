/*
lazy val root = project in file(".") dependsOn antlrPlugin
lazy val antlrPlugin = uri("git://github.com/stefri/sbt-antlr#5ab3a4b1c9ff6f7d43822d79897e1f478fb4da4b")
 */
resolvers ++= Seq(
  "stefri" at "http://stefri.github.com/repo/releases"
)

addSbtPlugin("com.github.stefri" % "sbt-antlr" % "0.5")
