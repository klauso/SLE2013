sampleSetting := "some value"

sampleSetting += "; and then some"

sampleSetting += sample2.value.mkString

compileOptions := Seq.empty

compileOptions += "-Xprint:typer"

scalacOptions += "-Xprint:typer"

//Some example dependencies:

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-actors" % scalaVersion.value,
    "org.scalatest" %% "scalatest" % "2.0"
  )

sample2 := Seq("a", "b")
