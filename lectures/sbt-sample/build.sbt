sampleSetting := "some value"

sampleSetting += "; and then some"

sampleSetting += sample2.value.mkString

compileOptions := Seq.empty

compileOptions += "-Xprint:typer"

scalacOptions += "-Xprint:typer"

//libraryDependencies += "org.scala-lang" % "scala-actors" % scalaVersion.value

//libraryDependencies += "org.scalatest" % "scalatest" %% "1.0"

sample2 := Seq("a", "b")
