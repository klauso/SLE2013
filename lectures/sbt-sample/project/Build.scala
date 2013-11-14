import sbt._
import Keys._

import language.postfixOps

object SampleBuild extends Build {
  lazy val root = Project(id = "my-proj", base = file("."))
  override val settings: Seq[Setting[_]] =
    super.settings ++
      Seq(
        inputFiles := {
          (baseDirectory.value ** "*.scala" ---
            (baseDirectory.value / "project" ***)
          ) get
        },

        doCompile := {
          val cmdLine = Seq("scalac") ++ (compileOptions in root).value ++
            (inputFiles.value map (_ getPath))

          println(cmdLine !!)

          Seq.empty //FIXME
        }
      )

  lazy val sampleSetting = settingKey[String]("Sample key which does not do much")

  lazy val sample2 = settingKey[Seq[String]]("...")

  lazy val compileOptions = settingKey[Seq[String]]("Compile options")

  lazy val doCompile = taskKey[Seq[File]]("Compilation")

  lazy val inputFiles = taskKey[Seq[File]]("Input source files")
}
