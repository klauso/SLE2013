/**
  * This is a full SBT build definition, because it is defined in a .scala file.
  *
  * Documentation in:
  * http://www.scala-sbt.org/release/docs/Getting-Started/Full-Def.html
  */

import sbt._
import Keys._

import language.postfixOps

object SampleBuild extends Build {
  //Define the root project.
  lazy val root = Project(id = "my-proj", base = file("."))

  //Sample keys. They differ in their type, which affect how we can set their values.
  lazy val sampleSetting = settingKey[String]("Sample key which does not do much")
  lazy val sample2 = settingKey[Seq[String]]("...")

  //Define a simple compilation task.
  lazy val compileOptions = settingKey[Seq[String]]("Compile options")

  lazy val doCompile = taskKey[Seq[File]]("Compilation")

  lazy val inputFiles = taskKey[Seq[File]]("Input source files")

  override val settings: Seq[Setting[_]] =
    super.settings ++
      Seq(
        inputFiles := {
          //Use the file search DSL. Described in
          //http://www.scala-sbt.org/0.13.0/api/index.html#sbt.PathFinder,
          //http://www.scala-sbt.org/0.13.0/api/index.html#sbt.FileFilter$
          // and subclasses of FileFilter (not used here).

          (baseDirectory.value ** "*.scala" ---
            //Here we're using implicit conversions from File to RichFile and
            //to PathFinder.
            (baseDirectory.value / "project" ***)
          ) get
        },

        doCompile := {
          val cmdLine = Seq("scalac") ++ (compileOptions in root).value ++
            (inputFiles.value map (_ getPath))

          //Use (very simply) the command execution DSL - described in:
          //http://www.scala-lang.org/api/current/index.html#scala.sys.process.ProcessBuilder
          println(cmdLine !!)

          Seq.empty //FIXME
        }
      )
}
