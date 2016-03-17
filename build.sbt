import sbtassembly.Plugin.AssemblyKeys._

lazy val root = (project in file(".")).
  settings(
    name := "ScalaLinter",
    organization := "edu.rit.csh",
    version := "0.1.0",
    scalaVersion := "2.11.7",
    assemblySettings,
    jarName in assembly := "ScalaLinter.jar",
    test in assembly := {},
    antlr4Settings,
    antlr4PackageName in Antlr4 := Some("edu.rit.csh.scalaLint.parser"),
    libraryDependencies ++= Seq(
      "org.antlr" % "antlr4-runtime" % "4.5.2-1",
      "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4",
      "com.lihaoyi" %% "fastparse" % "0.3.5",
      "org.scalatest" % "scalatest_2.11" % "3.0.0-M15" % "test"
  )
)
