name := "QLearning"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.3.0"
)

scalacOptions ++= Seq(
  "-feature"
)

assemblyJarName in assembly := "qlearning.jar"
