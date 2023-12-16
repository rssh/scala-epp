


scalaVersion:="3.3.1"
organization:="ua.gradsoft"
name:="epp"
version:="0.0.1"

val spacVersion = "0.12.1"
val catsVersion = "2.10.0"

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.0-M10" % Test,
  //"com.github.geirolz" %% "cats-xml-core" % "0.0.14",
  "org.scala-lang.modules" %% "scala-xml" % "2.2.0",
  "org.typelevel" %% "cats-core" % catsVersion
)
