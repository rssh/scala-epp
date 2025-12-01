scalaVersion := "3.7.4"
organization := "com.github.rssh"
name := "scala-epp"
enablePlugins(ScalaxbPlugin)
scalaxbPackageName := "ua.gradsoft.epp.xsdmodel"
scalaxbJaxbPackage := sbtscalaxb.ScalaxbPlugin.autoImport.JaxbPackage.Jakarta
libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "jakarta.xml.bind" % "jakarta.xml.bind-api" % "4.0.2",
  "org.glassfish.jaxb" % "jaxb-runtime" % "4.0.6",
  "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M20",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test"
)
