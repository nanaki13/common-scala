name := "data-modeler"
version := "0.1"
scalaVersion := "3.0.0"


libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.36.0.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked"
)