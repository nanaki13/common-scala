

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.36.0.2"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked"
)
val common = {version := "0.1"
              scalaVersion := "3.0.0"}
lazy val `data-modeler` = (project in file(".") ).settings{
  name := "data-modeler"
  common
}

lazy val `server` = (project in file("data-server") ).settings{
  name := "data-server"
  common
}
