

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

}.settings( common)

lazy val `server` = (project in file("data-server") ).settings{
  name := "data-server"

  libraryDependencies += ("com.typesafe.akka" %% "akka-http" % "10.2.6").cross(CrossVersion.for3Use2_13)
 // libraryDependencies += ("com.typesafe.akka" %% "akka-actor" % "2.6.16").cross(CrossVersion.for3Use2_13)
  libraryDependencies += ("com.typesafe.akka" %% "akka-actor-typed" % "2.6.16").cross(CrossVersion.for3Use2_13)
}.settings( common)
