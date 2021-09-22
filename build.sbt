
val testDep = Seq("org.scalactic" %% "scalactic" % "3.2.9",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test")
libraryDependencies ++= testDep
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.36.0.2"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked"
)
val common = Seq(version := "0.1",
              scalaVersion := "3.0.0",
  libraryDependencies ++= testDep)
lazy val `data-modeler` = (project in file(".") ).settings{
  name := "data-modeler"

}.settings( common)

lazy val `server` = (project in file("data-server") ).settings{
  name := "data-server"

  libraryDependencies ++=Seq (
    ("com.typesafe.akka" %% "akka-http" % "10.2.6").cross(CrossVersion.for3Use2_13),
    ("com.typesafe.akka" %% "akka-stream" % "2.6.16").cross(CrossVersion.for3Use2_13),
    ("io.spray" %% "spray-json" % "1.3.6").cross(CrossVersion.for3Use2_13),
    ("com.typesafe.akka" %% "akka-actor-typed" % "2.6.16").cross(CrossVersion.for3Use2_13),
    ("com.typesafe.akka" %% "akka-http-spray-json" % "10.2.6").cross(CrossVersion.for3Use2_13),
  ("com.typesafe.akka" %% "akka-slf4j" % "2.6.16").cross(CrossVersion.for3Use2_13),
  ("ch.qos.logback" % "logback-classic" % "1.2.6"))
}.settings( common).dependsOn(`data-modeler`)
