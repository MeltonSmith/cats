ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.13.3"

val catsVersion = "2.1.1"

lazy val root = (project in file("."))
  .settings(
    name := "cats"
  )


libraryDependencies ++= Seq (
  "org.typelevel" %% "cats-core" % catsVersion
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)