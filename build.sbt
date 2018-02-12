import Dependencies._

lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "org.oshikiri",
      scalaVersion := "2.11.12", // required by https://github.com/jupyter-scala/jupyter-scala
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "bandit-example",
    libraryDependencies ++= Seq(
      "org.scalanlp" %% "breeze" % "0.13.2",
      "org.scalanlp" %% "breeze-natives" % "0.13.2"
    )
  )

scalacOptions ++= (
  "-Ywarn-unused" ::
  "-Ywarn-unused-import" ::
  "-unchecked" ::
  "-Xlint" ::
  Nil
)

scalafmtConfig in ThisBuild := Some(file(".scalafmt.conf"))
