import Dependencies._

lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "org.oshikiri",
      scalaVersion := "2.11.11",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "bandit-example",
    libraryDependencies ++= Seq(
      "org.scalanlp" %% "breeze" % "0.13.2",
      "org.scalanlp" %% "breeze-natives" % "0.13.2"
    )
  )

SettingKey[Unit]("scalafmtGenerateConfig") :=
  IO.write( // writes to file once when build is loaded
    file(".scalafmt.conf"),
    """style = IntelliJ
      |# Your configuration here
      """.stripMargin.getBytes("UTF-8")
  )
scalacOptions ++= (
  "-Ywarn-unused" ::
  "-Ywarn-unused-import" ::
  "-unchecked" ::
  "-Xlint" ::
  Nil
)
