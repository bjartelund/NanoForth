scalaVersion := "3.9.0"

lazy val root = rootProject
  .settings(
    name := "NanoForth",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.2.3" % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )
