val scala3Version = "3.2.1"


lazy val root = project.in(file("."))
  .aggregate(csvscheme.js, csvscheme.jvm)
  .settings(
    publish := {},
    publishLocal := {},
  )

lazy val csvscheme =
  crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "csvscheme",
    version := "0.1.0-SNAPSHOT",
    publish := {},
    publishLocal := {},
    scalaVersion := scala3Version,
    libraryDependencies ++=
      Seq(
        "io.higherkindness" %% "droste-core" % "0.9.0",
        "org.typelevel" %% "cats-core" % "2.9.0",
        "org.typelevel" %% "cats-free" % "2.9.0",
        "org.typelevel" %% "cats-parse" % "0.3.7",
        "org.scalameta" %% "munit" % "0.7.29" % Test)
  ).
  jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies ++=
      Seq(
          "io.higherkindness" %% "droste-core" % "0.9.0",
          "org.typelevel" %% "cats-core" % "2.9.0",
          "org.typelevel" %% "cats-free" % "2.9.0",
          "org.typelevel" %% "cats-parse" % "0.3.7",
          "org.scalameta" %% "munit" % "0.7.29" % Test)
  ).
  jsSettings(
    // Add JS-specific settings here
    //.cross(CrossVersion.for3Use2_13)
    libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "2.1.1",
    scalaJSUseMainModuleInitializer := true
  )
