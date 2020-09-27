lazy val root = (project in file(".")).
  settings(
    name := "WhiteboxHH",
    version := "0.1.0",
    scalaVersion := "2.12.3"
    , mainClass in (Compile, run) := Some("whiteboxhh.WhiteboxHH")
  )

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.5",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "org.apache.commons" % "commons-math3" % "3.6.1",
)

// End ///////////////////////////////////////////////////////////////


