lazy val root = project.in(file(".")).aggregate(rootJS, rootJVM)

lazy val rootJS = project.in(file("proofpeer-versionary-js")).settings(scalaJSSettings: _*).settings(
  name := "proofpeer-versionary", 
  organization := "net.proofpeer", 
  scalaVersion := "2.11.2",
  version := "0.1-SNAPSHOT",
  unmanagedSourceDirectories in Compile +=
    (baseDirectory.value / "..") / "proofpeer-versionary-shared" / "src" / "main" / "scala",
  libraryDependencies += "net.proofpeer" %%% "proofpeer-general" % "0.1-SNAPSHOT",
  libraryDependencies += "net.proofpeer" %%% "proofpeer-indent" % "0.5-SNAPSHOT"
)

lazy val rootJVM = project.in(file("proofpeer-versionary-jvm")).settings(
  name := "proofpeer-versionary",
  organization := "net.proofpeer",
  scalaVersion := "2.11.2",
  version := "0.1-SNAPSHOT",
  unmanagedSourceDirectories in Compile +=
    (baseDirectory.value / "..") / "proofpeer-versionary-shared" / "src" / "main" / "scala",
  libraryDependencies += "net.proofpeer" %% "proofpeer-general" % "0.1-SNAPSHOT",
  libraryDependencies += "net.proofpeer" %% "proofpeer-indent" % "0.5-SNAPSHOT",  
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)
