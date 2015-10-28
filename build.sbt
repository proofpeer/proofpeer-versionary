lazy val root = project.in(file(".")).aggregate(fooJS, fooJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val foo = crossProject.in(file(".")).
  settings(
    name := "ProofPeer Versionary",
    organization := "net.proofpeer",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.7",
    scalacOptions += "-deprecation",
    libraryDependencies += "net.proofpeer" %%% "proofpeer-general" % "0.1-SNAPSHOT",
    libraryDependencies += "net.proofpeer" %%% "proofpeer-indent" % "0.5-SNAPSHOT"
  ).
  jvmSettings(
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
  )

lazy val fooJS = foo.js
lazy val fooJVM = foo.jvm
