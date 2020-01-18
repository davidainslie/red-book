import Dependencies._
import sbt._

lazy val root = project("red-book", file("."))
  .settings(description := "Scala Red Book by Backwards")
  .settings(javaOptions in Test ++= Seq("-Dconfig.resource=application.test.conf"))

def project(id: String, base: File): Project =
  Project(id, base)
    .enablePlugins(JavaAppPackaging)
    .settings(
      resolvers ++= Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots"),
        Resolver.bintrayRepo("cakesolutions", "maven"),
        "Artima Maven Repository" at "http://repo.artima.com/releases",
        "jitpack" at "https://jitpack.io",
        "Confluent Platform Maven" at "http://packages.confluent.io/maven/"
      ),
      scalaVersion := BuildProperties("scala.version"),
      sbtVersion := BuildProperties("sbt.version"),
      organization := "com.backwards",
      name := id,
      autoStartServer := false,
      addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
      libraryDependencies ++= dependencies,
      scalacOptions += "-Ymacro-annotations",
      fork := true,
      publishArtifact in Test := true
    )