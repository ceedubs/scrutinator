import sbt._
import Keys._

object Dependencies {
  import BuildSettings._
  val scalatraVersion = "2.3.0.RC1"

  object Scalatra {
    val core = "org.scalatra" %% "scalatra" % scalatraVersion
    val commands = "org.scalatra" %% "scalatra-commands" % scalatraVersion
    val swagger = "org.scalatra" %% "scalatra-swagger" % scalatraVersion
    val scalatraSpecs2 = "org.scalatra" %% "scalatra-specs2" % scalatraVersion
  }

  object Tests {
    val specs2 = "org.specs2" %% "specs2" % "2.3.11"
    val scalacheck = "org.scalacheck" %% "scalacheck" % "1.11.3"
  }

  val commonDependencies = Seq(
    Scalatra.core,
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "com.chuusai" % "shapeless_2.10.4" % "2.0.0",
    "org.typelevel" %% "shapeless-scalaz" % "0.2")
}

object BuildSettings {
  import Dependencies._

  val buildVersion = "0.1.0-SNAPSHOT"
  val buildScalaVersion = "2.10.4"

  val buildScalacOptions = Seq(
    "-deprecation",
    "-language:higherKinds",
    "-Yclosure-elim",
    "-Yinline",
    "-Yinline-warnings",
    "-unchecked",
    "-feature",
    "-Xfatal-warnings",
    "-Xlint",
    "-optimize",
    "-encoding", "utf8",
    "-Xcheckinit")

  val buildLicenses = Seq(
    "MIT License" -> url("http://www.opensource.org/licenses/mit-license.html"))

  val buildScmInfo = Some(
      ScmInfo(
        url("https://github.com/ceedubs/scrutinator"),
        "scm:git:https://github.com/ceedubs/scrutinator.git",
        Some("scm:git:git@github.com:ceedubs/scrutinator.git")))

  val nexus = "https://oss.sonatype.org/"

  val buildPublishTo =
    if (buildVersion.trim.endsWith("SNAPSHOT"))
      Some( "snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")

  val pomSettings = Seq(
    pomIncludeRepository := { _ => false },

    pomExtra := (
      <developers>
        <developer>
          <id>ceedubs</id>
          <name>Cody Allen</name>
          <email>ceedubs@gmail.com</email>
          <url>http://github.com/ceedubs</url>
        </developer>
      </developers>
    )
  )

  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    organization := "net.ceedubs",
    version := "0.1.0-SNAPSHOT",
    description := "Request binding in Scalatra with help from Shapeless",
    homepage := Some(url("https://github.com/ceedubs/scrutinator")),
    scalaVersion := buildScalaVersion,
    licenses := buildLicenses,
    scmInfo := buildScmInfo,
    startYear := Some(2013),
    scalacOptions := buildScalacOptions,
    parallelExecution in Test := true,
    resolvers ++= Seq(
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases")),
    publishArtifact in Test := false,
    libraryDependencies ++= Dependencies.commonDependencies)
}

object build extends Build {

  lazy val root = Project (
    "scrutinator",
    file("."),
    settings = BuildSettings.defaultSettings
  ) aggregate (core, scrutinatorJson4s, scrutinatorScalatra, scrutinatorSwagger)

  lazy val core = Project(
    "scrutinator-core",
    file("core"),
    settings = BuildSettings.defaultSettings ++ Seq(
      libraryDependencies ++= Seq(
        "javax.servlet" % "javax.servlet-api" % "3.1.0",
        Dependencies.Tests.specs2 % "test",
        Dependencies.Tests.scalacheck % "test")
    )
  )

  lazy val scrutinatorJson4s = Project(
    "scrutinator-json4s",
    file("json4s"),
    settings = BuildSettings.defaultSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.json4s" %% "json4s-jackson" % "3.2.7")
    )
  ) dependsOn (core % "test->test;compile->compile")

  lazy val scrutinatorScalatra = Project(
    "scrutinator-scalatra",
    file("scalatra"),
    settings = BuildSettings.defaultSettings ++ Seq(
      libraryDependencies ++= Seq(
        Dependencies.Scalatra.commands,
        Dependencies.Scalatra.swagger,
        Dependencies.Scalatra.scalatraSpecs2 % "test"
      )
    )
  ) dependsOn (core % "test->test;compile->compile")

  lazy val scrutinatorSwagger = Project(
    "scrutinator-swagger",
    file("swagger"),
    settings = BuildSettings.defaultSettings
  ) dependsOn (core % "test->test;compile->compile", scrutinatorScalatra % "test->test;compile->compile")
}
