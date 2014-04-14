/* basic project info */
name := "scrutinator"

organization := "net.ceedubs"

version := "0.1.0-SNAPSHOT"

description := "Request binding in Scalatra with help from Shapeless"

homepage := Some(url("https://github.com/ceedubs/scrutinator"))

startYear := Some(2013)

licenses := Seq(
  "MIT License" -> url("http://www.opensource.org/licenses/mit-license.html")
)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/ceedubs/scrutinator"),
    "scm:git:https://github.com/ceedubs/scrutinator.git",
    Some("scm:git:git@github.com:ceedubs/scrutinator.git")
  )
)

/* scala versions and options */
scalaVersion := "2.10.2"

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-encoding", "UTF-8"
  // "-optimise"   // this option will slow your build
)

scalacOptions ++= Seq(
  "-Yclosure-elim",
  "-Yinline"
)

// These language flags will be used only for 2.10.x.
// Uncomment those you need, or if you hate SIP-18, all of them.
scalacOptions <++= scalaVersion map { sv =>
  if (sv startsWith "2.10") List(
    "-Xverify",
    "-Ywarn-all",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:experimental.macros"
  )
  else Nil
}

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

/* dependencies */
libraryDependencies ++= Seq (
  "org.scalaz" %% "scalaz-core" % "7.0.3",
  "org.typelevel" %% "shapeless-scalaz" % "0.2-SNAPSHOT" changing(),
  "com.chuusai" % "shapeless_2.10.2" % "2.0.0",
  "org.scalatra" %% "scalatra" % "2.3.0.RC1",
  "org.scalatra" %% "scalatra-commands" % "2.3.0.RC1",
  "org.scalatra" %% "scalatra-swagger"  % "2.3.0.RC1",
  "org.scalatra" %% "scalatra-specs2"  % "2.3.0.RC1" % "test",
  "javax.servlet"          % "javax.servlet-api"  % "3.1.0",
  "org.specs2"     %% "specs2"         % "2.3.1"    % "test",
  "org.scalacheck" %% "scalacheck"     % "1.10.1"   % "test",
  "org.json4s" %% "json4s-jackson" % "3.2.7"
)

/* you may need these repos */
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

/* testing */
parallelExecution in Test := true

/* sbt behavior */
logLevel in compile := Level.Warn

traceLevel := 5

offline := false

/* publishing */
publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) Some(
    "snapshots" at nexus + "content/repositories/snapshots"
  )
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

mappings in (Compile, packageBin) ~= { (ms: Seq[(File, String)]) =>
  ms filter { case (file, toPath) =>
      toPath != "application.conf"
  }
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

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
