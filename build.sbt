/* basic project info */
name := "scalatra-shapeless-bindings"

organization := "ceedubs"

version := "0.1.0-SNAPSHOT"

description := "Request binding in Scalatra with help from Shapeless"

homepage := Some(url("https://github.com/ceedubs/scalatra-shapeless-bindings"))

startYear := Some(2013)

licenses := Seq(
  ("GPLv3", url("http://www.gnu.org/licenses/gpl-3.0.txt"))
)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/ceedubs/scalatra-shapeless-bindings"),
    "scm:git:https://github.com/ceedubs/scalatra-shapeless-bindings.git",
    Some("scm:git:git@github.com:ceedubs/scalatra-shapeless-bindings.git")
  )
)

// organizationName := "My Company"

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

/* entry point */
mainClass in (Compile, packageBin) := Some("org.scalatra.contrib.shapeless.binding.Main")

mainClass in (Compile, run) := Some("org.scalatra.contrib.shapeless.binding.Main")

/* dependencies */
libraryDependencies ++= Seq (
  "org.scalaz" %% "scalaz-core" % "7.0.2",
  "org.typelevel" %% "shapeless-scalaz" % "0.2-SNAPSHOT",
  "com.chuusai" % "shapeless" % "2.0.0-SNAPSHOT" cross CrossVersion.full
)

/* you may need these repos */
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

/* testing */
parallelExecution in Test := false

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

/* assembly plugin */
mainClass in AssemblyKeys.assembly := Some("org.scalatra.contrib.shapeless.binding.Main")

assemblySettings

test in AssemblyKeys.assembly := {}
