import sbt.Keys.homepage
import sbt.url
import xerial.sbt.Sonatype.GitHubHosting

val thisReleaseVersion = "0.0.1"

val scala3V                = "3.7.0"
val scalaV                 = scala3V
val supportedScalaVersions = Seq(scalaV)
def munitFramework         = new TestFramework("munit.Framework")

val munitTest         = "org.scalameta"        %% "munit"      % "0.7.29" % Test
val assertVerboseTest = "com.eed3si9n.expecty" %% "expecty"    % "0.16.0" % Test
val os_lib            = "com.lihaoyi"          %% "os-lib"     % "0.9.3"
val fastparse         = "com.lihaoyi"          %% "fastparse"  % "3.1.1"
val enumeratum        = "com.beachape"         %% "enumeratum" % "1.7.6"

val kindProjector = "org.typelevel"   % "kind-projector" % "0.13.3" cross CrossVersion.full
val reflections   = "org.reflections" % "reflections"    % "0.10.2"
val sourcecode    = "com.lihaoyi"    %% "sourcecode"     % "0.4.2"

val kindProjectorPlugin = compilerPlugin(kindProjector)

def scala_reflect(value: String) = "org.scala-lang" % "scala-reflect" % value % Compile

lazy val publishingOptions = Seq(
  organization           := "io.chymyst",
  version                := thisReleaseVersion,
  ThisBuild / version    := thisReleaseVersion,
  licenses               := Seq("Apache License, Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage               := Some(url("https://github.com/winitzki/scall")),
  description            := "Implementation of the Earley parsing algorithm in Scala",
  publishTo              := sonatypePublishToBundle.value,
  sonatypeProjectHosting := Some(GitHubHosting("winitzki", "earley", "winitzki@gmail.com")),
)

lazy val noPublishing =
  Seq(version := thisReleaseVersion, publishArtifact := false, publishMavenStyle := true, publish := {}, publishLocal := {}, publish / skip := true)

lazy val jdkModuleOptions: Seq[String] = {
  val jdkVersion = scala.sys.props.get("JDK_VERSION")
  val options    = if (jdkVersion exists (_ startsWith "8.")) Seq() else Seq("--add-opens", "java.base/java.util=ALL-UNNAMED")
  println(s"Additional JDK ${jdkVersion.getOrElse("")} options: ${options.mkString(" ")}")
  options ++ Seq("-Xss1000000") // Make the stack size larger.
}

lazy val root = (project in file("."))
  .settings(noPublishing)
  .settings(scalaVersion := scalaV, crossScalaVersions := supportedScalaVersions, name := "earley-root")
  .aggregate(earley_core, earley_macros, earley_typeclasses, earley_testutils)

lazy val earley_core = (project in file("earley-core"))
  .settings(noPublishing)
  .settings(
    name                     := "earley-core",
    scalaVersion             := scalaV,
    crossScalaVersions       := supportedScalaVersions,
    Test / parallelExecution := true,
    Test / fork              := true,
    coverageEnabled          := false,
    scalafmtFailOnErrors     := false, // Cannot disable the unicode surrogate pair error in Parser.scala?
    testFrameworks += munitFramework,
    Test / javaOptions ++= jdkModuleOptions,
    Compile / scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _))       => Seq("-Ydebug")
        case Some((2, 12 | 13)) => Seq("-Ypatmat-exhaust-depth", "10") // Cannot make it smaller than 10. Want to speed up compilation.
      }
    },
    ThisBuild / scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _))       => Seq("-Xkind-projector") // Seq("-Xkind-projector:underscores")
        case Some((2, 12 | 13)) => Seq()                   // Seq("-Xsource:3", "-P:kind-projector:underscore-placeholders")
      }
    },
    // We need to run tests in forked JVM starting with the current directory set to the base resource directory.
    // That base directory should contain `./dhall-lang` and all files below that.
    Test / baseDirectory     := (Test / resourceDirectory).value,
    // addCompilerPlugin is a shortcut for libraryDependencies += compilerPlugin(dependency)
    // See https://stackoverflow.com/questions/67579041
    libraryDependencies ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(scala_reflect(scalaVersion.value), kindProjectorPlugin)
        case Some((3, _)) => Seq.empty // No need for scala-reflect with Scala 3.
      }),
    libraryDependencies ++= Seq(munitTest, assertVerboseTest, enumeratum,  sourcecode, os_lib % Test),
  ).dependsOn(earley_testutils % "test->compile", earley_typeclasses, earley_macros)

lazy val earley_testutils = (project in file("earley-testutils"))
  .settings(publishingOptions)
  .settings(
    name                     := "earley-testutils",
    scalaVersion             := scalaV,
    crossScalaVersions       := supportedScalaVersions,
    Test / parallelExecution := true,
    Test / fork              := true,
    testFrameworks += munitFramework,
    Test / javaOptions ++= jdkModuleOptions,
    libraryDependencies ++= Seq(munitTest, assertVerboseTest),
  )
lazy val earley_macros    = (project in file("earley-macros"))
  .settings(publishingOptions)
  .settings(
    name                     := "earley-macros",
    scalaVersion             := scalaV,
    crossScalaVersions       := supportedScalaVersions,
    Test / parallelExecution := true,
    testFrameworks += munitFramework,
    libraryDependencies ++= Seq(munitTest, assertVerboseTest),
    libraryDependencies ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(scala_reflect(scalaVersion.value), kindProjectorPlugin)
        case Some((3, _)) => Seq.empty // No need for scala-reflect with Scala 3.
      }),
  )

lazy val earley_typeclasses = (project in file("earley-typeclasses"))
  .settings(publishingOptions)
  .settings(
    name                     := "earley-typeclasses",
    scalaVersion             := scalaV,
    crossScalaVersions       := supportedScalaVersions,
    Test / parallelExecution := true,
    testFrameworks += munitFramework,
    libraryDependencies ++= Seq(munitTest, assertVerboseTest),
    libraryDependencies ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(kindProjectorPlugin)
        case Some((3, _)) => Seq.empty
      }),
  )

/////////////////////////////////////////////////////////////////////////////////////////////////////
// Publishing to Sonatype Maven repository
publishMavenStyle      := true
publishTo              := sonatypePublishToBundle.value
sonatypeProfileName    := "io.chymyst"
//ThisBuild / sonatypeCredentialHost := sonatypeCentralHost  // Not relevant because io.chymyst was created before 2021.
//
Test / publishArtifact := false
//
/////////////////////////////////////////////////////////////////////////////////////////////////////
