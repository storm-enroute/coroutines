


import java.io._
import org.stormenroute.mecha._
import sbt._
import sbt.Keys._
import sbt.Process._



object CoroutinesBuild extends MechaRepoBuild {

  def repoName = "coroutines"

  /* coroutines */

  val frameworkVersion = Def.setting {
    ConfigParsers.versionFromFile(
      (baseDirectory in coroutines).value / "version.conf",
      List("coroutines_major", "coroutines_minor"))
  }

  val coroutinesCrossScalaVersions = Def.setting {
    val dir = (baseDirectory in coroutines).value
    val path = dir + File.separator + "cross.conf"
    scala.io.Source.fromFile(path).getLines.filter(_.trim != "").toSeq
  }

  val coroutinesScalaVersion = Def.setting {
    coroutinesCrossScalaVersions.value.head
  }

  val coroutinesSettings = Defaults.defaultSettings ++
    MechaRepoPlugin.defaultSettings ++ Seq(
    name := "coroutines",
    organization := "com.storm-enroute",
    version <<= frameworkVersion,
    scalaVersion <<= coroutinesScalaVersion,
    crossScalaVersions <<= coroutinesCrossScalaVersions,
    libraryDependencies <++= (scalaVersion)(sv => dependencies(sv)),
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-optimise",
      "-Yinline-warnings"
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at
        "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at
        "https://oss.sonatype.org/content/repositories/releases"
    ),
    ivyLoggingLevel in ThisBuild := UpdateLogging.Quiet,
    publishMavenStyle := true,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    pomExtra :=
      <url>http://storm-enroute.com/</url>
      <licenses>
        <license>
          <name>BSD-style</name>
          <url>http://opensource.org/licenses/BSD-3-Clause</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:storm-enroute/coroutines.git</url>
        <connection>scm:git:git@github.com:storm-enroute/coroutines.git</connection>
      </scm>
      <developers>
        <developer>
          <id>axel22</id>
          <name>Aleksandar Prokopec</name>
          <url>http://axel22.github.com/</url>
        </developer>
      </developers>,
    mechaPublishKey <<= mechaPublishKey.dependsOn(publish),
    mechaDocsRepoKey := "git@github.com:storm-enroute/apidocs.git",
    mechaDocsBranchKey := "gh-pages",
    mechaDocsPathKey := "coroutines"
  )

  def dependencies(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, major)) if major >= 11 => Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
      "org.scala-lang" % "scala-reflect" % "2.11.4"
    )
    case _ => Nil
  }

  val coroutinesCommonSettings = Defaults.defaultSettings ++
    MechaRepoPlugin.defaultSettings ++ Seq(
    name := "coroutines-common",
    organization := "com.storm-enroute",
    version <<= frameworkVersion,
    scalaVersion <<= coroutinesScalaVersion,
    crossScalaVersions <<= coroutinesCrossScalaVersions,
    libraryDependencies <++= (scalaVersion)(sv => commonDependencies(sv)),
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-optimise",
      "-Yinline-warnings"
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at
        "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at
        "https://oss.sonatype.org/content/repositories/releases"
    ),
    ivyLoggingLevel in ThisBuild := UpdateLogging.Quiet,
    publishMavenStyle := true,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    pomExtra :=
      <url>http://storm-enroute.com/</url>
      <licenses>
        <license>
          <name>BSD-style</name>
          <url>http://opensource.org/licenses/BSD-3-Clause</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:storm-enroute/coroutines.git</url>
        <connection>scm:git:git@github.com:storm-enroute/coroutines.git</connection>
      </scm>
      <developers>
        <developer>
          <id>axel22</id>
          <name>Aleksandar Prokopec</name>
          <url>http://axel22.github.com/</url>
        </developer>
      </developers>,
    mechaPublishKey <<= mechaPublishKey.dependsOn(publish),
    mechaDocsRepoKey := "git@github.com:storm-enroute/apidocs.git",
    mechaDocsBranchKey := "gh-pages",
    mechaDocsPathKey := "coroutines-common"
  )

  def commonDependencies(scalaVersion: String) =
    CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, major)) if major >= 11 => Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
      "org.scala-lang" % "scala-reflect" % "2.11.4"
    )
    case _ => Nil
  }

  lazy val coroutines: Project = Project(
    "coroutines",
    file("."),
    settings = coroutinesSettings
  ) aggregate(
    coroutinesCommon
  ) dependsOn(
    coroutinesCommon % "compile->compile;test->test"
  ) dependsOnSuperRepo

  lazy val coroutinesCommon: Project = Project(
    "coroutines-common",
    file("coroutines-common"),
    settings = coroutinesCommonSettings
  ) dependsOnSuperRepo

}
