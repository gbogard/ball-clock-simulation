import sbt._

object Dependencies {
  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"

  object Monocle {
    val version = "2.0.0-RC1"
    val core = "com.github.julien-truffaut" %% "monocle-core" % version
    val `macro` = "com.github.julien-truffaut" %% "monocle-macro" % version
    val all = Seq(core, `macro`)
  }

  object Circe {
    private val version = "0.11.0"
    val core = "io.circe" %% "circe-core" % version
    val generic = "io.circe" %% "circe-generic" % version
    val all: Seq[ModuleID] = Seq(core, generic)
  }
}
