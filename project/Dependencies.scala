import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
  lazy val cats = "org.typelevel" %% "cats-core" % "2.0.0-RC1" 
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "1.3.1"

  object Monocle {
    lazy val version = "2.0.0-RC1"
    lazy val core = "com.github.julien-truffaut" %%  "monocle-core"  % version
    lazy val `macro` = "com.github.julien-truffaut" %%  "monocle-macro" % version 
    lazy val all = Seq(core, `macro`)
  }
}
