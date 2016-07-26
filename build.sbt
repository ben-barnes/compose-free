lazy val naturalcoproducts = ( project in file ( "." ) ).
  settings( commonSettings ).
  settings( dependencies ).
  settings ( plugins )

lazy val commonSettings = Seq (
  organization := "io.chronon",
  name         := "naturalcoproducts",
  version      := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq (
    "-deprecation",
    "-feature",
    "-optimise",
    "-unchecked",
    "-Xlint:_"
  )
)

lazy val dependencies = Seq (
  libraryDependencies ++= Seq (
    "org.scalaz" %% "scalaz-core" % "7.2.4",
    "org.scalaz" %% "scalaz-effect" % "7.2.4"
  )
)

lazy val plugins = Seq (
  resolvers += Resolver.sonatypeRepo ( "releases" ),
  addCompilerPlugin ( "org.spire-math" %% "kind-projector" % "0.8.0" )
)
