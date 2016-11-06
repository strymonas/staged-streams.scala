name := "staged-streams"

version := "1.0"

scalaVersion := "2.11.2"

scalaOrganization := "org.scala-lang.virtualized"

resolvers ++= Seq(Resolver.sonatypeRepo("releases"),
		  Resolver.sonatypeRepo("snapshots"))

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "0.9.0"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.11.2"

enablePlugins(JmhPlugin)

scalacOptions += "-Yvirtualize"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck,
  "-maxSize", "5",
  "-minSuccessfulTests", "100",
  "-workers", "2",
  "-verbosity", "1")

javaOptions in run ++= Seq("-Xms6g", "-Xmx6g", "-Xss4m",
			   "-XX:+CMSClassUnloadingEnabled",
			   "-XX:ReservedCodeCacheSize=256m",
			   "-XX:-TieredCompilation", "-XX:+UseNUMA")
