import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.3"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.0.1"
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
ThisBuild / resolvers += Resolver.sonatypeCentralRepo("releases")

ThisBuild / sonatypeProfileName := "io.github.edadma"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/edadma/nex"),
    "scm:git@github.com:edadma/nex.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "edadma",
    name = "Edward A. Maxedon, Sr.",
    email = "edadma@gmail.com",
    url = url("https://github.com/edadma"),
  ),
)

ThisBuild / homepage := Some(url("https://github.com/edadma/nex"))
ThisBuild / description := "Nex: a modern, AOT-compiled, array-first numerical programming language."

ThisBuild / publishTo := sonatypePublishToBundle.value

lazy val nex = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "nex",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
      "io.github.edadma"       %%% "indentation"              % "0.0.4",
      "io.github.edadma"       %%% "path"                     % "0.0.6",
      "com.github.scopt"       %%% "scopt"                    % "4.1.0",
      "com.lihaoyi"            %%% "pprint"                   % "0.9.6",
      "org.scalatest"          %%% "scalatest"                % "3.2.19" % "test",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .nativeSettings(
//    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    //  scalaJSLinkerConfig ~= { _.withModuleSplitStyle(ModuleSplitStyle.SmallestModules) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    //    Test / scalaJSUseMainModuleInitializer := true,
    //    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    // Library bundle: no @main initializer. The playground entry
    // point is reached via `@JSExportTopLevel("nexRunSource")` in
    // js/src/main/scala/io/github/edadma/nex/Playground.scala.
    // The CLI's `@main run` lives in jvm/src/main/scala/ so it
    // doesn't drag ProcessBuilder/Process into the JS link.
    scalaJSUseMainModuleInitializer        := false,
    // Bake the source prelude into the JS bundle as a string
    // constant. The playground entry parses + prepends it to
    // `elaborateProject(...)` so the bundle gets the same complex
    // sqrt / sin / cos / etc. overloads the JVM CLI gets, without
    // having to reach the filesystem at runtime (which would drag
    // `cross_platform` → fs into the link graph and break the
    // browser bundle). The generator scans every `.nex` file
    // directly under `prelude/` and emits one entry per file so
    // future additions to the source prelude land automatically.
    Compile / sourceGenerators += Def.task {
      val out =
        (Compile / sourceManaged).value /
          "io" / "github" / "edadma" / "nex" / "BakedPrelude.scala"
      val preludeDir = (ThisBuild / baseDirectory).value / "prelude"
      val entries =
        if (preludeDir.isDirectory)
          IO.listFiles(preludeDir).filter(_.getName.endsWith(".nex")).toList.sortBy(_.getName)
        else Nil
      // Encode each prelude file as a Base64 string in the generated
      // Scala source so triple-quotes / dollar-signs / etc. in the
      // Nex source can't trip up Scala's string-literal grammar. The
      // playground decodes back to UTF-8 at parse time.
      val b64 = java.util.Base64.getEncoder
      val lines = entries.map { f =>
        val enc = b64.encodeToString(IO.readBytes(f))
        "(\"" + f.getName + "\", \"" + enc + "\")"
      }
      val body =
        "package io.github.edadma.nex\n\n" +
        "// Source-prelude files baked into the JS bundle at build time.\n" +
        "// Each entry is (filename, base64-encoded UTF-8 source); the\n" +
        "// playground decodes, parses each entry, and hands the resulting\n" +
        "// LoadedModule(path = List(\"prelude\"), ...) to\n" +
        "// NexElaborator.elaborateProject so the in-browser interpreter\n" +
        "// sees the same overload set the JVM CLI's loadAndElaborate\n" +
        "// provides. Regenerated by the JS source-generator in build.sbt\n" +
        "// whenever a prelude .nex file changes.\n" +
        "object BakedPrelude:\n" +
        "  private val raw: List[(String, String)] = List(\n    " +
        lines.mkString(",\n    ") + "\n  )\n\n" +
        "  val files: List[(String, String)] = raw.map { case (name, enc) =>\n" +
        "    (name, new String(java.util.Base64.getDecoder.decode(enc), \"UTF-8\"))\n" +
        "  }\n"
      IO.write(out, body)
      Seq(out)
    }.taskValue,
//    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
  )

lazy val root = project
  .in(file("."))
  .aggregate(nex.js, nex.jvm, nex.native)
  .settings(
    name                := "nex",
    publish / skip      := true,
    publishLocal / skip := true,
  )
