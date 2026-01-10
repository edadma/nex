# cross_template

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/cross_template_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/cross_template)](https://github.com/edadma/cross_template/commits)
![GitHub](https://img.shields.io/github/license/edadma/cross_template)
![Scala Version](https://img.shields.io/badge/Scala-3.7.4-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.20.1-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.9-blue.svg)

A Scala 3 cross-platform project template that compiles to JVM, JavaScript (Scala.js), and Native (Scala Native) targets.

## Overview

This template provides a ready-to-use structure for creating Scala applications that can run on multiple platforms. It demonstrates how to set up a cross-platform Scala project using sbt-crossproject, allowing you to write code once and compile it for:

- **JVM** - Traditional Java Virtual Machine deployment
- **JavaScript** - Browser and Node.js environments via Scala.js
- **Native** - Compiled native executables via Scala Native

The template includes platform-specific code examples, proper build configuration, and publishing setup for Maven Central.

## Quick Start

### Prerequisites

- JDK 11 or higher
- sbt 1.11.4 or higher
- Node.js (for JavaScript platform)
- LLVM/Clang (for Native platform)

### Clone and Run

```bash
# Clone or download this template
git clone https://github.com/edadma/cross_template.git
cd cross_template

# Run on JVM
sbt cross_templateJVM/run

# Run on JavaScript (Node.js)
sbt cross_templateJS/run

# Run on Native
sbt cross_templateNative/run
```

Each command will output something like:
```
Hello world - jvm
Hello world - js  
Hello world - native
```

## Project Structure

```
cross_template/
├── shared/                    # Shared code across all platforms
│   └── src/
│       ├── main/scala/        # Main shared source code
│       └── test/scala/        # Shared test code
├── jvm/                       # JVM-specific code
│   └── src/main/scala/
├── js/                        # JavaScript-specific code  
│   └── src/main/scala/
├── native/                    # Native-specific code
│   └── src/main/scala/
├── project/                   # sbt build configuration
│   ├── build.properties
│   └── plugins.sbt
└── build.sbt                  # Main build configuration
```

## Building and Testing

```bash
# Compile all platforms
sbt compile

# Run tests on all platforms
sbt test

# Run tests on specific platform
sbt cross_templateJVM/test
sbt cross_templateJS/test
sbt cross_templateNative/test

# Create JARs
sbt package

# Create optimized JS bundle
sbt cross_templateJS/fastOptJS

# Create native executable
sbt cross_templateNative/nativeLink
```

## Customizing the Template

### 1. Update Project Information

Edit `build.sbt` to change:
- `name` - your project name
- `organization` - your organization/group ID
- `version` - your project version
- `ThisBuild / homepage` - your project homepage
- `ThisBuild / scmInfo` - your Git repository info
- `ThisBuild / developers` - your information

### 2. Add Dependencies

Add libraries to the `libraryDependencies` sections in `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "org.typelevel" %%% "cats-core" % "2.10.0",
  "io.circe" %%% "circe-core" % "0.14.6"
)
```

Use `%%%` for cross-platform dependencies and `%%` for platform-specific ones.

### 3. Platform-Specific Code

Add platform-specific implementations in the respective platform directories:

- `jvm/src/main/scala/` - JVM-specific code (file I/O, database connections, etc.)
- `js/src/main/scala/` - JavaScript-specific code (DOM manipulation, browser APIs, etc.)
- `native/src/main/scala/` - Native-specific code (system calls, C interop, etc.)

### 4. Update Package Structure

Change the package name from `io.github.edadma.cross_template` to your desired package structure throughout the source files.

## Publishing

The template is configured for publishing to Maven Central via Sonatype. To publish:

1. Set up your Sonatype credentials
2. Configure PGP signing
3. Run:

```bash
sbt publishSigned
sbt sonatypeBundleRelease
```

## Platform-Specific Notes

### JavaScript (Scala.js)
- Configured for ES modules output
- Node.js environment for testing
- Source maps disabled for smaller bundles

### Native (Scala Native)
- Includes `scala-java-time` for date/time operations
- Optimized for executable generation

### JVM
- Standard JVM configuration
- Compatible with Java 11+

## Examples

### Adding a New Module

Create platform-specific implementations:

```scala
// shared/src/main/scala/yourpackage/Utils.scala
trait Utils {
  def platformInfo: String
}

// jvm/src/main/scala/yourpackage/Utils.scala  
object Utils extends Utils {
  def platformInfo = s"Running on JVM ${System.getProperty("java.version")}"
}

// js/src/main/scala/yourpackage/Utils.scala
import scala.scalajs.js
object Utils extends Utils {
  def platformInfo = s"Running on JS ${js.Dynamic.global.process.version}"
}
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass: `sbt test`
6. Submit a pull request

## License

This project is licensed under the ISC License - see the [LICENSE](LICENSE) file for details.
