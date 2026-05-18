---
title: Install
summary: Prerequisites and how to get the toolchain.
weight: 10
---

## Prerequisites

- **JVM 17 or newer.** Needed to run the CLI itself. Any distribution works (Temurin, GraalVM, Zulu, Oracle).
- **sbt 1.9 or newer.** Used as the build and runner front-end. Install via [SDKMAN](https://sdkman.io/) (`sdk install sbt`) or [Homebrew](https://brew.sh/) (`brew install sbt`).
- **clang on `$PATH`.** Required only by `nex compile` (the AOT path). The interpreter (`nex run`, `nex test`) doesn't need it. macOS ships clang via the Xcode Command Line Tools (`xcode-select --install`); Linux distros bundle clang in their package manager (`apt install clang`, `dnf install clang`, etc.).

Verify each is installed:

```bash
java   -version   # 17+
sbt    --version  # 1.9+
clang  --version  # any recent
```

## Get the source

There is no published binary yet. Clone the repository and build from source:

```bash
git clone https://github.com/edadma/nex.git
cd nex
sbt nexJVM/compile     # first build pulls dependencies; subsequent runs are fast
```

The first `sbt` invocation downloads dependencies (several minutes). After that the build is incremental and quick.

## Supported targets

The AOT compile path is verified end-to-end on **Mac arm64**. Linux x86_64 and Linux aarch64 should work — the codegen emits portable LLVM IR and shells out to whatever clang finds — but neither is exercised on every commit. Report any platform-specific issues.

The interpreter (`nex run`, `nex test`) runs anywhere the JVM does, including Windows.

## Next step

Once the build succeeds: [run your first program](/getting-started/02-first-program/).
