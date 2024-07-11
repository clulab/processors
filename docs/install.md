---
title: Installation
has_children: false
nav_order: 2
---

# Installation

This software requires `Java` larger than 8 and `Scala` larger than 2.11.  `Scala` is often installed as part of [sbt](https://www.scala-sbt.org/download.html), the Scala Build Tool, which one would need to build the project or incorporate it into a different `Scala` project.  `sbt` will be configured to use a compatible version of `Scala` automatically.

Beyond that, the bulk of this software is available in compiled jars on [Maven Central](https://search.maven.org/search?q=g:org.clulab%20a:processors*) and in source code at [GitHub](https://github.com/clulab/processors).

## Use with Maven

To use `processors` with [Maven](https://maven.apache.org/index.html), typically to build a Java project, simply add the dependencies below to your `pom.xml` file.  Replace `x.x.x` with an actual version number; the latest stable version is `10.0.0` for `processors` and `0.3.1` for `processors-model`.

```xml
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors</artifactId>
   <version>x.x.x</version>
</dependency>
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors-model</artifactId>
   <version>y.y.y</version>
</dependency>
```

## Use with `sbt`

The equivalent `sbt` dependencies are

```scala
libraryDependencies ++= {
  val procVer = "x.x.x"
  val procModelVer = "y.y.y"

  Seq(
    "org.clulab" %% "processors" % procVer,
    "org.clulab" %% "processors-model" % procModelVer
  )
}

## External Binaries

Most `processors` dependencies are captured in the `build.sbt` files. However, a few unit tests also depend on the [svm-rank binaries](https://www.cs.cornell.edu/people/tj/svm_light/svm_rank.html), which should be installed separately. Simply installing the `svm-rank` binaries to `/usr/local/bin` (or another generic location in your path) solves the problem.

### Installing on Mac OS X via `homebrew`

On Mac OS X, you can also install these external binaries using `brew`:

```bash
brew tap myedibleenso/nlp
brew install svmlight svmrank
```

### Skipping tests involving external binaries

Alternatively, when using `sbt` you can run just the unit tests that do not require external binaries with the following command:

```shell
sbt "test-only -- -l NeedsExternalBinary"
```

## Compiling the Source Code

The source code is an `sbt` project, so use the usual commands, e.g., `sbt compile` to compile.
