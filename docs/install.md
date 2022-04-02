---
title: Installation
has_children: false
nav_order: 2
---

# Installation

This software requires `Java` 8 or 11 in order to run as intended and `Scala` [2.11](https://www.scala-lang.org/download/2.11.12.html) or [2.12](https://www.scala-lang.org/download/2.12.15.html) along with Java in order to recompile it.  `Scala` is often installed as part of [sbt](https://www.scala-sbt.org/download.html), the Scala Build Tool, which one would need to build the project or incorporate it into a different `Scala` project.  `sbt` will be configured to use a compatible version of `Scala` automatically.

Beyond that, the bulk of this software is available in compiled jars on [Maven Central](https://search.maven.org/search?q=g:org.clulab%20a:processors*) and in source code at [GitHub](https://github.com/clulab/processors).

## Use with Maven

To use `processors` with [Maven](https://maven.apache.org/index.html), typically to build a Java project, simply add the dependencies below to your `pom.xml` file.  Replace `x.x.x` with an actual version number; the latest stable version is `8.4.8`.

Please note that some of the transitive dependencies for this project, including [processors-models](http://artifactory.cs.arizona.edu:8081/artifactory/webapp/#/artifacts/browse/tree/General/sbt-release/org/clulab/processors-models), are not available at Maven Central because of size limitations there. However, all but quite recent versions of Maven fetch these transitive dependencies automatically, so no additional configuration is usually needed. All you need are these:

```xml
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors-corenlp_2.12</artifactId>
   <version>x.x.x</version>
</dependency>
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors-main_2.12</artifactId>
   <version>x.x.x</version>
</dependency>
```

However, some versions of Maven will not fetch dependencies over HTTP connections.  They must be coaxed into doing so by editing [settings.xml](https://maven.apache.org/settings.html).  Specifically, this mirror needs to be added:

```xml
<mirror>
  <id>artifactory.cs.arizona.edu-http-unblocker</id>
  <mirrorOf>Artifactory</mirrorOf>
  <name></name>
  <url>http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release</url>
</mirror>
```

## Use with `sbt`

`sbt` does not follow transitive dependencies to non-standard repositories, so it does not fetch [processors-models](http://artifactory.cs.arizona.edu:8081/artifactory/webapp/#/artifacts/browse/tree/General/sbt-release/org/clulab/processors-models) without further configuration.  An additional resolver must be provided, but thereafter the procedure is the same.  The equivalent `sbt` dependencies are

```scala
resolvers += "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release"

libraryDependencies ++= {
  val procVer = "x.x.x"

  Seq(
    "org.clulab" %% "processors-main" % procVer,
    "org.clulab" %% "processors-corenlp" % procVer
  )
}
```
Some newer versions of `sbt` do not allow resolvers to use unencrypted HTTP connections.  If `sbt` complains about the download from the Artifactory server, please change the one line to
```scala
resolvers += ("Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release").withAllowInsecureProtocol(true)
```

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
