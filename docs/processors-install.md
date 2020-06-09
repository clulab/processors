---
title: Installation
parent: Processors
has_children: true
nav_order: 1
---

# Installation

This software requires Java 1.8 and Scala 2.11 or higher.

The bulk of this software is available on Maven Central. To use, simply add the dependencies below to your `pom.xml` (please replace `x.x.x` with an actual version number; the latest stable version is `8.1.0`).  One component is not available at Maven Central because of size limitations there.  However, Maven seems to fetch this transitive dependency, [processors-models](http://artifactory.cs.arizona.edu:8081/artifactory/webapp/#/artifacts/browse/tree/General/sbt-release/org/clulab/processors-models), automatically.

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
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors-odin_2.12</artifactId>
   <version>x.x.x</version>
</dependency>
```

SBT is not able to fetch the dependency without further configuration.  An additional resolver must be provided, but thereafter the procedure is the same.  The equivalent SBT dependencies are:

```scala
resolvers += "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release"

libraryDependencies ++= {
  val procVer = "x.x.x"

  Seq(
    "org.clulab" %% "processors-main" % procVer,
    "org.clulab" %% "processors-corenlp" % procVer,
    "org.clulab" %% "processors-odin" % procVer
  )
}
```

# External Dependencies

Most of the `processors` dependencies are captured in the build file. However, a few `processors` unit tests depend also on `svm-rank`, which should be installed separately. Simply installing the `svm-rank` binaries to `/usr/local/bin` (or another generic location on your OS) solves the problem:

https://www.cs.cornell.edu/people/tj/svm_light/svm_rank.html

## Installing external dependencies on Mac OS X via `homebrew`

```bash
brew tap myedibleenso/nlp
brew install svmlight svmrank
```

## Skipping tests involving external dependencies

Alternatively, you can run just the unit tests that do not require external binaries with the following command:

`sbt 'test-only -- -l NeedsExternalBinary'`

# How to compile the source code

This is a standard sbt project, so use the usual commands, e.g., `sbt compile`, `sbt assembly`, to compile.
Add the generated jar files under `target/` to your $CLASSPATH, along with the other necessary dependency jars. Take a look at `build.sbt` to see which dependencies are necessary at runtime.


