---
title: Installation
has_children: false
nav_order: 2
---

# Installation

This software requires Java 1.8 and Scala 2.11 or higher.

The bulk of this software is available on Maven Central. To use, simply add the dependencies below to your `pom.xml` (please replace `x.x.x` with an actual version number; the latest stable version is `8.3.5`).  

Please note that one of the transitive dependencies for this project, [processors-models](http://artifactory.cs.arizona.edu:8081/artifactory/webapp/#/artifacts/browse/tree/General/sbt-release/org/clulab/processors-models), is not available at Maven Central because of size limitations there. However, Maven seems to fetch this transitive dependency automatically, so no additional configuration is needed. All you need are these dependencies:

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

SBT is not able to fetch the above transitive dependency without further configuration.  An additional resolver must be provided, but thereafter the procedure is the same.  The equivalent SBT dependencies are:

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

## External Binaries

Most of the `processors` dependencies are captured in the build file. However, a few `processors` unit tests depend also on the `svm-rank` binary, which should be installed separately. Simply installing the `svm-rank` binaries to `/usr/local/bin` (or another generic location in your path) solves the problem:

https://www.cs.cornell.edu/people/tj/svm_light/svm_rank.html

### Installing external dependencies on Mac OS X via `homebrew`

On Mac OS X, you can also install these external binaries using `brew`:

```bash
brew tap myedibleenso/nlp
brew install svmlight svmrank
```

### Skipping tests involving external binaries

Or, alternatively, you can run just the unit tests that do not require external binaries with the following command:

`sbt 'test-only -- -l NeedsExternalBinary'`

## How to compile the source code

This is a `sbt` project, so use the usual commands, e.g., `sbt compile`, to compile.


