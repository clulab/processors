[![Build Status](http://jenkins.cs.arizona.edu:8090/buildStatus/icon?job=processors%2Fmaster)](http://jenkins.cs.arizona.edu:8090/job/processors) 
[![Maven Central](https://img.shields.io/maven-central/v/org.clulab/processors-main_2.12)](https://mvnrepository.com/artifact/org.clulab/processors-main)

# What is it?

This is the main public code repository of the [Computational Language Understanding (CLU) Lab](http://clulab.org) at [University of Arizona](http://www.arizona.edu). Please see [http://clulab.github.io/processors/](http://clulab.github.io/processors/) for more information about this software, installation and usage instructions.

# Changes

+ [Please see the CHANGES file](CHANGES.md)

# License

Our code is licensed as follows:
+ **`main, odin, openie`** - Apache License Version 2.0. Please note that these subprojects do not interact with the `corenlp` subproject below.
+ **`corenlp`** - GPL Version 3 or higher, due to the dependency on [Stanford's CoreNLP](http://stanfordnlp.github.io/CoreNLP/). If you use only `CluProcessor`, this dependency does not have to be included in your project.

