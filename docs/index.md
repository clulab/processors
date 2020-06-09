---
title: Overview
has_children: false
nav_order: 1
---

# What is it?

This is the main public code repository of the Computational Language Understanding (CLU) Lab at [University of Arizona](http://www.arizona.edu). 

This repository contains:

+ A rule-based event extraction (EE) framework called Odin (Open Domain INformer) in the `org.clulab.odin` package. See the Odin section for more details.
+ Two full-fledged Rhetorical Structure Theory (RST) discourse parsers. The discourse parsers are transparently included in our natural language (NL) processors (see below). The version in `CoreNLPProcessor` relies on constituent syntax, whereas the one in `FastNLPProcessor` uses dependency syntax. They perform approximately the same, but the latter is much faster.
+ A multi-task learning framework for deep learning and sequence modeling called [Metal](https://github.com/clulab/processors/wiki/Metal), which is implemented on top of [DyNet](https://dynet.readthedocs.io/en/latest/). This framework includes a simple domain-specific language (DSL) that allows you to ramp up sequence models very quickly without writing any Scala code. We use `Metal` to implement most of the components in `CluProcessor`. 
+ A machine learning (ML) package (`org.clulab.learning`), which includes implementations for common ML algorithms (e.g., Perceptron, Logistic Regression, Support Vector Machines, Random Forests) for both classification and ranking.
+ A suite of NL processors in the `org.clulab.processors` package. We currently provide the following APIs: 
	+ `CoreNLPProcessor` - a wrapper for [Stanford's CoreNLP](http://nlp.stanford.edu/software/corenlp.shtml), using its constituent parser;
	+ `FastNLPProcessor` - a wrapper for Stanford's CoreNLP, but using its neural-network dependency parser;
	+ `CluProcessor` - an in-house processor (licensed under the Apache license) that contains: tokenization (using [Antlr](http://www.antlr.org)), lemmatization (using [MorphaStemmer](https://search.maven.org/#artifactdetails%7Cedu.washington.cs.knowitall.nlptools%7Cnlptools-stem-morpha_2.10%7C2.4.5%7Cjar)), POS tagging, named entity recognition (NER), and shallow syntactic parsing or chunking, and semantic role labeling. The last four components are implemented using `Metal`, our multi-task learning framework. 

# Authors

[Mihai Surdeanu](http://surdeanu.info/mihai/), [Marco Valenzuela](https://github.com/marcovzla), [Gustave Hahn-Powell](https://github.com/myedibleenso), Peter Jansen, [Daniel Fried](http://www.cs.arizona.edu/~dfried/), Dane Bell, [Keith Alcock](http://www.keithalcock.com), and Tom Hicks.

# License

Our code is licensed as follows:
+ **`main, odin`** - Apache License Version 2.0. Please note that these subprojects do not interact with the `corenlp` subproject below.
+ **`corenlp`** - GLP Version 3 or higher, due to the dependency on [Stanford's CoreNLP](http://stanfordnlp.github.io/CoreNLP/). If you use only `CluProcessor`, this dependency does not have to be included in your project.


# Citations

If you use one of our discourse parsers, please cite this paper:

Mihai Surdeanu, Thomas Hicks, and Marco A. Valenzuela-Escarcega. Two Practical Rhetorical Structure Theory Parsers. In *Proceedings of the Conference of the North American Chapter of the Association for Computational Linguistics - Human Language Technologies: Software Demonstrations (NAACL HLT)*, 2015. [[pdf]](http://surdeanu.info/mihai/papers/naacl2015-discourse.pdf) [[bib]](http://surdeanu.info/mihai/papers/naacl2015-discourse.bib)

If you use Odin, our event extraction framework, please cite this paper:

Marco A. Valenzuela-Escarcega, Gustave Hahn-Powell, Thomas Hicks, and Mihai Surdeanu. A Domain-independent Rule-based Framework for Event Extraction. In *Proceedings of the 53rd Annual Meeting of the Association for Computational Linguistics and the 7th International Joint Conference on Natural Language Processing of the Asian Federation of Natural Language Processing: Software Demonstrations (ACL-IJCNLP)*, 2015. [[pdf]](http://surdeanu.info/mihai/papers/acl2015.pdf) [[bib]](http://surdeanu.info/mihai/papers/acl2015.bib)

If you use `CoreNLPProcessor`, please cite Stanford's paper:

Christopher D. Manning, Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and David McClosky. The Stanford CoreNLP Natural Language Processing Toolkit. In *Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics (ACL)*, 2014. [[pdf]](http://surdeanu.info/mihai/papers/acl2014-corenlp.pdf) [[bib]](http://surdeanu.info/mihai/papers/acl2014-corenlp.bib)

If you use `CluProcessor`, please cite this paper:

Mihai Surdeanu and Christopher D. Manning. Ensemble Models for Dependency Parsing: Cheap and Good? In *Proceedings of the North American Chapter of the Association for Computational Linguistics Conference (NAACL-2010)*, 2010. [[pdf]](http://surdeanu.info/mihai/papers/naacl10-parsing.pdf) [[bib]](http://surdeanu.info/mihai/papers/naacl10-parsing-surdeanu.bib)

If you use anything else in this package, please link to this page.


