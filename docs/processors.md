---
title: Processors
has_children: true
nav_order: 2
---

# Processors

A core part of this library is a suite of natural language processors that include tokenization, part-of-speech tagging, named entity recognition, syntactic parsing, semantic role labeling, and discourse parsing.

We include a wrapper for [Stanford's CoreNLP](http://nlp.stanford.edu/software/corenlp.shtml) as well as a toolkit built in house. 
We currently provide the following APIs: 
	+ `CoreNLPProcessor` - a wrapper for Stanford's CoreNLP, using its constituent parser;
	+ `FastNLPProcessor` - a wrapper for Stanford's CoreNLP, but using its neural-network dependency parser;
	+ `CluProcessor` - an in-house processor (licensed under the Apache license) that contains: tokenization (using [Antlr](http://www.antlr.org)), lemmatization (using [MorphaStemmer](https://search.maven.org/#artifactdetails%7Cedu.washington.cs.knowitall.nlptools%7Cnlptools-stem-morpha_2.10%7C2.4.5%7Cjar)), POS tagging, named entity recognition (NER), and shallow syntactic parsing or chunking, and semantic role labeling. The last four components are implemented using `Metal`, our multi-task learning framework. 


