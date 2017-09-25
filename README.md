[![Build Status](https://travis-ci.org/clulab/processors.svg?branch=master)](https://travis-ci.org/clulab/processors)

# What is it?

This is the main public code repository of the Computational Language Understanding (CLU) Lab led by [Mihai Surdeanu](http://surdeanu.info/mihai/) at [University of Arizona](http://www.arizona.edu). This repository contains:

+ A rule-based event extraction (EE) framework called Odin (Open Domain INformer) in the `org.clulab.odin` package. See [Odin's Wiki page](https://github.com/clulab/processors/wiki/ODIN-(Open-Domain-INformer)) for more details.
+ Two full-fledged Rhetorical Structure Theory (RST) discourse parsers. The discourse parsers are transparently included in our natural language (NL) processors (see below). The version in `CoreNLPProcessor` relies on constituent syntax, whereas the one in `FastNLPProcessor` uses dependency syntax. They perform approximately the same, but the latter is much faster. 
+ A machine learning (ML) package (`org.clulab.learning`), which includes implementations for common ML algorithms (e.g., Perceptron, Logistic Regression, Support Vector Machines, Random Forests) for both classification and ranking.
+ A suite of NL processors in the `org.clulab.processors` package. We currently provide the following APIs: 
	+ `CoreNLPProcessor` - a wrapper for [Stanford's CoreNLP](http://nlp.stanford.edu/software/corenlp.shtml), using its constituent parser;
	+ `FastNLPProcessor` - a wrapper for Stanford's CoreNLP, but using its neural-network dependency parser;
	+ `BioNLPProcessor` - a version of `CoreNLPProcessor` tuned for the biomedical domain: better tokenization for biomedical texts, improved POS tagging for the bio domain, and a custom NER for this domain that recognizes entities relevant in this domain such as proteins, chemical, and biological processes;
	+ `FastBioNLPProcessor` - a version of `FastNLPProcessor` tuned for the biomedical domain, similarly to `BioNLPProcessor`; 
	+ `CluProcessor` - an in-house processor (licensed under the Apache license) that contains: tokenization (using [Antlr](http://www.antlr.org)), lemmatization (using [MorphaStemmer](https://search.maven.org/#artifactdetails%7Cedu.washington.cs.knowitall.nlptools%7Cnlptools-stem-morpha_2.10%7C2.4.5%7Cjar)), POS tagging (using an in-house bidirectional maximum entropy Markov model), and syntax (using an ensemble of models built with [maltparser](http://mallet.cs.umass.edu)), which supports both basic and enhanced dependencies. Performance is comparable to `FastNLPProcessor`, under a more permissive license. Additionally, the memory footprint of `CluProcessor` is smaller than that of `FastNLPProcessor`, so it may be more appropriate for older machines.
  + `BioCluProcessor` - a version of `CluProcessor` tuned for the biomedical domain.

This software requires Java 1.8, Scala 2.11, and CoreNLP 3.x or higher.

Our code is licensed as follows:
+ **`main, odin, modelsmain`** - Apache License Version 2.0. Please note that these subprojects do not interact with the `corenlp` subproject below.
+ **`corenlp, modelscorenlp`** - GLP Version 3 or higher, due to the dependency on [Stanford's CoreNLP](http://stanfordnlp.github.io/CoreNLP/). If you use only `CluProcessor`, these dependencies do not have to be included in your project.

(c) Mihai Surdeanu, 2013 -

Authors: [Mihai Surdeanu](http://surdeanu.info/mihai/), [Marco Valenzuela](https://github.com/marcovzla), [Gustave Hahn-Powell](https://github.com/myedibleenso), Peter Jansen, [Daniel Fried](http://www.cs.arizona.edu/~dfried/), Dane Bell, and Tom Hicks.

# Changes
+ [Please see the CHANGES file](CHANGES.md)

# Citations

If you use one of our discourse parsers, please cite this paper:

Mihai Surdeanu, Thomas Hicks, and Marco A. Valenzuela-Escarcega. Two Practical Rhetorical Structure Theory Parsers. In *Proceedings of the Conference of the North American Chapter of the Association for Computational Linguistics - Human Language Technologies: Software Demonstrations (NAACL HLT)*, 2015. [[pdf]](http://surdeanu.info/mihai/papers/naacl2015-discourse.pdf) [[bib]](http://surdeanu.info/mihai/papers/naacl2015-discourse.bib)

If you use Odin, our event extraction framework, please cite this paper:

Marco A. Valenzuela-Escarcega, Gustave Hahn-Powell, Thomas Hicks, and Mihai Surdeanu. A Domain-independent Rule-based Framework for Event Extraction. In *Proceedings of the 53rd Annual Meeting of the Association for Computational Linguistics and the 7th International Joint Conference on Natural Language Processing of the Asian Federation of Natural Language Processing: Software Demonstrations (ACL-IJCNLP)*, 2015. [[pdf]](http://surdeanu.info/mihai/papers/acl2015.pdf) [[bib]](http://surdeanu.info/mihai/papers/acl2015.bib)

If you use `CoreNLPProcessor`, please cite Stanford's paper:

Christopher D. Manning, Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and David McClosky. The Stanford CoreNLP Natural Language Processing Toolkit. In *Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics (ACL)*, 2014. [[pdf]](http://surdeanu.info/mihai/papers/acl2014-corenlp.pdf) [[bib]](http://surdeanu.info/mihai/papers/acl2014-corenlp.bib)

If you use `CluProcessor`, please cite this paper:

Mihai Surdeanu and Christopher D. Manning. Ensemble Models for Dependency Parsing: Cheap and Good? In *Proceedings of the North American Chapter of the Association for Computational Linguistics Conference (NAACL-2010)*, 2010. [[pdf]](http://surdeanu.info/mihai/papers/naacl10-parsing.pdf) [[bib]](http://surdeanu.info/mihai/papers/naacl10-parsing-surdeanu.bib)

If you use anything else in this package, please link to this github page.

# Installation

This software is available on Maven Central. To use, simply add the following dependencies to your `pom.xml`:

```xml
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors-corenlp_2.11</artifactId>
   <version>6.1.3</version>
</dependency>
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors-main_2.11</artifactId>
   <version>6.1.3</version>
</dependency>
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors-odin_2.11</artifactId>
   <version>6.1.3</version>
</dependency>
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors-modelsmain_2.11</artifactId>
   <version>6.1.3</version>
</dependency>
<dependency>
   <groupId>org.clulab</groupId>
   <artifactId>processors-modelscorenlp_2.11</artifactId>
   <version>6.1.3</version>
</dependency>

```

The equivalent SBT dependencies are:

```scala
libraryDependencies ++= {
  val procVer = "6.1.3"

  Seq(
    "org.clulab" %% "processors-main" % procVer,
    "org.clulab" %% "processors-corenlp" % procVer,
    "org.clulab" %% "processors-odin" % procVer,
    "org.clulab" %% "processors-modelsmain" % procVer,
    "org.clulab" %% "processors-modelscorenlp" % procVer,
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

# Why you should use this code
+ **Simple API** - the APIs provided are, at least in my opinion, simpler than those provided for the original code. For example, when using CoreNLP you won't have to deal with hash maps that take class objects as keys. Instead, we use mostly arrays of integers or strings, which are self explanatory.
+ **Memory efficient** - arrays are more memory efficient than hash maps. Furthermore, we used our own implementation to intern strings (i.e., avoiding the storage of duplicated strings multiple times). Due to these changes, I measured up to 99% decrease in memory for the annotations corresponding to a typical natural language text, when compared to the original CoreNLP code. (Note: this reduction takes effect only _after_ CoreNLP finishes its work.)
+ **Faster access** - again, because we use arrays instead of hash maps, access to the NL annotations (once constructed) is considerably faster than in the original Stanford code.
+ **Tool-independent API** - we support multiple NL and machine learning tools. If you use this code, you will have to change your code only minimally (i.e., only the constructor for the `Processor` object).
+ **Discourse parsing** - we include a complete RST parser; simply instantiate `CoreNLPProcessor` with `withDiscourse = true`.
+ **Biomedical tools** - we now include tools to process biomedical texts, which can be used under the same simple interface. We also offer event extraction tools for the biomedical domain.
+ **Rule-based event extraction** - we include Odin, an event extraction framework, which can be customized to various domains.
+ **Apache license** - our `CluProcessor` replicates most functionality of Stanford's CoreNLP (tokenization, POS tagging, dependency parsing, more soon!) under the more permissive Apache license. 

# How to compile the source code

This is a standard sbt project, so use the usual commands, e.g., `sbt compile`, `sbt assembly`, to compile.
Add the generated jar files under `target/` to your $CLASSPATH, along with the other necessary dependency jars. Take a look at `build.sbt` to see which dependencies are necessary at runtime.

# How to use it

## Common scenarios

Most of the examples here use Scala. However, this software can be used as is from Java as well! Scroll down towards the end of this document to see a Java usage example.

### Annotating entire documents

(see [`corenlp/src/main/scala/org/clulab/processors/examples/ProcessorExample.scala`](https://github.com/clulab/processors/blob/master/corenlp/src/main/scala/org/clulab/processors/examples/ProcessorExample.scala) for a complete running example)

```scala
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.struct.DirectedGraphEdgeIterator

// create the processor
// any processor works here! Try FastNLPProcessor, BioNLPProcessor, or our own CluProcessor
// use no arguments in the c'tor if you don't need the discourse parser
val proc:Processor = new CoreNLPProcessor(ShallowNLPProcessor.WITH_DISCOURSE) 

// the actual work is done here
val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")

// you are basically done. the rest of this code simply prints out the annotations

// let's print the sentence-level annotations
var sentenceCount = 0
for (sentence <- doc.sentences) {
  println("Sentence #" + sentenceCount + ":")
  println("Tokens: " + sentence.words.mkString(" "))
  println("Start character offsets: " + sentence.startOffsets.mkString(" "))
  println("End character offsets: " + sentence.endOffsets.mkString(" "))

  // these annotations are optional, so they are stored using Option objects, hence the foreach statement
  sentence.lemmas.foreach(lemmas => println(s"Lemmas: ${lemmas.mkString(" ")}"))
  sentence.tags.foreach(tags => println(s"POS tags: ${tags.mkString(" ")}"))
  sentence.chunks.foreach(chunks => println(s"Chunks: ${chunks.mkString(" ")}"))
  sentence.entities.foreach(entities => println(s"Named entities: ${entities.mkString(" ")}"))
  sentence.norms.foreach(norms => println(s"Normalized entities: ${norms.mkString(" ")}"))
  sentence.dependencies.foreach(dependencies => {
    println("Syntactic dependencies:")
    val iterator = new DirectedGraphEdgeIterator[String](dependencies)
    while(iterator.hasNext) {
      val dep = iterator.next
      // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
      println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
    }
  })
  sentence.syntacticTree.foreach(tree => {
    println("Constituent tree: " + tree)
    // see the org.clulab.utils.Tree class for more information
    // on syntactic trees, including access to head phrases/words
  })

  sentenceCount += 1
  println("\n")
}

// let's print the coreference chains
doc.coreferenceChains.foreach(chains => {
  for (chain <- chains.getChains) {
    println("Found one coreference chain containing the following mentions:")
    for (mention <- chain) {
      // note that all these offsets start at 0 too
      println("\tsentenceIndex:" + mention.sentenceIndex +
        " headIndex:" + mention.headIndex +
        " startTokenOffset:" + mention.startOffset +
        " endTokenOffset:" + mention.endOffset +
        " text: " + doc.sentences(mention.sentenceIndex).words.slice(mention.startOffset, mention.endOffset).mkString("[", " ", "]"))
    }
  }
})

// let's print the discourse tree
doc.discourseTree.foreach(dt => {
  println("Document-wide discourse tree:")
  println(dt.toString())
})
```
The above code generates the following output:

```
Sentence #0:
Tokens: John Smith went to China .
Start character offsets: 0 5 11 16 19 24
End character offsets: 4 10 15 18 24 25
Lemmas: John Smith go to China .
POS tags: NNP NNP VBD TO NNP .
Chunks: B-NP I-NP B-VP B-PP B-NP O
Named entities: PERSON PERSON O O LOCATION O
Normalized entities: O O O O O O
Syntactic dependencies:
  head:1 modifier:0 label:nn
  head:2 modifier:1 label:nsubj
  head:2 modifier:4 label:prep_to
Constituent tree: (ROOT (S (NP (NNP John) (NNP Smith)) (VP (VBD went) (PP (TO to) (NP (NNP China)))) (. .)))


Sentence #1:
Tokens: He visited Beijing , on January 10th , 2013 .
Start character offsets: 26 29 37 44 46 49 57 61 63 67
End character offsets: 28 36 44 45 48 56 61 62 67 68
Lemmas: he visit Beijing , on January 10th , 2013 .
POS tags: PRP VBD NNP , IN NNP JJ , CD .
Chunks: B-NP B-VP B-NP O B-PP B-NP I-NP I-NP I-NP O
Named entities: O O LOCATION O O DATE DATE DATE DATE O
Normalized entities: O O O O O 2013-01-10 2013-01-10 2013-01-10 2013-01-10 O
Syntactic dependencies:
  head:1 modifier:0 label:nsubj
  head:1 modifier:2 label:dobj
  head:1 modifier:8 label:tmod
  head:2 modifier:5 label:prep_on
  head:5 modifier:6 label:amod
Constituent tree: (ROOT (S (NP (PRP He)) (VP (VBD visited) (NP (NP (NNP Beijing)) (, ,) (PP (IN on) (NP (NNP January) (JJ 10th))) (, ,)) (NP-TMP (CD 2013))) (. .)))


Found one coreference chain containing the following mentions:
  sentenceIndex:1 headIndex:2 startTokenOffset:2 endTokenOffset:3 text: [Beijing]
Found one coreference chain containing the following mentions:
  sentenceIndex:1 headIndex:0 startTokenOffset:0 endTokenOffset:1 text: [He]
  sentenceIndex:0 headIndex:1 startTokenOffset:0 endTokenOffset:2 text: [John Smith]
Found one coreference chain containing the following mentions:
  sentenceIndex:1 headIndex:5 startTokenOffset:5 endTokenOffset:9 text: [January 10th , 2013]
Found one coreference chain containing the following mentions:
  sentenceIndex:0 headIndex:4 startTokenOffset:4 endTokenOffset:5 text: [China]

Document-wide discourse tree:
elaboration (LeftToRight)
  TEXT:John Smith went to China .
  TEXT:He visited Beijing , on January 10th , 2013 .
```

For more details about the annotation data structures, please see the `org/clulab/processor/Document.scala` file.

Changing processors is trivial: just replace the first line in the above example with:

```scala
val proc:Processor = new FastNLPProcessor()
// everything else stays the same
```

`FastNLPProcessor` uses the Stanford tokenizer, POS tagger, and NER, but replaces its parser with maltparser, trained to generated Stanford "basic" (rather than "collapsed") dependencies. This means that this annotator does not produce constituent trees and coreference chains. However, because of the faster parser, you should see a speedup increase of at least one order of magnitude. The output of the above code with `FastNLPProcessor` is:

    Sentence #0:
    Tokens: John Smith went to China .
    Start character offsets: 0 5 11 16 19 24
    End character offsets: 4 10 15 18 24 25
    Lemmas: John Smith go to China .
    POS tags: NNP NNP VBD TO NNP .
    Chunks: B-NP I-NP B-VP B-PP B-NP O
    Named entities: PERSON PERSON O O LOCATION O
    Normalized entities: O O O O O O
    Syntactic dependencies:
     head:1 modifier:0 label:nn
     head:2 modifier:1 label:nsubj
     head:2 modifier:3 label:prep
     head:2 modifier:5 label:punct
     head:3 modifier:4 label:pobj


    Sentence #1:
    Tokens: He visited Beijing , on January 10th , 2013 .
    Start character offsets: 26 29 37 44 46 49 57 61 63 67
    End character offsets: 28 36 44 45 48 56 61 62 67 68
    Lemmas: he visit Beijing , on January 10th , 2013 .
    POS tags: PRP VBD NNP , IN NNP JJ , CD .
    Chunks: B-NP B-VP B-NP O B-PP B-NP I-NP I-NP I-NP O
    Named entities: O O LOCATION O O DATE DATE DATE DATE O
    Normalized entities: O O O O O 2013-01-10 2013-01-10 2013-01-10 2013-01-10 O
    Syntactic dependencies:
     head:1 modifier:0 label:nsubj
     head:1 modifier:2 label:dobj
     head:1 modifier:3 label:punct
     head:1 modifier:4 label:prep
     head:1 modifier:9 label:punct
     head:4 modifier:5 label:pobj
     head:5 modifier:6 label:amod
     head:5 modifier:7 label:punct
     head:5 modifier:8 label:num

### Annotating documents already split into sentences

```scala
val doc = proc.annotateFromSentences(List("John Smith went to China.", "He visited Beijing."))
// everything else stays the same
```

### Annotating documents already split into sentences and tokenized

```scala
val doc = annotateFromTokens(List(
  List("John", "Smith", "went", "to", "China", "."),
  List("There", ",", "he", "visited", "Beijing", ".")))
// everything else stays the same
```

## Using individual annotators

You can of course use only some of the annotators provided by CoreNLP by calling them individually. To illustrate,
the `Processor.annotate()` method is implemented as follows:

```scala
def annotate(doc:Document): Document = {
  tagPartsOfSpeech(doc)
  lemmatize(doc)
  recognizeNamedEntities(doc)
  parse(doc)
  chunking(doc)
  labelSemanticRoles(doc)
  resolveCoreference(doc)
  discourse(doc)
  doc.clear()
  doc
}
```

(Note that CoreNLP currently does not support semantic role labeling.)
You can use just a few of these annotators. For example, if you need just POS tags, lemmas and named entities, you could use
the following code:

```scala
val doc = proc.mkDocument("John Smith went to China. He visited Beijing, on January 10th, 2013.")
proc.tagPartsOfSpeech(doc)
proc.lemmatize(doc)
proc.recognizeNamedEntities(doc)
doc.clear()
```

Note that the last method called (`doc.clear()`) clears the internal structures created by the actual CoreNLP annotators.
This saves a lot of memory, so, although it is not strictly necessary, I recommend you call it.

## Serialization

This package also offers serialization code for the generated annotations. The two scenarios currently supported are:

### Serialization to/from streams

```scala
// saving to a PrintWriter, pw
val someAnnotation = proc.annotate(someText)
val serializer = new DocumentSerializer
serializer.save(someAnnotation, pw)

// loading from an BufferedReader, br
val someAnnotation = serializer.load(br)
```

### Serialization to/from Strings

```scala
// saving to a String object, savedString
val someAnnotation = proc.annotate(someText)
val serializer = new DocumentSerializer
val savedString = serializer.save(someAnnotation)

// loading from a String object, fromString
val someAnnotation = serializer.load(fromString)
```

Note that space required for these serialized annotations is considerably smaller (8 to 10 times) than the corresponding
serialized Java objects. This is because we store only the information required to recreate these annotations (e.g., words, lemmas, etc.)
without storing any of the Java/Scala objects and classes.

### Serialization to/from `json`

As of v5.9.6, `Document` and `Mention` instances can be serialized to/from `json` ([see the complete working example](https://gist.github.com/myedibleenso/87a3191c73938840b8ed768ec305db38)).  

## Cleaning up the interned strings

Classes that implement the `Processor` trait intern String objects to avoid allocating memory repeatedly for the same string.
This is implemented by maintaining an internal dictionary of strings previously seen. This dictionary is unlikely to
use a lot of memory due to the Zipfian distribution of language. But, if memory usage is a big concern, it can be cleaned by
calling:

```scala
Processor.in.clear()
```

I recommend you do this only _after_ you annotated all the documents you plan to keep in memory.

Although I see no good reason for doing this, you can disable the interning of strings completely by setting the `internStrings = false` in the
CoreNLProcessor constructor, as such:

```scala
val processor = new CoreNLPProcessor(internStrings = false)
```

## Using processors from Java

Scala is (largely) compatible with Java, so this library can be directly used from Java. Below is Java code that translates most of the functionality from the first Scala example in this document to Java:

```java
package org.clulab.processors;


import org.clulab.discourse.rstparser.DiscourseTree;
import org.clulab.processors.fastnlp.FastNLPProcessor;
import org.clulab.processors.corenlp.CoreNLPProcessor;
import org.clulab.struct.CorefMention;
import org.clulab.struct.DirectedGraphEdgeIterator;

public class ProcessorsJavaExample {
    public static void main(String [] args) throws Exception {
        // create the processor
        Processor proc = new CoreNLPProcessor(true, false, 0, 100);
        // for much faster processing, use FastNLPProcessor
        // Processor proc = new FastNLPProcessor(true, false);

        // the actual work is done here
        Document doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.", false);

        // you are basically done. the rest of this code simply prints out the annotations

        // let's print the sentence-level annotations
        int sentenceCount = 0;
        for (Sentence sentence: doc.sentences()) {
            System.out.println("Sentence #" + sentenceCount + ":");
            System.out.println("Tokens: " + mkString(sentence.words(), " "));
            System.out.println("Start character offsets: " + mkString(sentence.startOffsets(), " "));
            System.out.println("End character offsets: " + mkString(sentence.endOffsets(), " "));

            // these annotations are optional, so they are stored using Option objects, hence the isDefined checks
            if(sentence.lemmas().isDefined()){
                System.out.println("Lemmas: " + mkString(sentence.lemmas().get(), " "));
            }
            if(sentence.tags().isDefined()){
                System.out.println("POS tags: " + mkString(sentence.tags().get(), " "));
            }
            if(sentence.chunks().isDefined()){
                System.out.println("Chunks: " + mkString(sentence.chunks().get(), " "));
            }
            if(sentence.entities().isDefined()){
                System.out.println("Named entities: " + mkString(sentence.entities().get(), " "));
            }
            if(sentence.norms().isDefined()){
                System.out.println("Normalized entities: " + mkString(sentence.norms().get(), " "));
            }
            if(sentence.dependencies().isDefined()) {
                System.out.println("Syntactic dependencies:");
                DirectedGraphEdgeIterator<String> iterator = new
                    DirectedGraphEdgeIterator<String>(sentence.dependencies().get());
                while(iterator.hasNext()) {
                    scala.Tuple3<Object, Object, String> dep = iterator.next();
                    // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
                    System.out.println(" head:" + dep._1() + " modifier:" + dep._2() + " label:" + dep._3());
                }
            }
            if(sentence.syntacticTree().isDefined()) {
                System.out.println("Constituent tree: " + sentence.syntacticTree().get());
                // see the org.clulab.struct.Tree class for more information
                // on syntactic trees, including access to head phrases/words
            }

            sentenceCount += 1;
            System.out.println("\n");
        }

        // let's print the coreference chains
        if(doc.coreferenceChains().isDefined()) {
            // these are scala.collection Iterator and Iterable (not Java!)
            scala.collection.Iterator<scala.collection.Iterable<CorefMention>> chains = doc.coreferenceChains().get().getChains().iterator();
            while(chains.hasNext()) {
                scala.collection.Iterator<CorefMention> chain = chains.next().iterator();
                System.out.println("Found one coreference chain containing the following mentions:");
                while(chain.hasNext()) {
                    CorefMention mention = chain.next();
                    // note that all these offsets start at 0 too
                    System.out.println("\tsentenceIndex:" + mention.sentenceIndex() +
                        " headIndex:" + mention.headIndex() +
                        " startTokenOffset:" + mention.startOffset() +
                        " endTokenOffset:" + mention.endOffset());
                }
            }
        }
	
        // let's print the discourse tree
        if(doc.discourseTree().isDefined()) {
            DiscourseTree tree = doc.discourseTree().get();
            System.out.println("Discourse tree:\n" + tree);
        }
    }

    public static String mkString(String [] sa, String sep) {
        StringBuilder os = new StringBuilder();
        for(int i = 0; i < sa.length; i ++) {
            if(i > 0) os.append(sep);
            os.append(sa[i]);
        }
        return os.toString();
    }
    public static String mkString(int [] sa, String sep) {
        StringBuilder os = new StringBuilder();
        for(int i = 0; i < sa.length; i ++) {
            if(i > 0) os.append(sep);
            os.append(Integer.toString(sa[i]));
        }
        return os.toString();
    }
}
```

The output of this code is:

    Sentence #0:
    Tokens: John Smith went to China .
    Start character offsets: 0 5 11 16 19 24
    End character offsets: 4 10 15 18 24 25
    Lemmas: John Smith go to China .
    POS tags: NNP NNP VBD TO NNP .
    Chunks: B-NP I-NP B-VP B-PP B-NP O
    Named entities: PERSON PERSON O O LOCATION O
    Normalized entities: O O O O O O
    Syntactic dependencies:
     head:1 modifier:0 label:nn
     head:2 modifier:1 label:nsubj
     head:2 modifier:4 label:prep_to
    Constituent tree:
    (ROOT
        (S
            (NP
                (NNP John)
                (NNP Smith)
            )
            (VP
                (VBD went)
                (PP
                    (TO to)
                    (NP
                        (NNP China)
                    )
                )
            )
            (. .)
        )
    )


    Sentence #1:
    Tokens: He visited Beijing , on January 10th , 2013 .
    Start character offsets: 26 29 37 44 46 49 57 61 63 67
    End character offsets: 28 36 44 45 48 56 61 62 67 68
    Lemmas: he visit Beijing , on January 10th , 2013 .
    POS tags: PRP VBD NNP , IN NNP JJ , CD .
    Chunks: B-NP B-VP B-NP O B-PP B-NP I-NP I-NP I-NP O
    Named entities: O O LOCATION O O DATE DATE DATE DATE O
    Normalized entities: O O O O O 2013-01-10 2013-01-10 2013-01-10 2013-01-10 O
    Syntactic dependencies:
     head:1 modifier:0 label:nsubj
     head:1 modifier:2 label:dobj
     head:1 modifier:8 label:tmod
     head:2 modifier:5 label:prep_on
     head:5 modifier:6 label:amod
    Constituent tree:
    (ROOT
        (S
            (NP
                (PRP He)
            )
            (VP
                (VBD visited)
                (NP
                    (NP
                        (NNP Beijing)
                    )
                    (, ,)
                    (PP
                        (IN on)
                        (NP
                            (NNP January)
                            (JJ 10th)
                        )
                    )
                    (, ,)
                )
                (NP-TMP
                    (CD 2013)
                )
            )
            (. .)
        )
    )


    Found one coreference chain containing the following mentions:
		sentenceIndex:1 headIndex:2 startTokenOffset:2 endTokenOffset:7
    Found one coreference chain containing the following mentions:
		sentenceIndex:0 headIndex:1 startTokenOffset:0 endTokenOffset:2
		sentenceIndex:1 headIndex:0 startTokenOffset:0 endTokenOffset:1
    Found one coreference chain containing the following mentions:
		sentenceIndex:1 headIndex:5 startTokenOffset:5 endTokenOffset:7
    Found one coreference chain containing the following mentions:
		sentenceIndex:0 headIndex:4 startTokenOffset:4 endTokenOffset:5
    Found one coreference chain containing the following mentions:
		sentenceIndex:1 headIndex:8 startTokenOffset:8 endTokenOffset:9
    Discourse tree:
    elaboration (LeftToRight)
    	TEXT:John Smith went to China .
        TEXT:He visited Beijing , on January 10th , 2013 .

## The discourse parser

The discourse parser in `processors` is inspired by the parser of [Feng and Hirst](http://www.cs.toronto.edu/~weifeng/software.html) and the HILDA parser of [Hernault et al.](http://elanguage.net/journals/dad/article/view/591), but with a different feature set.
It is transparently integrated in both `CoreNLPProcessor` and `FastNLPProcessor`: just instantiate it as `CoreNLPProcessor(withDiscourse = true)` or `FastNLPProcessor(withDiscourse = true)`. If discourse is enabled, `Document.discourseTree` stores the discourse tree for the entire document as an instance of the `DiscourseTree` class.

Following the conventions from other modern discourse parsing work, the discourse tree:
+ Is represented as a binary tree, containing hypotactic relations (containing one nucleus and one satellite node) or paratactic relations (both nodes have equal importance).
+ Stores the relation labels in the parent node (in `DiscourseTree.relationLabel`) rather than the satellite nodes (like the RST corpus). We use the same 18 labels as Feng and Hirst.
+ Stores the relation direction in `DiscourseTree.relationDirection`. The direction can be `LeftToRight` (meaning the nucleus is the left child), `RightToLeft` (the right node is the nucleus), or `None` (for paratactic relations).

Developers only: For more details on the discourse parsers, please see [this Wiki page](https://github.com/clulab/processors/wiki/Discourse-Parsers-Details).

## The `org.clulab.learning` package

`processors` now contains a machine learning (ML) package (`org.clulab.learning`), which includes implementations for common ML algorithms (e.g., Perceptron, Logistic Regression, Support Vector Machines, Random Forests) for both classification and ranking.

The structure of this package is heavily inspired by Stanford's CoreNLP. Similar to CoreNLP, we use a `Datum` trait to store a single data point, which is implemented by `BVFDatum` to store binary-valued-feature datums, or by `RVFDatum` to store real-valued-feature datums. A collection of data points is stored as a `Dataset`, which is similarly implemented by `BVFDataset` or `RVFDataset`. All classifiers implement the `Classifier` trait, which has three main methods: `train`, which trains a model a given dataset, `classOf`, which returns the most likely prediction for a given datum, and `scoresOf`, which returns the scores for all known labels for a given datum. We currently support the following classifiers: large-margin Perceptron (`PerceptronClassifier`), linear SVMs and logistic regression from [liblinear](http://www.csie.ntu.edu.tw/~cjlin/liblinear/) (`LibLinearClassifier`), dual SVMs from [libsvm](http://www.csie.ntu.edu.tw/~cjlin/libsvm/) (`LibSVMClassifier`), and random forests, implemented in-house (`RFClassifier`).

A similar structure exists for ranking problems, with `RankingDataset` used to store a corpus of ranking examples, and  `RankingClassifier` as the API to be implemented by all ranking classifiers. We currently support the following classifiers: ranking Perceptron (`PerceptronRankingClassifier`), and ranking SVMs from [svm-rank](http://www.cs.cornell.edu/people/tj/svm_light/svm_rank.html) (`SVMRankingClassifier`).

For usage examples, including how to create datums and datasets from scratch or import them from the svm-light format, please take a look at the examples under `src/test/scala/org/clulab/learning`.

## The Odin event extraction framework

Please see [Odin's Wiki](https://github.com/clulab/processors/wiki/ODIN-(Open-Domain-INformer)) page for details.

## Other resources

+ [Odin online demo](http://agathon.sista.arizona.edu:8080/odinweb/open)
+ See our [Reach project](https://github.com/clulab/reach) for more applications of Odin and demos
