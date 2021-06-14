---
title: Processors
has_children: false
nav_order: 3
---

# Processors

This library contains a suite of natural language processors that include tokenization, part-of-speech tagging, named entity recognition, syntactic parsing, and semantic role labeling.

We include a wrapper for [Stanford's CoreNLP](http://nlp.stanford.edu/software/corenlp.shtml) as well as a toolkit built in house. We currently provide the following APIs: 

* `CoreNLPProcessor` - a wrapper for Stanford's CoreNLP, using its constituent parser;
* `FastNLPProcessor` - a wrapper for Stanford's CoreNLP, but using its neural-network dependency parser;
* `CluProcessor` - an in-house processor (licensed under the Apache license) that contains: tokenization (using [Antlr](http://www.antlr.org)), lemmatization (using [MorphaStemmer](https://search.maven.org/#artifactdetails%7Cedu.washington.cs.knowitall.nlptools%7Cnlptools-stem-morpha_2.10%7C2.4.5%7Cjar)), POS tagging, named entity recognition (NER), and shallow syntactic parsing or chunking, and semantic role labeling. The last four components are implemented using `Metal`, our multi-task learning framework. 

## Common usage scenarios

Most of the examples here use Scala. However, this software can be used as is from Java as well! Scroll down towards the end of this document to see a Java usage example.

### Annotating entire documents

(see [`corenlp/src/main/scala/org/clulab/processors/examples/ProcessorExample.scala`](https://github.com/clulab/processors/blob/master/corenlp/src/main/scala/org/clulab/processors/examples/ProcessorExample.scala) for a complete running example)

```scala
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.struct.DirectedGraphEdgeIterator

// create the processor
// any processor works here! Try FastNLPProcessor, or our own CluProcessor
val proc:Processor = new CoreNLPProcessor() 

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

```

For more details about the annotation data structures, please see the `org/clulab/processors/Document.scala` file.

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
     
Similarly, you can use our in-house processor with:

```scala
org.clulab.dynet.Utils.initializeDyNet()
val proc:Processor = new CluProcessor()
// everything else stays the same
```

Note that our processor has slightly different components. For example, our named entity recognizer currently includes only named entities, e.g., LOCATION, PERSON, and ORGANIZATION, and does not include numeric ones. On the other hand, `CluProcessor` includes a semantic role labeling component, which is missing from the CoreNLP processors.

Also note that because `CluProcessor` relies on DyNet, you will have to initialize DyNet first, as shown in the code. The default initialization parameters should work in most cases, but, if you want to increase the memory that is initially allocated to DyNet, you can pass these values to the `initializeDyNet` method, e.g.:

```scala
org.clulab.dynet.Utils.initializeDyNet(autoBatch = false, mem = "1024,1024,1024,1024")
```

The `mem` parameter is split into 4 values, because DyNet's memory consists of 4 zones. The first zone is used by the forward pass, the second by the backward (backpropagation) pass; the third zone stores the DyNet parameters; and the last one is the scratch zone. More information about this exists in [DyNet's documentation](https://dynet.readthedocs.io/en/latest/commandline.html): "DyNet runs by default with 512MB of memory, which is split evenly for the forward and backward steps, parameter storage as well as scratch use. This will be expanded automatically every time one of the pools runs out of memory. By setting NUMBER here, DyNet will allocate more memory immediately at the initialization stage. Note that you can also individually set the amount of memory for forward calculation, backward calculation, parameters, and scratch use by using comma separated variables."

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

You can use the annotators provided by CoreNLP separately by calling them individually. To illustrate,
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
serialized Java objects. This is because we store only the information required to recreate these annotations (e.g., words, lemmas, etc.) without storing any of the Java/Scala objects and classes.

### Serialization to/from `json`

As of v5.9.6, `Document` and `Mention` instances can be serialized to/from `json` ([see the complete working example](https://gist.github.com/myedibleenso/87a3191c73938840b8ed768ec305db38)).  

## Cleaning up the interned strings

Classes that implement the `Processor` trait may intern String objects to avoid allocating memory repeatedly for the same string. This is implemented by maintaining an internal dictionary of strings previously seen. This dictionary is unlikely to
use a lot of memory due to the Zipfian distribution of language. But, if memory usage is a big concern, it can be cleaned by
calling:

```scala
Processor.in.clear()
```

I recommend you do this only _after_ you annotated all the documents you plan to keep in memory.

You can also disable the interning of strings completely by setting the `internStrings = false` in the
processor constructor, as such:

```scala
val processor = new CoreNLPProcessor(internStrings = false)
```

## Using processors from Java

Scala is (largely) compatible with Java, so this library can be directly used from Java. Below is Java code that translates most of the functionality from the first Scala example in this document to Java:

```java
package org.clulab.processors;


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

