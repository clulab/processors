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
* `CluProcessor` - an in-house processor (licensed under the Apache license) that contains tokenization (using [Antlr](http://www.antlr.org)), lemmatization (using [MorphaStemmer](https://search.maven.org/#artifactdetails%7Cedu.washington.cs.knowitall.nlptools%7Cnlptools-stem-morpha_2.10%7C2.4.5%7Cjar)), POS tagging, named entity recognition (NER), shallow syntactic parsing or chunking, dependency parsing, and semantic role labeling. The last five components are implemented using `Metal`, our multi-task learning framework.

## Common usage scenarios

Most of the examples here use Scala. However, this software can be used as is from Java as well! Scroll down towards the end of this document to see a Java usage example.

### Annotating entire documents

See also [code](https://github.com/clulab/processors/blob/master/corenlp/src/main/scala/org/clulab/processors/examples/DocumentationExample.scala) for the complete running example.

```scala
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.struct.DirectedGraphEdgeIterator

// Create the processor.  Any processor works here!
// Try FastNLPProcessor or our own CluProcessor.
val proc: Processor = new CoreNLPProcessor()

// The actual work is done here.
val doc: Document = proc.annotate("John Smith went to China. He visited Beijing on January 10th, 2013.")

// You are basically done.  The rest of this code simply prints out the annotations.

// Let's print the sentence-level annotations.
for ((sentence, sentenceIndex) <- doc.sentences.zipWithIndex) {
  println("Sentence #" + sentenceIndex + ":")
  println("Tokens: " + mkString(sentence.words))
  println("Start character offsets: " + mkString(sentence.startOffsets))
  println("End character offsets: " + mkString(sentence.endOffsets))

  // These annotations are optional, so they are stored using Option objects,
  // hence the foreach statement.
  sentence.lemmas.foreach(lemmas => println("Lemmas: " + mkString(lemmas)))
  sentence.tags.foreach(tags => println("POS tags: " + mkString(tags)))
  sentence.chunks.foreach(chunks => println("Chunks: " + mkString(chunks)))
  sentence.entities.foreach(entities => println("Named entities: " + mkString(entities)))
  sentence.norms.foreach(norms => println("Normalized entities: " + mkString(norms)))
  sentence.dependencies.foreach { dependencies =>
    println("Syntactic dependencies:")
    val iterator = new DirectedGraphEdgeIterator[String](dependencies)
    iterator.foreach { dep =>
      // Note that we use offsets starting at 0 unlike CoreNLP, which uses offsets starting at 1.
      println(" head: " + dep._1 + " modifier: " + dep._2 + " label: " + dep._3)
    }
  }
  sentence.syntacticTree.foreach { syntacticTree =>
    // See the org.clulab.utils.Tree class for more information
    // on syntactic trees, including access to head phrases/words.
    println("Constituent tree: " + syntacticTree)
  }
  println()
  println()
}

// Let's print the coreference chains.
doc.coreferenceChains.foreach { chains =>
  for (chain <- chains.getChains) {
    println("Found one coreference chain containing the following mentions:")
    for (mention <- chain) {
      val text = doc.sentences(mention.sentenceIndex).words
          .slice(mention.startOffset, mention.endOffset).mkString("[", " ", "]")
      // Note that all these offsets start at 0, too.
      println("\tsentenceIndex: " + mention.sentenceIndex +
          " headIndex: " + mention.headIndex +
          " startTokenOffset: " + mention.startOffset +
          " endTokenOffset: " + mention.endOffset +
          " text: " + text)
    }
  }
}

def mkString[T](elems: Array[T]): String = elems.mkString(" ")
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
Named entities: PERSON PERSON O O COUNTRY O
Normalized entities: O O O O O O
Syntactic dependencies:
 head:1 modifier:0 label:compound
 head:2 modifier:1 label:nsubj
 head:2 modifier:4 label:nmod_to
 head:4 modifier:3 label:case
Constituent tree: (ROOT (S (NP (NNP John) (NNP Smith)) (VP (VBD went) (PP (TO to) (NP (NNP China)))) (. .)))


Sentence #1:
Tokens: He visited Beijing , on January 10th , 2013 .
Start character offsets: 26 29 37 44 46 49 57 61 63 67
End character offsets: 28 36 44 45 48 56 61 62 67 68
Lemmas: he visit Beijing , on January 10th , 2013 .
POS tags: PRP VBD NNP , IN NNP JJ , CD .
Chunks: B-NP B-VP B-NP O B-PP B-NP I-NP I-NP I-NP O
Named entities: O O CITY O O DATE DATE DATE DATE O
Normalized entities: O O O O O 2013-01-10 2013-01-10 2013-01-10 2013-01-10 O
Syntactic dependencies:
 head:1 modifier:0 label:nsubj
 head:1 modifier:2 label:iobj
 head:1 modifier:8 label:dobj
 head:2 modifier:5 label:nmod_on
 head:5 modifier:4 label:case
 head:5 modifier:6 label:amod
Constituent tree: (ROOT (S (NP (PRP He)) (VP (VBD visited) (NP (NP (NNP Beijing)) (, ,) (PP (IN on) (NP (NNP January) (JJ 10th))) (, ,)) (NP (CD 2013))) (. .)))


Found one coreference chain containing the following mentions:
	sentenceIndex: 1 headIndex: 2 startTokenOffset: 2 endTokenOffset: 7 text: [Beijing , on January 10th]
Found one coreference chain containing the following mentions:
	sentenceIndex: 0 headIndex: 1 startTokenOffset: 0 endTokenOffset: 2 text: [John Smith]
	sentenceIndex: 1 headIndex: 0 startTokenOffset: 0 endTokenOffset: 1 text: [He]
Found one coreference chain containing the following mentions:
	sentenceIndex: 1 headIndex: 5 startTokenOffset: 5 endTokenOffset: 7 text: [January 10th]
Found one coreference chain containing the following mentions:
	sentenceIndex: 0 headIndex: 4 startTokenOffset: 4 endTokenOffset: 5 text: [China]
Found one coreference chain containing the following mentions:
	sentenceIndex: 1 headIndex: 8 startTokenOffset: 8 endTokenOffset: 9 text: [2013]
```

For more details about the annotation data structures, please see the [Document](https://github.com/clulab/processors/blob/master/main/src/main/scala/org/clulab/processors/Document.scala) class.

Changing processors is trivial: just replace the first executable line in the above example with

```scala
val proc: Processor = new FastNLPProcessor()
// Everything else stays the same.
```

`FastNLPProcessor` is similar to `CoreNLPProcessor` but uses Stanford's dependency parser instead of the constituent parser and does not include coreference resolution. Because the dependency parser is faster, you should see a considerable decrease in run time.  The output of the above code with `FastNLPProcessor` is
```
Sentence #0:
Tokens: John Smith went to China .
Start character offsets: 0 5 11 16 19 24
End character offsets: 4 10 15 18 24 25
Lemmas: John Smith go to China .
POS tags: NNP NNP VBD TO NNP .
Chunks: B-NP I-NP B-VP B-PP B-NP O
Named entities: PERSON PERSON O O COUNTRY O
Normalized entities: O O O O O O
Syntactic dependencies:
 head: 1 modifier: 0 label: compound
 head: 2 modifier: 1 label: nsubj
 head: 2 modifier: 4 label: nmod_to
 head: 2 modifier: 5 label: punct
 head: 4 modifier: 3 label: case


Sentence #1:
Tokens: He visited Beijing on January 10th , 2013 .
Start character offsets: 26 29 37 45 48 56 60 62 66
End character offsets: 28 36 44 47 55 60 61 66 67
Lemmas: he visit Beijing on January 10th , 2013 .
POS tags: PRP VBD NNP IN NNP JJ , CD .
Chunks: B-NP B-VP B-NP B-PP B-NP I-NP I-NP I-NP O
Named entities: O O CITY O DATE DATE DATE DATE O
Normalized entities: O O O O 2013-01-10 2013-01-10 2013-01-10 2013-01-10 O
Syntactic dependencies:
 head: 1 modifier: 0 label: nsubj
 head: 1 modifier: 2 label: dobj
 head: 1 modifier: 4 label: nmod_on
 head: 1 modifier: 8 label: punct
 head: 4 modifier: 3 label: case
 head: 4 modifier: 5 label: amod
 head: 5 modifier: 6 label: punct
 head: 5 modifier: 7 label: dep
```

Similarly, you can use our in-house processor with

```scala
org.clulab.dynet.Utils.initializeDyNet()
val proc: Processor = new CluProcessor()
// Everything else stays the same.
```

Note that our processor has slightly different components. `CluProcessor` does not include coreference resolution, but it includes a semantic role labeling component, which is missing from the CoreNLP processors.

Also note that because `CluProcessor` relies on DyNet, you will have to initialize DyNet first, as shown in the code. The default initialization parameters should work in most cases, but, if you want to increase the memory that is initially allocated to DyNet, you can pass these values to the `initializeDyNet` method, e.g.:

```scala
org.clulab.dynet.Utils.initializeDyNet(mem = "1024,1024,1024,1024")
```

The `mem` parameter is split into 4 values, because DyNet's memory consists of 4 zones. The first zone is used by the forward pass, the second by the backward (backpropagation) pass; the third zone stores the DyNet parameters; and the last one is the scratch zone. More information about this can be found in [DyNet's documentation](https://dynet.readthedocs.io/en/latest/commandline.html): "DyNet runs by default with 512MB of memory, which is split evenly for the forward and backward steps, parameter storage as well as scratch use. This will be expanded automatically every time one of the pools runs out of memory. By setting NUMBER here, DyNet will allocate more memory immediately at the initialization stage. Note that you can also individually set the amount of memory for forward calculation, backward calculation, parameters, and scratch use by using comma separated variables."

### Annotating documents already split into sentences

```scala
val doc = proc.annotateFromSentences(List("John Smith went to China.", "He visited Beijing."))
// Everything else stays the same.
```

### Annotating documents already split into sentences and tokenized

```scala
val doc = proc.annotateFromTokens(List(
  List("John", "Smith", "went", "to", "China", "."),
  List("There", ",", "he", "visited", "Beijing", ".")
))
// Everything else stays the same.
```

## Using individual annotators

You can use the annotators provided by a `Processor` separately by calling them individually. To illustrate, the `annotate()` method is implemented as follows for the two CoreNLP processors:

```scala
def annotate(doc: Document): Document = {
  tagPartsOfSpeech(doc)
  lemmatize(doc)
  recognizeNamedEntities(doc)
  parse(doc)
  chunking(doc)
  relationExtraction(doc)
  srl(doc)
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

Note that the last method, `doc.clear()`, clears the internal structures created by the actual CoreNLP annotators.  This saves a lot of memory, so, although it is not strictly necessary, I recommend you call it.

## Serialization

This package also offers serialization code for the generated annotations (docs). The two scenarios currently supported are:

### Serialization to/from Writers and Readers

```scala
// saving to a PrintWriter
val someAnnotation = proc.annotate(someText)
val serializer = new org.clulab.serialization.DocumentSerializer
serializer.save(someAnnotation, printWriter)

// loading from an BufferedReader
val someAnnotation2 = serializer.load(bufferedReader)
```

### Serialization to/from Strings

```scala
// saving to a String
val someAnnotation = proc.annotate(someText)
val serializer = new org.clulab.serialization.DocumentSerializer
val string = serializer.save(someAnnotation)

// loading from a String
val someAnnotation2 = serializer.load(string)
```

Note that space required for these serialized annotations is considerably smaller (8 to 10 times) than the corresponding serialized Java objects. This is because we store only the information required to recreate these annotations (e.g., words, lemmas, etc.) without storing any of the Java/Scala objects and classes.

### Serialization to/from `json`

As of v5.9.6, `Document` and `Mention` instances can be serialized to/from `json`.  See the complete [working example](https://gist.github.com/myedibleenso/87a3191c73938840b8ed768ec305db38).

## Cleaning up the interned strings

Classes that implement the `Processor` trait may intern `String` objects to avoid allocating memory repeatedly for the same string. This is implemented by maintaining an internal dictionary of strings previously seen. This dictionary is unlikely to use a lot of memory due to the Zipfian distribution of language. But, if memory usage is a big concern, it can be cleaned by
calling

```scala
Processor.clearStrings()
```

I recommend you do this only _after_ you annotated all the documents you plan to keep in memory.

You can also disable the interning of strings completely by setting the `internStrings = false` in the Processor constructor, as such:

```scala
val proc = new CoreNLPProcessor(internStrings = false)
```

## Using processors from Java

Scala is (largely) compatible with Java, so this library can be directly used from Java. Below is Java [code](https://github.com/clulab/processors/blob/master/corenlp/src/main/java/org/clulab/processors/ProcessorsJavaExample.java) that translates the functionality from the first Scala example in this document to Java:

```java
package org.clulab.processors;

import com.typesafe.config.ConfigFactory;
import org.clulab.processors.clu.CluProcessor;
import org.clulab.processors.corenlp.CoreNLPProcessor;
import org.clulab.processors.fastnlp.FastNLPProcessor;
import org.clulab.sequences.LexiconNER;
import org.clulab.struct.CorefMention;
import org.clulab.struct.DirectedGraphEdgeIterator;
import org.clulab.utils.JavaUtils;

import scala.Option;
import scala.collection.immutable.HashMap;
import scala.collection.immutable.Map;

import java.util.Arrays;
import java.util.Iterator;

public class ProcessorsJavaExample {
    // These are for the CluProcessor.
    // final static Option<LexiconNER> noLexiconNER = Option.empty();
    // final static Option<String> noString = Option.empty();
    // final static Map<String, Object> emptyMap = new HashMap();

    public static void main(String [] args) throws Exception {
        // Create the processor.  Any processor works here!
        // Try FastNLPProcessor or our own CluProcessor.
        Processor proc = new CoreNLPProcessor(true, true, false, 0, 100);

        // Processor proc = new FastNLPProcessor(true, true, false, 0);

        // org.clulab.fatdynet.utils.Initializer.initialize(emptyMap);
        // Processor proc = new CluProcessor(ConfigFactory.load("cluprocessor"), noLexiconNER, noString);

        // The actual work is done here.
        Document doc = proc.annotate("John Smith went to China. He visited Beijing on January 10th, 2013.", false);

        // You are basically done.  The rest of this code simply prints out the annotations.

        // Let's print the sentence-level annotations.
        for (int sentenceIndex = 0; sentenceIndex < doc.sentences().length; sentenceIndex++) {
            Sentence sentence = doc.sentences()[sentenceIndex];
            System.out.println("Sentence #" + sentenceIndex + ":");
            System.out.println("Tokens: " + mkString(sentence.words()));
            System.out.println("Start character offsets: " + mkString(sentence.startOffsets()));
            System.out.println("End character offsets: " + mkString(sentence.endOffsets()));

            // These annotations are optional, so they are stored using Option objects,
            // hence the isDefined() and get() calls.
            if (sentence.lemmas().isDefined())
                System.out.println("Lemmas: " + mkString(sentence.lemmas().get()));
            if (sentence.tags().isDefined())
                System.out.println("POS tags: " + mkString(sentence.tags().get()));
            if (sentence.chunks().isDefined())
                System.out.println("Chunks: " + mkString(sentence.chunks().get()));
            if (sentence.entities().isDefined())
                System.out.println("Named entities: " + mkString(sentence.entities().get()));
            if (sentence.norms().isDefined())
                System.out.println("Normalized entities: " + mkString(sentence.norms().get()));
            if (sentence.dependencies().isDefined()) {
                System.out.println("Syntactic dependencies:");
                Iterator<scala.Tuple3<Object, Object, String>> iterator =
                        JavaUtils.asJava(new DirectedGraphEdgeIterator<>(sentence.dependencies().get()));
                for (scala.Tuple3<Object, Object, String> dep: iteratorToIterable(iterator)) {
                    // Note that we use offsets starting at 0 unlike CoreNLP, which uses offsets starting at 1.
                    System.out.println(" head: " + dep._1() + " modifier: " + dep._2() + " label: " + dep._3());
                }
            }
            if (sentence.syntacticTree().isDefined()) {
                // See the org.clulab.utils.Tree class for more information
                // on syntactic trees, including access to head phrases/words.
                System.out.println("Constituent tree: " + sentence.syntacticTree().get());
            }
            System.out.println();
            System.out.println();
        }

        // Let's print the coreference chains.
        if (doc.coreferenceChains().isDefined()) {
            Iterator<scala.collection.Iterable<CorefMention>> chains =
                    JavaUtils.asJava(doc.coreferenceChains().get().getChains().iterator());
            for (scala.collection.Iterable<CorefMention> chain: iteratorToIterable(chains)) {
                System.out.println("Found one coreference chain containing the following mentions:");
                for (CorefMention mention: iteratorToIterable(JavaUtils.asJava(chain.iterator()))) {
                    String text = "[" + mkString(Arrays.copyOfRange(
                            doc.sentences()[mention.sentenceIndex()].words(),
                            mention.startOffset(), mention.endOffset())) + "]";
                    // Note that all these offsets start at 0, too.
                    System.out.println("\tsentenceIndex: " + mention.sentenceIndex() +
                            " headIndex: " + mention.headIndex() +
                            " startTokenOffset: " + mention.startOffset() +
                            " endTokenOffset: " + mention.endOffset() +
                            " text: " + text);
                }
            }
        }
    }

    public static String mkString(String[] strings, String sep) {
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < strings.length; i ++) {
            if (i > 0) stringBuilder.append(sep);
            stringBuilder.append(strings[i]);
        }
        return stringBuilder.toString();
    }

    public static String mkString(String[] strings) { return mkString(strings, " "); }

    public static String mkString(int[] ints, String sep) {
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < ints.length; i ++) {
            if (i > 0) stringBuilder.append(sep);
            stringBuilder.append(ints[i]);
        }
        return stringBuilder.toString();
    }

    public static String mkString(int[] ints) { return mkString(ints, " "); }

    public static<T> Iterable<T> iteratorToIterable(Iterator<T> iterator) { return () -> iterator; }
}
```

The output of this code matches the output of the Scala code exactly.