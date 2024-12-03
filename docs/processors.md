---
title: Processors
has_children: false
nav_order: 3
---

# Processors

This library contains a suite of natural language processors that include tokenization, lemmatization, part-of-speech tagging, named entity recognition, syntactic chunking, and syntactic dependency parsing.

These tools are wrapped up in `BalaurProcessor`, which contains:
* Tokenization (implemented with [Antlr](http://www.antlr.org));
* Lemmatization (using [MorphaStemmer](https://search.maven.org/#artifactdetails%7Cedu.washington.cs.knowitall.nlptools%7Cnlptools-stem-morpha_2.10%7C2.4.5%7Cjar));
* Part-of-speech (POS) tagging;
* Numeric entity recognition (e.g., dates, money) (implemented using Odin);
* Named entity recognition (NER);
* Shallow syntactic parsing or chunking; and
* Syntactic dependency parsing.

The POS tagging, NER, chunking, and dependency parsing tasks are implemented using a multi-task-learning (MTL) architecture implemented on top of a shared encoder using PyTorch and Hugging Face. The complete code for this MTL system is in our [`scala-transformers` project](https://github.com/clulab/scala-transformers).
Note that the dependency parsing component uses the algorithm of [(Amini et al., 2023)](https://aclanthology.org/2023.acl-short.124.pdf).


## Common usage scenarios

Most of the examples here use Scala. However, this software can be used as is from Java as well! Scroll down towards the end of this document to see a Java usage example.

### Annotating entire documents

This code is also available in the `org.clulab.processors.apps.ProcessorsScalaExample` object.

```scala
package org.clulab.processors.apps

import org.clulab.processors.{Document, Processor}
import org.clulab.struct.DirectedGraphEdgeIterator

object ProcessorsScalaExample extends App {
  val proc = Processor()

  // The actual work is done here.
  val doc: Document = proc.annotate("John Smith went to China. He visited Beijing on January 10th, 2013.")

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
    println()
  }

  def mkString[T](elems: Array[T]): String = elems.mkString(" ")
}
```
The above code generates the following output:

```
Sentence #0:
Tokens: John Smith went to China .
Start character offsets: 0 5 11 16 19 24
End character offsets: 4 10 15 18 24 25
Lemmas: john smith go to china .
POS tags: NNP NNP VBD TO NNP .
Chunks: B-NP I-NP B-VP B-PP B-NP O
Named entities: B-PER I-PER O O B-LOC O
Normalized entities:      
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
Lemmas: he visit beij on january 10th , 2013 .
POS tags: PRP VBD NNP IN NNP JJ , CD .
Chunks: B-NP B-VP B-NP B-PP B-NP I-NP I-NP I-NP O
Named entities: O O B-LOC O B-DATE I-DATE I-DATE I-DATE O
Normalized entities:     2013-01-10 2013-01-10 2013-01-10 2013-01-10 
Syntactic dependencies:
 head: 1 modifier: 0 label: nsubj
 head: 1 modifier: 2 label: dobj
 head: 1 modifier: 4 label: nmod_on
 head: 1 modifier: 8 label: punct
 head: 4 modifier: 3 label: case
 head: 4 modifier: 5 label: amod
 head: 4 modifier: 6 label: punct
 head: 4 modifier: 7 label: nummod
```

For more details about the annotation data structures, please see the [Document](https://github.com/clulab/processors/blob/master/main/src/main/scala/org/clulab/processors/Document.scala) class.


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

## Using processors from Java

Scala is (largely) compatible with Java, so this library can be directly used from Java. The code below shows an example. This code is also available in the `org.clulab.processors.apps.ProcessorsJavaExample` class.

```java
package org.clulab.processors.apps;

import org.clulab.processors.Document;
import org.clulab.processors.Processor;
import org.clulab.processors.Processor$;
import org.clulab.processors.Sentence;
import org.clulab.struct.DirectedGraphEdgeIterator;
import org.clulab.utils.JavaUtils;

import java.util.Iterator;

public class ProcessorsJavaExample {
    public static void main(String [] args) throws Exception {
        // Create the processor
        Processor proc = Processor$.MODULE$.mkProcessor();

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
            System.out.println();
            System.out.println();
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
