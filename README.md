# What is it?

This is the main public code repository of the NLP group led by [Mihai Surdeanu](http://surdeanu.info/mihai/) at [University of Arizona](http://www.arizona.edu). This repository contains (in descending order of novelty):

+ A rule-based event extraction (EE) framework called ODIN (Open Domain INformer) in the `edu.arizona.sista.odin` package, together with a grammar tailored for the biomedical domain. See [ODIN's Wiki page](https://github.com/sistanlp/processors/wiki/ODIN-(Open-Domain-INformer)) for more details.
+ Two full-fledged Rhetorical Structure Theory (RST) discourse parsers. The discourse parsers are transparently included in our natural language (NL) processors (see below). The version in `CoreNLPProcessor` relies on constituent syntax, whereas the one in `FastNLPProcessor` uses dependency syntax. The latter is marginally worse (~2 F1 points lower for the complete task) but it is much faster.
+ A machine learning (ML) package (`edu.arizona.sista.learning`), which includes implementations for common ML algorithms (e.g., Perceptron, Logistic Regression, Support Vector Machines, Random Forests) for both classification and ranking.
+ A suite of NL processors in the `edu.arizona.sista.processors` package. We currently provide three APIs: one for [Stanford's CoreNLP](http://nlp.stanford.edu/software/corenlp.shtml), one for a faster processor (`FastNLPProcessor`)
that cherry picks fast components from multiple sources (Stanford and [MaltParser](http://www.maltparser.org/)), and, lastly, one for biomedical texts (`BioNLPProcessor`), which integrates resources trained for this domain (Stanford parser and our own NER based on the Stanford CRF).

This software requires Java 1.8, Scala 2.11, and CoreNLP 3.x or higher.

All the code that we write is licensed under Apache License Version 2.0. However, some of the libraries used here, most notably CoreNLP, are GPL.

(c) Mihai Surdeanu, 2013 -

Authors: [Mihai Surdeanu](http://surdeanu.info/mihai/), Marco Valenzuela, Gustave Hanh-Powell, Peter Jansen, Daniel Fried, Dane Bell, and Tom Hicks.

# Changes
+ **5.2** - Version 2 of ODIN, including a cleaner (more declarative) rule language, which minimizes the need for custom actions. See `edu.arizona.sista.odin.domains.toydomain` for an example of a toy domain, and `edu.arizona.sista.odin.domains.bigmechanism.dryrun2015` for a complete biomedical domain.
+ **5.1** - Improved tokenization for the bio domain. Replaced the BANNER NER for the bio domain with our own implementation. Installing BANNER is thus no longer necessary.
+ **5.0** - changed to Java 8, Scala 2.11, and CoreNLP 3.5.1. First public release of ODIN (domain-independent event extraction) framework, in the `edu.arizona.sista.odin` package. First release of ODIN's DARPA biomedical grammar. `FastNLPProcessor` now supports both the Malt and the new Stanford NN dependency parser (the Stanford parser is now the default setting).
+ **4.0** - added `BioNLPProcessor`. Install our fork of the [BANNER named entity recognizer](https://github.com/sistanlp/banner) before!
+ **3.3** - bug fix: make sure DocumentSerializer.load() works when multiple documents are serialized into the same file.
+ **3.2** - Added a discourse parser to `FastNLPProcessor`. This performs marginally worse than the one in `CoreNLPProcessor`, but it is much faster for end-to-end processing, due to the shift-reduce syntactic parser.
+ **3.1** - Minimal functionality added to the learning package. Changed to CoreNLP 3.3.1.
+ **3.0** - Added a RST discourse parser to `CoreNLPProcessor`. Added the `edu.arizona.sista.learning` package. Utils classes are now under `edu.arizona.sista.utils` rather than `edu.arizona.sista.processors.utils`.
+ **2.2** - Various bug fixes. Added support for basic dependencies to `CoreNLPProcessor`.
+ **2.1** - Bug fix in FastNLPProcessor: better root detection algorithm, robust to malt inconsistencies.
+ **2.0** - We now support two processors: `CoreNLPProcessor` and `FastNLPProcessor`. Changed the package name from `e.a.s.processor` to `e.a.s.processors`. Added Java usage example to README. Updated to CoreNLP 3.3.0. Added better unit tests to check for thread safetiness.
+ **1.5** - Bug fixing. Made the string interning process (see `Processor.in`) local to each thread to avoid concurrency issues in multi-threaded programs. Added new unit tests. Added minor functionality to Lexicon.
+ **1.4** - Code cleanup. Added some minor new functionality such as finding base NPs in the Trees class.
+ **1.3** - Reverted back to the `1.x` version numbers, since we will add other software here not just CoreNLP. Added correct mvn dependencies for the CoreNLP jars. Removed the `install*.sh` scripts, which are no longer needed.
+ **3.2.0** - Updated to Scala 2.10.1 and CoreNLP 3.2.0. Changed versioning system to be identical to CoreNLP's, so it's clear which CoreNLP version is used.
+ **1.0** - Initial release

# Installation

This software is available on Maven Central. To use, simply add the following dependencies to your `pom.xml`:

    <dependency>
       <groupId>edu.arizona.sista</groupId>
       <artifactId>processors_2.11</artifactId>
       <version>5.2</version>
    </dependency>
    <dependency>
       <groupId>edu.arizona.sista</groupId>
       <artifactId>processors_2.11</artifactId>
       <version>5.2</version>
       <classifier>models</classifier>
    </dependency>

 The equivalent SBT dependencies are:

    libraryDependencies ++= Seq(
        "edu.arizona.sista" %% "processors" % "5.2",
        "edu.arizona.sista" %% "processors" % "5.2" classifier "models",
    )


# Why you should use this code
+ **Simple API** - the APIs provided are, at least in my opinion, simpler than those provided for the original code. For example, when using CoreNLP you won't have to deal with hash maps that take class objects as keys. Instead, we use mostly arrays of integers or strings, which are self explanatory.
+ **Memory efficient** - arrays are more memory efficient than hash maps. Furthermore, we used our own implementation to intern strings (i.e., avoiding to store duplicated strings multiple times). Due to these changes, I measured up to 99% decrease in memory for the annotations corresponding to a typical natural language text, when compared to the original CoreNLP code. (Note: this reduction takes effect only _after_ CoreNLP finishes its work.)
+ **Faster access** - again, because we use arrays instead of hash maps, access to the NL annotations (once constructed) is considerably faster than in the original Stanford code.
+ **Tool-independent API** - we support multiple NL and machine learning tools. If you use this code, you will have to change your code only minimally (i.e., only the constructor for the `Processor` object).
+ **Discourse parsing** - we include a complete RST parser; simply instantiate `CoreNLPProcessor` with `withDiscourse = true`.
+ **Biomedical tools** - we now include tools to process biomedical texts, which can be used under the same simple interface. We also offer event extraction tools for the biomedical domain.
+ **Rule-based event extraction** - we include an event extraction framework, which can be customized to various domains.

# How to compile the source code

This is a standard sbt project, so use the usual commands, e.g., `sbt compile`, `sbt assembly`, to compile.
Add the generated jar files under `target/` to your $CLASSPATH, along with the other necessary dependency jars. Take a look at `build.sbt` to see which dependencies are necessary at runtime.

# How to use it

## Common scenarios

Most of the examples here use Scala. However, this software can be used as is from Java as well! Scroll down towards the end of this document to see a Java usage example.

### Annotating entire documents

    // create the processor
    // any processor works here! Try FastNLPProcessor or BioNLPProcessor.
    val proc:Processor = new CoreNLPProcessor(withDiscourse = true)

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
      sentence.lemmas.foreach(lemmas => println("Lemmas: " + lemmas.mkString(" ")))
      sentence.tags.foreach(tags => println("POS tags: " + tags.mkString(" ")))
      sentence.entities.foreach(entities => println("Named entities: " + entities.mkString(" ")))
      sentence.norms.foreach(norms => println("Normalized entities: " + norms.mkString(" ")))
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
        // see the edu.arizona.sista.utils.Tree class for more information
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

The above code generates the following output:

    Sentence #0:
    Tokens: John Smith went to China .
    Start character offsets: 0 5 11 16 19 24
    End character offsets: 4 10 15 18 24 25
    Lemmas: John Smith go to China .
    POS tags: NNP NNP VBD TO NNP .
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

For more details about the annotation data structures, please see the `edu/arizona/sista/processor/Document.scala` file.

Changing processors is trivial: just replace the first line in the above example with:

    val proc:Processor = new FastNLPProcessor()
    // everything else stays the same

FastNLPProcessor uses the Stanford tokenizer, POS tagger, and NER, but replaces its parser with maltparser, trained to generated Stanford "basic" (rather than "collapsed") dependencies. This means that this annotator does not produce constituent trees and coreference chains. However, because of the faster parser, you should see a speedup increase of at least one order of magnitude. The output of the above code with `FastNLPProcessor` is:

    Sentence #0:
    Tokens: John Smith went to China .
    Start character offsets: 0 5 11 16 19 24
    End character offsets: 4 10 15 18 24 25
    Lemmas: John Smith go to China .
    POS tags: NNP NNP VBD TO NNP .
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

    val doc = proc.annotateFromSentences(List("John Smith went to China.", "He visited Beijing."))
    // everything else stays the same

### Annotating documents already split into sentences and tokenized

    val doc = annotateFromTokens(List(
      List("John", "Smith", "went", "to", "China", "."),
      List("There", ",", "he", "visited", "Beijing", ".")))
    // everything else stays the same

## Using individual annotators

You can of course use only some of the annotators provided by CoreNLP by calling them individually. To illustrate,
the `Processor.annotate()` method is implemented as follows:

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

(Note that CoreNLP currently does not support chunking and semantic role labeling.)
You can use just a few of these annotators. For example, if you need just POS tags, lemmas and named entities, you could use
the following code:

    val doc = proc.mkDocument("John Smith went to China. He visited Beijing, on January 10th, 2013.")
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    doc.clear()

Note that the last method called (`doc.clear()`) clears the internal structures created by the actual CoreNLP annotators.
This saves a lot of memory, so, although it is not strictly necessary, I recommend you call it.

## Serialization

This package also offers serialization code for the generated annotations. The two scenarios currently supported are:

### Serialization to/from streams

    // saving to a PrintWriter, pw
    val someAnnotation = proc.annotate(someText)
    val serializer = new DocumentSerializer
    serializer.save(someAnnotation, pw)

    // loading from an InputStream, is
    val someAnnotation = serializer.load(is)

### Serialization to/from Strings

    // saving to a String object, savedString
    val someAnnotation = proc.annotate(someText)
    val serializer = new DocumentSerializer
    val savedString = serializer.save(someAnnotation)

    // loading from a String object, fromString
    val someAnnotation = serializer.load(fromString)

Note that space required for these serialized annotations is considerably smaller (8 to 10 times) than the corresponding
serialized Java objects. This is because we store only the information required to recreate these annotations (e.g., words, lemmas, etc.)
without storing any of the Java/Scala objects and classes.

## Cleaning up the interned strings

Classes that implement the `Processor` trait intern String objects to avoid allocating memory repeatedly for the same string.
This is implemented by maintaining an internal dictionary of strings previously seen. This dictionary is unlikely to
use a lot of memory due to the Zipfian distribution of language. But, if memory usage is a big concern, it can be cleaned by
calling:

    Processor.in.clear()

I recommend you do this only _after_ you annotated all the documents you plan to keep in memory.

Although I see no good reason for doing this, you can disable the interning of strings completely by setting the `internStrings = false` in the
CoreNLProcessor constructor, as such:

    val processor = new CoreNLPProcessor(internStrings = false)

## Using processors from Java

Scala is (largely) compatible with Java, so this library can be directly used from Java. Below is Java code that translates most of the functionality from the first Scala example in this document to Java:

    import edu.arizona.sista.struct.DirectedGraphEdgeIterator;
    import edu.arizona.sista.processors.*;
    import edu.arizona.sista.processors.corenlp.CoreNLPProcessor;
    import edu.arizona.sista.processors.fastnlp.FastNLPProcessor;

    public class ProcessorJavaExample {
        public static void main(String [] args) throws Exception {
            // create the processor
            Processor proc = new CoreNLPProcessor(true, false, false, 100);
            // for much faster processing, use FastNLPProcessor
            // Processor proc = new FastNLPProcessor(true, false);

            // the actual work is done here
            Document doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.");

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
                    // see the edu.arizona.sista.struct.Tree class for more information
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

The output of this code is:

    Sentence #0:
    Tokens: John Smith went to China .
    Start character offsets: 0 5 11 16 19 24
    End character offsets: 4 10 15 18 24 25
    Lemmas: John Smith go to China .
    POS tags: NNP NNP VBD TO NNP .
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

## The discourse parser

The discourse parser in `processors` is inspired by the parser of [Feng and Hirst](http://www.cs.toronto.edu/~weifeng/software.html) and the HILDA parser of [Hernault et al.](http://elanguage.net/journals/dad/article/view/591), but with a different feature set.
It is transparently integrated in both `CoreNLPProcessor` and `FastNLPProcessor`: just instantiate it as `CoreNLPProcessor(withDiscourse = true)` or `FastNLPProcessor(withDiscourse = true)`. If discourse is enabled, `Document.discourseTree` stores the discourse tree for the entire document as an instance of the `DiscourseTree` class.

Following the conventions from other modern discourse parsing work, the discourse tree:
+ Is represented as a binary tree, containing hypotactic relations (containing one nucleus and one satellite node) or paratactic relations (both nodes have equal importance).
+ Stores the relation labels in the parent node (in `DiscourseTree.relationLabel`) rather than the satellite nodes (like the RST corpus). We use the same 18 labels as Feng and Hirst.
+ Stores the relation direction in `DiscourseTree.relationDirection`. The direction can be `LeftToRight` (meaning the nucleus is the left child), `RightToLeft` (the right node is the nucleus), or `None` (for paratactic relations).

If you end up using this discourse parser, I would appreciate it if you cited this work:

Peter Jansen, Mihai Surdeanu, and Peter Clark. [Discourse Complements Lexical Semantics for Non-factoid Answer Reranking](http://www.surdeanu.info/mihai/papers/acl2014.pdf). In Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics (ACL), 2014. [[bib]](http://www.surdeanu.info/mihai/papers/acl2014.bib)

Developers only: For more details on the discourse parsers, please see [this Wiki page](https://github.com/sistanlp/processors/wiki/Discourse-Parsers-Details).

## The `edu.arizona.sista.learning` package

`processors` now contains a machine learning (ML) package (`edu.arizona.sista.learning`), which includes implementations for common ML algorithms (e.g., Perceptron, Logistic Regression, Support Vector Machines, Random Forests) for both classification and ranking.

The structure of this package is heavily inspired by Stanford's CoreNLP. Similar to CoreNLP, we use a `Datum` trait to store a single data point, which is implemented by `BVFDatum` to store binary-valued-feature datums, or by `RVFDatum` to store real-valued-feature datums. A collection of data points is stored as a `Dataset`, which is similarly implemented by `BVFDataset` or `RVFDataset`. All classifiers implement the `Classifier` trait, which has three main methods: `train`, which trains a model a given dataset, `classOf`, which returns the most likely prediction for a given datum, and `scoresOf`, which returns the scores for all known labels for a given datum. We currently support the following classifiers: large-margin Perceptron (`PerceptronClassifier`), linear SVMs and logistic regression from [liblinear](http://www.csie.ntu.edu.tw/~cjlin/liblinear/) (`LibLinearClassifier`), dual SVMs from [libsvm](http://www.csie.ntu.edu.tw/~cjlin/libsvm/) (`LibSVMClassifier`), and random forests from [fast-random-forest](https://code.google.com/p/fast-random-forest/) (`RandomForestClassifier`).

A similar structure exists for ranking problems, with `RankingDataset` used to store a corpus of ranking examples, and  `RankingClassifier` as the API to be implemented by all ranking classifiers. We currently support the following classifiers: ranking Perceptron (`PerceptronRankingClassifier`), ranking SVMs from [svm-rank](http://www.cs.cornell.edu/people/tj/svm_light/svm_rank.html) (`SVMRankingClassifier`), and boosted decision trees from [jforests](https://code.google.com/p/jforests/) (`JForestsRankingClassifier`).

For usage examples, including how to create datums and datasets from scratch or import them from the svm-light format, please take a look at the examples under `src/test/scala/edu/arizona/sista/learning`.

## The ODIN event extraction framework

Please see [ODIN's Wiki](https://github.com/sistanlp/processors/wiki/ODIN-(Open-Domain-INformer)) page for details.

