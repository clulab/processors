package org.clulab.processors;

import org.clulab.processors.corenlp.CoreNLPProcessor;
import org.clulab.processors.fastnlp.FastNLPProcessor;
import org.clulab.struct.CorefMention;
import org.clulab.struct.DirectedGraphEdgeIterator;

import scala.collection.JavaConverters;

import java.util.Arrays;
import java.util.Iterator;

public class ProcessorsJavaExample {

    public static void main(String [] args) throws Exception {
        // Create the processor.  Any processor works here!
        // Try FastNLPProcessor or our own CluProcessor.
        Processor proc = new CoreNLPProcessor(true, true, false, 0, 100);

        // Processor proc = new FastNLPProcessor(true, true, false, 0);

        // The actual work is done here.
        Document doc = proc.annotate("John Smith went to China. He visited Beijing on January 10th, 2013.", false);

        // You are basically done.  The rest of this code simply prints out the annotations.

        // Let's print the sentence-level annotations.
        int sentenceIndex = 0;
        for (Sentence sentence: doc.sentences()) {
            System.out.println("Sentence #" + sentenceIndex + ":");
            System.out.println("Tokens: " + mkString(sentence.words(), " "));
            System.out.println("Start character offsets: " + mkString(sentence.startOffsets(), " "));
            System.out.println("End character offsets: " + mkString(sentence.endOffsets(), " "));

            // These annotations are optional, so they are stored using Option objects,
            // hence the foreach statement.
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
                scala.collection.Iterator<scala.Tuple3<Object, Object, String>> iterator =
                        new DirectedGraphEdgeIterator<>(sentence.dependencies().get());
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
            sentenceIndex += 1;
            System.out.println();
            System.out.println();
        }

        // Let's print the coreference chains.
        if (doc.coreferenceChains().isDefined()) {
            scala.collection.Iterator<scala.collection.Iterable<CorefMention>> chains = doc.coreferenceChains().get().getChains().iterator();
            while (chains.hasNext()) {
                scala.collection.Iterator<CorefMention> chain = chains.next().iterator();
                System.out.println("Found one coreference chain containing the following mentions:");
                while (chain.hasNext()) {
                    CorefMention mention = chain.next();
                    String text = "[" + mkString(Arrays.copyOfRange(doc.sentences()[mention.sentenceIndex()].words(),
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
        StringBuilder os = new StringBuilder();
        for (int i = 0; i < strings.length; i ++) {
            if (i > 0) os.append(sep);
            os.append(strings[i]);
        }
        return os.toString();
    }

    public static String mkString(String[] strings) {
        return mkString(strings, " ");
    }

    public static String mkString(int[] ints, String sep) {
        StringBuilder os = new StringBuilder();
        for (int i = 0; i < ints.length; i ++) {
            if (i > 0) os.append(sep);
            os.append(ints[i]);
        }
        return os.toString();
    }

    public static String mkString(int[] ints) {
        return mkString(ints, " ");
    }

    public static<T> Iterable<T> iteratorToIterable(scala.collection.Iterator<T> scalaIterator) {
        Iterator<T> javaIterator = JavaConverters.asJavaIterator(scalaIterator);
        return () -> javaIterator;
    }
}
