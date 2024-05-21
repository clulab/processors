package org.clulab.processors;

import com.typesafe.config.ConfigFactory;
import org.clulab.processors.clu.BalaurProcessor;
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
    // These are for the BalaurProcessor.
    // final static Option<LexiconNER> noLexiconNER = Option.empty();
    // final static Option<String> noString = Option.empty();
    // final static Map<String, Object> emptyMap = new HashMap();

    public static void main(String [] args) throws Exception {
        // Create the processor.  Any processor works here!
        // Try FastNLPProcessor or our own BalaurProcessor.
        Processor proc = new CoreNLPProcessor(true, true, false, 0, 100);

        // Processor proc = new FastNLPProcessor(true, true, false, 0);

        // org.clulab.fatdynet.utils.Initializer.initialize(emptyMap);
        // Processor proc = new BalaurProcessor(ConfigFactory.load("balaurprocessor"), noLexiconNER, noString);

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
