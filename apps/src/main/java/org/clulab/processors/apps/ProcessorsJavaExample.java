package org.clulab.processors.apps;

import org.clulab.processors.Document;
import org.clulab.processors.Processor;
import org.clulab.processors.Processor$;
import org.clulab.processors.Sentence;
import org.clulab.struct.DirectedGraphEdgeIterator;
import org.clulab.utils.JavaUtils;

import java.util.Iterator;
import scala.collection.Seq;

public class ProcessorsJavaExample {
    public static void main(String [] args) throws Exception {
        // Create the processor
        Processor proc = Processor$.MODULE$.mkProcessor();

        // The actual work is done here.
        Document doc = proc.annotate("John Smith went to China. He visited Beijing on January 10th, 2013.", false);

        // You are basically done.  The rest of this code simply prints out the annotations.

        // Let's print the sentence-level annotations.
        for (int sentenceIndex = 0; sentenceIndex < doc.sentences().length(); sentenceIndex++) {
            Sentence sentence = doc.sentences().apply(sentenceIndex);
            System.out.println("Sentence #" + sentenceIndex + ":");
            System.out.println("Tokens: " + mkStringStr(sentence.words()));
            System.out.println("Start character offsets: " + mkStringInt(sentence.startOffsets()));
            System.out.println("End character offsets: " + mkStringInt(sentence.endOffsets()));

            // These annotations are optional, so they are stored using Option objects,
            // hence the isDefined() and get() calls.
            if (sentence.lemmas().isDefined())
                System.out.println("Lemmas: " + mkStringStr(sentence.lemmas().get()));
            if (sentence.tags().isDefined())
                System.out.println("POS tags: " + mkStringStr(sentence.tags().get()));
            if (sentence.chunks().isDefined())
                System.out.println("Chunks: " + mkStringStr(sentence.chunks().get()));
            if (sentence.entities().isDefined())
                System.out.println("Named entities: " + mkStringStr(sentence.entities().get()));
            if (sentence.norms().isDefined())
                System.out.println("Normalized entities: " + mkStringStr(sentence.norms().get()));
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

    public static String mkStringStr(Seq<String> strings, String sep) {
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < strings.length(); i ++) {
            if (i > 0) stringBuilder.append(sep);
            stringBuilder.append(strings.apply(i));
        }
        return stringBuilder.toString();
    }

    public static String mkStringStr(Seq<String> strings) { return mkStringStr(strings, " "); }

    public static String mkStringInt(Seq<Object> ints, String sep) {
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < ints.length(); i ++) {
            if (i > 0) stringBuilder.append(sep);
            stringBuilder.append(ints.apply(i));
        }
        return stringBuilder.toString();
    }

    public static String mkStringInt(Seq<Object> ints) { return mkStringInt(ints, " "); }

    public static<T> Iterable<T> iteratorToIterable(Iterator<T> iterator) { return () -> iterator; }
}
