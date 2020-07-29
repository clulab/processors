package org.clulab.processors;

import edu.stanford.nlp.ling.CoreAnnotations.DocDateAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.NormalizedNamedEntityTagAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.CoreDocument;
import edu.stanford.nlp.pipeline.CoreSentence;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;

import java.util.ArrayList;
import java.util.Properties;

class TestProcessorEnv {

    public static class Processor {

        protected static StanfordCoreNLP newCoreNlp(String annotator) {
            Properties props = new Properties();
            props.put("annotators", annotator);
            props.put("maxAdditionalKnownLCWords", "0");
            return new StanfordCoreNLP(props, false);
        }

        protected StanfordCoreNLP tokenizer = newCoreNlp("tokenize,ssplit");
        protected StanfordCoreNLP posTagger = newCoreNlp("pos");
        protected StanfordCoreNLP lemmatizer = newCoreNlp("lemma");
        protected StanfordCoreNLP neRecognizer = newCoreNlp("ner");

        public CoreDocument mkDocument(String docText, String docDate) {
            CoreDocument document =  new CoreDocument(docText);
            Annotation annotation = document.annotation();

            annotation.set(DocDateAnnotation.class, docDate);
            tokenizer.annotate(annotation);
            return document;
        }

        public ArrayList<String> annotate(CoreDocument document) {
            posTagger.annotate(document);
            lemmatizer.annotate(document);
            neRecognizer.annotate(document);

            ArrayList<String> norms = new ArrayList<>();

            for (CoreSentence sentence: document.sentences()) {
                for (CoreLabel token: sentence.tokens()) {
                    String norm = token.get(NormalizedNamedEntityTagAnnotation.class);

                    if (norm == null)
                        norms.add("O");
                    else
                        norms.add(norm);
                }

            }
            return norms;
        }
    }

    public static String text1 = "The flight follows two other Dutch-funded charters on Tuesday 18 and Wednesday 19 last week. A third charter left Yemen earlier this month.";
    public static String date1 = "2012-09-22";

    public static String text2 = "Libya has collapsed, a UNHCR spokeswoman said on Tuesday.";
    public static String date2 = "2010-06-08";

    public static void main(String[] args) {
        Processor processor = new Processor();

        // It doesn't especially matter what order these are in.
        // It is important that date1 != date2.
        CoreDocument document1a = processor.mkDocument(text1, date1);
        CoreDocument document1b = processor.mkDocument(text1, date1);
        CoreDocument document2  = processor.mkDocument(text2, date2);

        // The combination of text2 and date2 will cause SUTime to change
        // the environment, which results in expected != actual.
        ArrayList<String> expected = processor.annotate(document1a);
        processor.annotate(document2);
        ArrayList<String> actual = processor.annotate(document1b);

        if (actual.size() != expected.size())
            System.out.println("Actual and expected lengths differ: " + actual.size() + " != " + expected.size() + ".");
        else
            for (int i = 0; i < actual.size(); i++)
                if (!actual.get(i).equals(expected.get(i)))
                    System.out.println("Actual and expected differ at " + i + ": " + actual.get(i) + " != " + expected.get(i) + ".");
    }
}
