package org.clulab.processors;

import edu.stanford.nlp.ling.CoreAnnotations.CharacterOffsetBeginAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.CharacterOffsetEndAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.DocDateAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.SentenceIndexAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.NormalizedNamedEntityTagAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TextAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokenBeginAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokenEndAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.CoreDocument;
import edu.stanford.nlp.pipeline.CoreSentence;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.util.CoreMap;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

class TestProcessorEnv {

    public static class Token {
        protected String word;
        protected int start;
        protected int end;

        public Token(String word, int start, int end) {
            this.word = word;
            this.start = start;
            this.end = end;
        }
    }

    public static class DocData {
        public String docText;
        public String docDate;
        public List<String> sentTexts;
        public List<List<Token>> docTokens;
        // Calculate sentence offsets

        public DocData(String docText, String docDate, List<String> sentTexts, List<List<Token>> docTokens) {
            this.docText = docText;
            this.docDate = docDate;
            this.sentTexts = sentTexts;
            this.docTokens = docTokens;
        }

    }

    public static class Processor {

        protected static StanfordCoreNLP newCoreNlp(String annotator) {
            Properties props = new Properties();
            props.put("annotators", annotator);
            props.put("maxAdditionalKnownLCWords", "0");
            return new StanfordCoreNLP(props, false);
        }

//        protected static StanfordCoreNLP newTokenizer() {
//            Properties props = new Properties();
//            props.put("annotators", "tokenize,ssplit");
//            return new StanfordCoreNLP(props, false);
//        }

        protected StanfordCoreNLP tokenizer = newCoreNlp("tokenize,ssplit");
        protected StanfordCoreNLP posTagger = newCoreNlp("pos");
        protected StanfordCoreNLP lemmatizer = newCoreNlp("lemma");
        protected StanfordCoreNLP neRecognizer = newCoreNlp("ner");

        public Annotation mkAnnotation(DocData docData) {
            ArrayList<CoreLabel> docCoreLabels = new ArrayList<>();
            ArrayList<CoreMap> sentAnnotations = new ArrayList<>();

            Annotation docAnnotation = new Annotation(docData.docText);
            docAnnotation.set(TextAnnotation.class, docData.docText);
            docAnnotation.set(DocDateAnnotation.class, docData.docDate);
            docAnnotation.set(TokensAnnotation.class, docCoreLabels);
            docAnnotation.set(SentencesAnnotation.class, sentAnnotations);

            for (int sentIndex = 0; sentIndex < docData.docTokens.size(); sentIndex++) {
                List<Token> sentTokens = docData.docTokens.get(sentIndex);
                List<CoreLabel> sentCoreLabels = new ArrayList<>();

                Annotation sentAnnotation = new Annotation(docData.sentTexts.get(sentIndex));
                sentAnnotations.add(sentAnnotation);
                sentAnnotation.set(SentenceIndexAnnotation.class, sentIndex); // Stanford counts sentences starting from 0.
                sentAnnotation.set(TextAnnotation.class, docData.sentTexts.get(sentIndex));
                sentAnnotation.set(TokensAnnotation.class, sentCoreLabels);

                for (int tokenIndex = 0; tokenIndex < sentTokens.size(); tokenIndex++) {
                    Token token = sentTokens.get(tokenIndex);
                    CoreLabel coreLabel = new CoreLabel();
                    sentCoreLabels.add(coreLabel);

                    coreLabel.setWord(token.word);
                    coreLabel.setValue(token.word);
                    coreLabel.setBeginPosition(token.start);
                    coreLabel.setEndPosition(token.end);
                    coreLabel.setIndex(tokenIndex + 1); // Stanford counts tokens starting from 1
                    coreLabel.setSentIndex(sentIndex); // Stanford counts sentences starting from 0.
                }

                docCoreLabels.addAll(sentCoreLabels);
                int sentStartOffset = 4;
                int sentEndOffset = 6;
                sentAnnotation.set(CharacterOffsetBeginAnnotation.class, sentStartOffset);
                sentAnnotation.set(CharacterOffsetEndAnnotation.class, sentEndOffset);

                int tokenOffset = 5;
                sentAnnotation.set(TokenBeginAnnotation.class, tokenOffset);
                tokenOffset += 6;
                sentAnnotation.set(TokenEndAnnotation.class, tokenOffset);
            }
            return docAnnotation;
        }

        public CoreDocument mkDocument(String docText, String docDate) {
            CoreDocument document =  new CoreDocument(docText);
            Annotation annotation = document.annotation();

            annotation.set(DocDateAnnotation.class, docDate);
            tokenizer.annotate(annotation);
            return document;
        }

        public ArrayList<String> annotateMore(CoreDocument document) {
            posTagger.annotate(document);
            lemmatizer.annotate(document);
            neRecognizer.annotate(document);

            ArrayList<String> norms = new ArrayList<>();

            for (CoreSentence sentence: document.sentences()) {
                for (CoreLabel token: sentence.tokens()) {
//                    norms.add(token.ner());
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

    public static void main2(String[] args) {
        Processor processor = new Processor();

        CoreDocument document1a = processor.mkDocument(text1, date1);
        CoreDocument document2  = processor.mkDocument(text2, date2);
        CoreDocument document1b = processor.mkDocument(text1, date1);

        ArrayList<String> expected = processor.annotateMore(document1a);
        processor.annotateMore(document2);
        ArrayList<String> actual = processor.annotateMore(document1b);

        if (actual.size() != expected.size())
            System.out.println("Actual and expected lengths differed.");
        else
            for (int i = 0; i < actual.size(); i++)
                if (!actual.get(i).equals(expected.get(i)))
                    System.out.println("Actual and expected differed at " + i + ": " + actual.get(i) + " != " + expected.get(i));
    }

    public static void main1(String[] args) {
        Processor processor = new Processor();

//        Annotation document1a = processor.mkAnnotation(text1, date1);
//        Annotation document1b = processor.mkAnnotation(text1, date1);
//        Annotation document2  = processor.mkAnnotation(text2, date2);

//        String[] expected = processor.annotate(document1a);
//        processor.annotate(document2);
//        String[] actual = processor.annotate(document1b);

//        if (expected != actual)
            System.out.println("It failed!");
    }

    public static void main(String[] args) {
        main2(args);
    }
}
