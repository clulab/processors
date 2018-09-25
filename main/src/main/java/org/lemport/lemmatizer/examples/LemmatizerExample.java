//package examples;
package org.lemport.lemmatizer.examples;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.InvalidPropertiesFormatException;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import org.lemport.lemmatizer.rank.WordRankingLoadException;
import org.lemport.lemmatizer.dictionary.DictionaryLoadException;
import org.lemport.lemmatizer.lemma.LemmatizeException;
import org.lemport.lemmatizer.lemma.Lemmatizer;

public class LemmatizerExample {

  public static void main(String[] args) {
    System.out.println("Starting...");
    Lemmatizer lemmatizer = null;
    String[] tokens = {"Era", "uma", "vez", "um", "gato", "maltês", ",",
        "tocava", "piano", "e", "falava", "francês", "."};
    String[] tags = {"VERBF", "pron-det", "NOUN", "DET", "NOUN", "ADJ", "PUNCT", "VERBF",
        "NOUN", "CCONJ", "VERBF", "NOUN", "PUNCT"};
    String[] lemmas = null;

    try {
      lemmatizer = new Lemmatizer();
    }
    catch (NumberFormatException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch (FileNotFoundException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch (InvalidPropertiesFormatException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch (ParserConfigurationException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch (SAXException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch (WordRankingLoadException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch (DictionaryLoadException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    try {
      lemmas = lemmatizer.lemmatize(tokens, tags);
    }
    catch (LemmatizeException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    StringBuffer buffer = new StringBuffer();

    for (int i = 0; i < tokens.length; i++) {
      buffer.append(tokens[i] + "#" + tags[i] + ":" + lemmas[i] + " ");
    }

    System.out.println(buffer.toString().trim());

    System.out.println("Done!");
  }
}
