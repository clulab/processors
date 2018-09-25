//package lexicon;
package org.lemport.lemmatizer.lexicon;


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.Set;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public class Lexicon {
  private HashMap<String, HashSet<String>> lexicon = null;

  /**
   * Creates a new ...
   * 
   */
  public Lexicon() {
    lexicon = new HashMap<String, HashSet<String>>();
  }

  /**
   * Creates a new ...
   * 
   * @param  lexiconData ...
   * @throws IOException ...
   * @throws LexiconLoadException ...
   */
  public Lexicon(InputStream lexiconData)
      throws IOException, LexiconLoadException {
    this();
    this.load(lexiconData);
  }

  /**
   * This method ...
   * 
   * @param  lexiconData ...
   * @throws IOException ...
   * @throws LexiconLoadException ...
   */
  public void load(InputStream lexiconData)
      throws IOException, LexiconLoadException {
    BufferedReader reader = new BufferedReader(
        new InputStreamReader(lexiconData));
    String lemma = null;
    String partOfSpeech = null;
    String line = null;
    int lineNumber = 0;
    while ((line = reader.readLine()) != null) {
      lineNumber++;
      line = line.trim();
      lemma = new String();
      partOfSpeech = new String();
      if (line.length() > 0 && !line.startsWith("#")) {
        if (line.contains(",") && line.contains(".")
            && (line.indexOf(".") > line.indexOf(",") + 1)
            && (line.indexOf(".") + 1 < line.length())
            && (((line.charAt(line.indexOf(".") + 1)) != ':')
                || (line.charAt(line.indexOf(".") + 1)) != '+')) {
          lemma = line.substring(line.indexOf(",") + 1, line.indexOf("."));
          if (line.contains("+")) {
            partOfSpeech = line.substring(line.indexOf(".") + 1,
                line.indexOf("+"));
          }
          else if (line.contains(":")) {
            partOfSpeech = line.substring(line.indexOf(".") + 1,
                line.indexOf(":"));
          }
          else {
            partOfSpeech = line.substring(line.indexOf(".") + 1);
          }
          this.add(lemma, partOfSpeech);
        }
        else {
          reader.close();
          throw new LexiconLoadException(lineNumber + ": " + line);
        }
      }
    }
    reader.close();
  }

  /**
   * This method ...
   * 
   * @param  lexeme ...
   * @param  partOfSpeech ...
   */
  public void add(String lexeme, String partOfSpeech) {
    HashSet<String> posSet = lexicon.get(lexeme);
    if (posSet == null) {
      posSet = new HashSet<String>();
    }
    posSet.add(partOfSpeech); 
    lexicon.put(lexeme, posSet);
  }

  /**
   * This method ...
   * 
   * @param  lexeme ...
   * @return ...
   */
  public boolean contains(String lexeme) {
    return lexicon.containsKey(lexeme);
  }

  /**
   * This method ...
   * 
   * @param  lexeme ...
   * @return ...
   */
  public String[] remove(String lexeme) {
    HashSet<String> removedLexemes = lexicon.remove(lexeme);
    if (removedLexemes != null) {
      return removedLexemes.toArray(new String[removedLexemes.size()]);
    }
    else {
      return null;
    }
  }

  /**
   * This method ...
   * 
   * @param  lexeme ...
   * @param  partOfSpeech ...
   * @return ...
   */
  public String remove(String lexeme, String partOfSpeech) {
    HashSet<String> posSet = lexicon.remove(lexeme);
    HashSet<String> remainingPOS = new HashSet<String>();
    if (posSet != null) {
      for (String pos : posSet) {
        if (!pos.equals(partOfSpeech)) {
          remainingPOS.add(pos);        
        }
      }
      if (remainingPOS.size() > 0) {
        lexicon.put(lexeme, remainingPOS);
      }
      return partOfSpeech;
    }
    else {
      return null;
    }
  }

  /**
   * This method ...
   * 
   * @param  lexeme ...
   * @param  partOfSpeech ...
   * @return ...
   */
  public boolean contains(String lexeme, String partOfSpeech) {
    if (this.contains(lexeme)) {
      HashSet<String> posSet = lexicon.get(lexeme);
      for (String pos : posSet) {
        if (pos.equals(partOfSpeech)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * This method ...
   * 
   * @param  partOfSpeech ...
   * @return ...
   */
  public String[] retrieveLexemes(String partOfSpeech) {
    HashSet<String> lexemes = new HashSet<String>();
    Set<Entry<String, HashSet<String>>> entrySet = lexicon.entrySet();
    for (Entry<String, HashSet<String>> entry : entrySet) {
      if (entry.getValue().contains(partOfSpeech)) {
        lexemes.add(entry.getKey());
      }
    }
    return lexemes.toArray(new String[lexemes.size()]);
  }

  /**
   * This method ...
   * 
   * @param  lexeme ...
   * @return ...
   */
  public String[] retrievePartsOfSpeech(String lexeme) {
    HashSet<String> partsOfSpeech = new HashSet<String>();
    Set<Entry<String, HashSet<String>>> entrySet = lexicon.entrySet();
    for (Entry<String, HashSet<String>> entry : entrySet) {
      if (entry.getKey().equals(lexeme)) {
        partsOfSpeech.addAll(entry.getValue());
      }
    }
    return partsOfSpeech.toArray(new String[partsOfSpeech.size()]);
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public int size() {
    return lexicon.size();
  }
}
