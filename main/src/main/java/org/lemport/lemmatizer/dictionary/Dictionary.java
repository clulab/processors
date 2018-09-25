//package dictionary;
package org.lemport.lemmatizer.dictionary;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;

import org.lemport.lemmatizer.lexicon.Lexicon;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public class Dictionary {
  private HashMap<String, HashSet<DictionaryEntry>> dictionary = null;

  /**
   * Creates a new ...
   * 
   */
  public Dictionary() {
    dictionary = new HashMap<String, HashSet<DictionaryEntry>>();    
  }

  /**
   * Creates a new ...
   * 
   * @param  dictionaryStream ...
   * @throws IOException ...
   * @throws DictionaryLoadException ...
   */
  public Dictionary(InputStream dictionaryStream)
      throws IOException, DictionaryLoadException {
    this();
    this.load(dictionaryStream);
  }

  /**
   * This method ...
   * 
   * @param  dictionaryStream ...
   * @throws IOException ...
   * @throws DictionaryLoadException ...
   */
  public void load(InputStream dictionaryStream)
      throws IOException, DictionaryLoadException {
    BufferedReader reader = new BufferedReader(new InputStreamReader(
        dictionaryStream));
    String inflectedForm = null;
    String lemma = null;
    String partOfSpeech = null;
    String subcategory = null;
    String morphAttributes = null;
    String line = null;
    int lineNumber = 0;
    while ((line = reader.readLine()) != null) {
      lineNumber++;
      line = line.trim();
      inflectedForm = new String();
      lemma = new String();
      partOfSpeech = new String();
      subcategory = new String();
      morphAttributes = new String();
      if (line.length() > 0 && !line.startsWith("#")) {
        if (line.contains(",") && line.contains(".")
            && (line.indexOf(".") > line.indexOf(",") + 1)
            && (line.indexOf(".") + 1 < line.length())
            && (((line.charAt(line.indexOf(".") + 1)) != ':')
                || (line.charAt(line.indexOf(".") + 1)) != '+')) {
          inflectedForm = line.substring(0, line.indexOf(","));
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
          if (line.contains("+")) {
            if (line.contains(":")) {
              subcategory = line.substring(line.indexOf("+") + 1,
                  line.indexOf(":"));
            }
            else {
              subcategory = line.substring(line.indexOf("+"));
            }
          }
          if (line.contains(":")) {
            morphAttributes = line.substring(line.indexOf(":") + 1);
          }
          this.add(new DictionaryEntry(inflectedForm, lemma, partOfSpeech,
              subcategory, morphAttributes));
        }
        else {
          reader.close();
          throw new DictionaryLoadException(lineNumber + ": " + line);
        }
      }
    }
    reader.close();
  }

  /**
   * This method ...
   * 
   * @param  entry ...
   */
  public void add(DictionaryEntry entry) {
    HashSet<DictionaryEntry> entrySet =
        dictionary.get(entry.getInflectedForm());
    if (entrySet == null) {
      entrySet = new HashSet<DictionaryEntry>();
    }
    entrySet.add(entry); 
    dictionary.put(entry.getInflectedForm(), entrySet);
  }

  /**
   * This method ...
   * 
   * @param  inflectedForm ...
   * @return ...
   */
  public DictionaryEntry[] remove(String inflectedForm) {
    HashSet<DictionaryEntry> removedEntries = dictionary.remove(inflectedForm);
    if (removedEntries != null) {
      return removedEntries.toArray(new DictionaryEntry[removedEntries.size()]);
    }
    else {
      return null;
    }
  }

  /**
   * This method ...
   * 
   * @param  inflectedForm ...
   * @param  partOfSpeech ...
   * @return ...
   */
  public DictionaryEntry[] remove(String inflectedForm, String partOfSpeech) {
    HashSet<DictionaryEntry> entrySet = dictionary.remove(inflectedForm);
    HashSet<DictionaryEntry> remainingEntries = new HashSet<DictionaryEntry>();
    HashSet<DictionaryEntry> removedEntries = new HashSet<DictionaryEntry>();
    for (DictionaryEntry entry : entrySet) {
      if (entry.getInflectedForm().equals(inflectedForm)
          && entry.getPartOfSpeech().equals(partOfSpeech)) {
        removedEntries.add(entry); 
      }
      else {
        remainingEntries.add(entry);        
      }
    }
    if (remainingEntries.size() > 0) {
      dictionary.put(inflectedForm, remainingEntries);
    }
    if (removedEntries.size() > 0) {
      return removedEntries.toArray(new DictionaryEntry[removedEntries.size()]);
    }
    else {
      return null;
    }
  }

  /**
   * This method ...
   * 
   * @param  entry ...
   * @return ...
   */
  public DictionaryEntry remove(DictionaryEntry entry) {
    HashSet<DictionaryEntry> entrySet =
        dictionary.get(entry.getInflectedForm());
    if (entrySet != null) {
      entrySet.remove(entry);
      if (entrySet.size() > 0) {
        dictionary.put(entry.getInflectedForm(), entrySet);
      }
      else {
        dictionary.remove(entry.getInflectedForm());
      }
      return entry;
    }
    else {
      return null;
    }
  }

  /**
   * This method ...
   * 
   * @param  inflectedForm ...
   * @return ...
   */
  public boolean contains(String inflectedForm) {
    return dictionary.containsKey(inflectedForm);
  }

  /**
   * This method ...
   * 
   * @param  inflectedForm ...
   * @param  partOfSpeech ...
   * @return ...
   */
  public boolean contains(String inflectedForm, String partOfSpeech) {
    if (this.contains(inflectedForm)) {
      HashSet<DictionaryEntry> entrySet = dictionary.get(inflectedForm);
      for (DictionaryEntry entry : entrySet) {
        if (entry.getPartOfSpeech().equals(partOfSpeech)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * This method ...
   * 
   * @param  entry ...
   * @return ...
   */
  public boolean contains(DictionaryEntry entry) {
    if (this.contains(entry.getInflectedForm())) {
      HashSet<DictionaryEntry> entrySet =
          dictionary.get(entry.getInflectedForm());
      return entrySet.contains(entry);
    }
    return false;
  }

  /**
   * This method ...
   * 
   * @param  inflectedForm ...
   * @param  partOfSpeech ...
   * @return ...
   */
  public String[] retrieveLemmas(String inflectedForm, String partOfSpeech) {
    HashSet<DictionaryEntry> entrySet = dictionary.get(inflectedForm);
    HashSet<String> lemmas = new HashSet<String>();
    for (DictionaryEntry entry : entrySet) {
      if (entry.getPartOfSpeech().equals(partOfSpeech)) {
        lemmas.add(entry.getLemma());
      }
    }
    return lemmas.toArray(new String[lemmas.size()]);
  }

  /**
   * This method ...
   * 
   * @param  inflectedForm ...
   * @return ...
   */
  public DictionaryEntry[] retrieveEntries(String inflectedForm) {
    HashSet<DictionaryEntry> entries = new HashSet<DictionaryEntry>();
    if (dictionary.containsKey(inflectedForm)) {
      entries = dictionary.get(inflectedForm);
    }
    return entries.toArray(new DictionaryEntry[entries.size()]);
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public DictionaryEntry[] retrieveAllEntries() {
    HashSet<DictionaryEntry> entries = new HashSet<DictionaryEntry>();
    Collection<HashSet<DictionaryEntry>> entryCollection = dictionary.values();
    for (HashSet<DictionaryEntry> entrySet : entryCollection) {
      entries.addAll(entrySet);
    }
    return entries.toArray(new DictionaryEntry[entries.size()]);
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public Lexicon retrieveLexicon() {
    Lexicon lexicon = new Lexicon();
    for (String form : dictionary.keySet()) {
      for (DictionaryEntry entry : dictionary.get(form)) {
        lexicon.add(entry.getLemma(), entry.getPartOfSpeech());
      }
    }
    return lexicon;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public int size() {
    return dictionary.size();
  }
}
