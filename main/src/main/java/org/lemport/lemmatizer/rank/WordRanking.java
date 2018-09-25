//package rank;
package org.lemport.lemmatizer.rank;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public class WordRanking {
  private HashMap<String, WordListEntry> wordMap = null;

  /**
   * Creates a new ...
   * 
   */
  public WordRanking() {
    wordMap = new HashMap<String, WordListEntry>();    
  }

  /**
   * Creates a new ...
   * 
   * @param  dictionaryData ...
   * @throws NumberFormatException ...
   * @throws IOException ...
   * @throws WordRankingLoadException ...
   */
  public WordRanking(InputStream dictionaryData)
      throws NumberFormatException, IOException, WordRankingLoadException {
    this();
    this.load(dictionaryData);
  }

  /**
   * This method ...
   * 
   * @param  dictionaryData ...
   * @throws NumberFormatException ...
   * @throws IOException ...
   * @throws WordRankingLoadException ...
   */
  public void load(InputStream dictionaryData)
      throws NumberFormatException, IOException, WordRankingLoadException {
    BufferedReader reader = new BufferedReader(
        new InputStreamReader(dictionaryData));
    int rank = 0;
    int frequency = 0;
    String word = null;
    String line = null;
    int lineNumber = 0;
    while ((line = reader.readLine()) != null) {
      lineNumber++;
      line = line.trim();
      if  (line.length() > 0 && !line.startsWith("#")) {
        if (line.contains("\t") && (line.indexOf("\t") + 1 < line.length())) {
          word = line.substring(line.indexOf("\t") + 1).replace("=", " ");
          frequency = Integer.parseInt(line.substring(0, line.indexOf("\t")));
          rank++;
          wordMap.put(word, new WordListEntry(word, frequency, rank));
        }
        else {
          reader.close();
          throw new WordRankingLoadException(lineNumber + ": " + line);
        }
      }
    }
    reader.close();
  }

  /**
   * This method ...
   * 
   * @param  word ...
   * @return ...
   */
  public int getFrequency(String word) {
    if (wordMap.get(word) != null) {
      return wordMap.get(word).getFrequency();
    }
    else {
      return -1;
    }
  }

  /**
   * This method ...
   * 
   * @param  word ...
   * @return ...
   */
  public int getRank(String word) {
    if (wordMap.get(word) != null) {
      return wordMap.get(word).getRank();
    }
    else {
      return -1;
    }
  }

  /**
   * This method ...
   * 
   * @param  words ...
   * @return ...
   */
  public String[] rank(String[] words) {
    ArrayList<WordListEntry> rankedList = new ArrayList<WordListEntry>();
    for (String word : words) {
      rankedList.add(new WordListEntry(word, this.getFrequency(word),
          this.getRank(word)));
    }
    Collections.sort(rankedList);
    String[] rankedWords = new String[rankedList.size()];
    for (int i = 0; i < rankedWords.length; i++) {
      rankedWords[i] = rankedList.get(i).getWord();
    }
    return rankedWords;
  }

  /**
   * This method ...
   * 
   * @param  words ...
   * @param  limit ...
   * @return ...
   */
  public String[] rank(String[] words, int limit) {
    ArrayList<WordListEntry> rankedList = new ArrayList<WordListEntry>();
    for (String word : words) {
      rankedList.add(new WordListEntry(word, this.getFrequency(word),
          this.getRank(word)));
    }
    Collections.sort(rankedList);
    int length = rankedList.size();
    if (length > limit) {
      length = limit;
    }
    String[] rankedWords = new String[length];
    for (int i = 0; i < rankedWords.length; i++) {
      rankedWords[i] = rankedList.get(i).getWord();
    }
    return rankedWords;
  }

  /**
   * This method ...
   * 
   * @param  words ...
   * @return ...
   */
  public String retrieveTopWord(String[] words) {
    String topWord = null;
    if (words.length > 0) {
      topWord = this.rank(words, 1)[0];
    }
    return topWord;
  }

  //////////////////////////////////////////////////////////////////////////////

  private class WordListEntry implements Comparable<WordListEntry> {
    private String word = null;
    private int frequency = 0;
    private int rank = 0;

    public WordListEntry(String word, int frequency, int rank) {
      this.word = word;
      this.frequency = frequency;
      this.rank = rank;
    }

    public String getWord() {
      return word;
    }

    public int getFrequency() {
      return frequency;
    }

    public int getRank() {
      return rank;
    }

    public String toString() {
      return new String(rank + "\t"+ frequency + "\t"+ word);
    }

    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + getOuterType().hashCode();
      result = prime * result + frequency;
      result = prime * result + rank;
      result = prime * result + ((word == null) ? 0 : word.hashCode());
      return result;
    }

    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      WordListEntry other = (WordListEntry) obj;
      if (!getOuterType().equals(other.getOuterType())) {
        return false;
      }
      if (frequency != other.frequency) {
        return false;
      }
      if (rank != other.rank) {
        return false;
      }
      if (word == null) {
        if (other.word != null) {
          return false;
        }
      }
      else if (!word.equals(other.word)) {
        return false;
      }
      return true;
    }

    private WordRanking getOuterType() {
      return WordRanking.this;
    }

    public int compareTo(WordListEntry other) {
      if (this.frequency < other.getFrequency()) {
        return 1;
      }
      else if (this.frequency > other.getFrequency()) {
        return -1;
      }
      else {
        if (this.rank < other.getRank()) {
          return -1;
        }
        else if (this.rank > other.getRank()) {
          return 1;
        }
        else {
          return 0;
        }
      }
    }
  }
}
