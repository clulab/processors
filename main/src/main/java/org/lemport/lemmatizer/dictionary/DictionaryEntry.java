//package dictionary;
package org.lemport.lemmatizer.dictionary;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public class DictionaryEntry {
  private String inflectedForm = null;
  private String lemma = null;
  private String partOfSpeech = null;
  private String subcategory = null;
  private String morphAttributes = null;

  /**
   * Creates a new ...
   * 
   * @param  inflectedForm ...
   * @param  lemma ...
   * @param  partOfSpeech ...
   * @param  subcategory ...
   * @param  morphAttributes ...
   */
  public DictionaryEntry(String inflectedForm, String lemma,
      String partOfSpeech, String subcategory, String morphAttributes) {
    this.inflectedForm = inflectedForm;
    this.lemma = lemma;
    this.partOfSpeech = partOfSpeech;
    this.subcategory = subcategory;
    this.morphAttributes = morphAttributes;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public String getInflectedForm() {
    return inflectedForm;
  }

  /**
   * This method ...
   * 
   * @param  inflectedForm ...
   */
  public void setInflectedForm(String inflectedForm) {
    this.inflectedForm = inflectedForm;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public String getLemma() {
    return lemma;
  }

  /**
   * This method ...
   * 
   * @param  lemma ...
   */
  public void setLemma(String lemma) {
    this.lemma = lemma;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public String getPartOfSpeech() {
    return partOfSpeech;
  }

  /**
   * This method ...
   * 
   * @param  partOfSpeech ...
   */
  public void setPartOfSpeech(String partOfSpeech) {
    this.partOfSpeech = partOfSpeech;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public String getSubcategory() {
    return subcategory;
  }

  /**
   * This method ...
   * 
   * @param  subcategory ...
   */
  public void setSubcategory(String subcategory) {
    this.subcategory = subcategory;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public String getMorphAttributes() {
    return morphAttributes;
  }

  /**
   * This method ...
   * 
   * @param  morphAttributes ...
   */
  public void setMorphAttributes(String morphAttributes) {
    this.morphAttributes = morphAttributes;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result
        + ((inflectedForm == null) ? 0 : inflectedForm.hashCode());
    result = prime * result + ((lemma == null) ? 0 : lemma.hashCode());
    result = prime * result
        + ((morphAttributes == null) ? 0 : morphAttributes.hashCode());
    result = prime * result
        + ((partOfSpeech == null) ? 0 : partOfSpeech.hashCode());
    result = prime * result
        + ((subcategory == null) ? 0 : subcategory.hashCode());
    return result;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
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
    DictionaryEntry other = (DictionaryEntry) obj;
    if (inflectedForm == null) {
      if (other.inflectedForm != null) {
        return false;
      }
    }
    else if (!inflectedForm.equals(other.inflectedForm)) {
      return false;
    }
    if (lemma == null) {
      if (other.lemma != null) {
        return false;
      }
    }
    else if (!lemma.equals(other.lemma)) {
      return false;
    }
    if (morphAttributes == null) {
      if (other.morphAttributes != null) {
        return false;
      }
    }
    else if (!morphAttributes.equals(other.morphAttributes)) {
      return false;
    }
    if (partOfSpeech == null) {
      if (other.partOfSpeech != null) {
        return false;
      }
    }
    else if (!partOfSpeech.equals(other.partOfSpeech)) {
      return false;
    }
    if (subcategory == null) {
      if (other.subcategory != null) {
        return false;
      }
    }
    else if (!subcategory.equals(other.subcategory)) {
      return false;
    }
    return true;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public String toString() {
    StringBuffer entry = new StringBuffer();
    entry.append(inflectedForm);
    entry.append(",");
    entry.append(lemma);
    entry.append(".");
    entry.append(partOfSpeech);
    if (subcategory.length() > 0) {
      entry.append("+");
      entry.append(subcategory);
    }
    if (morphAttributes.length() > 0) {
      entry.append(":");
      entry.append(morphAttributes);
    }
    return entry.toString().trim();
  }
}
