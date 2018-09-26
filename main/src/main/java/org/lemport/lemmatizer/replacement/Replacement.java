//package replacement;
package org.lemport.lemmatizer.replacement;


import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public class Replacement implements Comparable<Replacement> {
  private String target = null;
  private String tag = null;
  private String prefix = null;
  private String suffix = null;
  private String exceptions = null;
  private String replacement = null;

  /**
   * Creates a new <code>Replacement</code> object ...
   *
   * @param prefix ...
   * @param  target ...
   * @param  suffix ...
   * @param  tag ...
   * @param  exceptions ...
   * @param  replacement ...
   */
  public Replacement(String prefix, String target, String suffix, String tag,
      String exceptions, String replacement) {
    this.prefix = prefix;
    this.target = target;
    this.suffix = suffix;
    this.tag = tag;
    this.exceptions = exceptions;
    this.replacement = replacement;
  }

  /**
   * This method ...
   * 
   * @return the exceptions
   */
  public String getExceptions() {
    return exceptions;
  }

  /**
   * This method ...
   * 
   * @param exceptions the exceptions to set
   */
  public void setExceptions(String exceptions) {
    this.exceptions = exceptions;
  }

  /**
   * Returns the target of this <code>Replacement</code>.
   *
   * @return the target of the <code>Replacement</code>
   */
  public String getTarget() {
    return target;
  }


  /**
   * Sets the target for this <code>Replacement</code>.
   *
   * @param  target the target for the <code>Replacement</code>
   */
  public void setTarget(String target) {
    this.target = target;
  }

  /**
   * Returns the tag of this <code>Replacement</code>.
   *
   * @return the tag of the <code>Replacement</code>
   */
  public String getTag() {
    return tag;
  }

  /**
   * Sets the tag of this <code>Replacement</code>.
   *
   * @param  tag the tag of the <code>Replacement</code>
   */
  public void setTag(String tag) {
    this.tag = tag;
  }

  /**
   * Returns the prefix of this <code>Replacement</code>.
   * 
   * @return the prefix of the <code>Replacement</code>
   */
  public String getPrefix() {
    return prefix;
  }

  /**
   * Sets the prefix of this <code>Replacement</code>.
   * 
   * @param  prefix the prefix of the <code>Replacement</code>
   */
  public void setPrefix(String prefix) {
    this.prefix = prefix;
  }

  /**
   * Returns the suffix of this <code>Replacement</code>.
   * 
   * @return the suffix of the <code>Replacement</code>
   */
  public String getSuffix() {
    return suffix;
  }

  /**
   * Sets the suffix of this <code>Replacement</code>.
   * 
   * @param  suffix the suffix of the <code>Replacement</code>
   */
  public void setSuffix(String suffix) {
    this.suffix = suffix;
  }

  /**
   * Returns the replacement of this <code>Replacement</code>.
   *
   * @return the replacement of the <code>Replacement</code>
   */
  public String getReplacement() {
    return replacement;
  }

  /**
   * Sets the replacement of this <code>Replacement</code>.
   *
   * @param  replacement the replacement of the <code>Replacement</code>
   */
  public void setReplacement(String replacement) {
    this.replacement = replacement;
  }

  /**
   * Returns a hash code for this <em>replacement</em>.
   *
   * @return a hash code value for this object
   */
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result
        + ((exceptions == null) ? 0 : exceptions.hashCode());
    result = prime * result + ((prefix == null) ? 0 : prefix.hashCode());
    result = prime * result
        + ((replacement == null) ? 0 : replacement.hashCode());
    result = prime * result + ((suffix == null) ? 0 : suffix.hashCode());
    result = prime * result + ((tag == null) ? 0 : tag.hashCode());
    result = prime * result + ((target == null) ? 0 : target.hashCode());
    return result;
  }

  /**
   * Checks if this <code>Replacement</code> is equal to another one.
   *
   * @param  obj the other <code>Object</code> (<code>Replacement</code>)
   *         to be compared with this one
   * @return the value of the equality (<code>true</code> or
   *         <code>false</code>)
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
    Replacement other = (Replacement) obj;
    if (exceptions == null) {
      if (other.exceptions != null) {
        return false;
      }
    }
    else if (!exceptions.equals(other.exceptions)) {
      return false;
    }
    if (prefix == null) {
      if (other.prefix != null) {
        return false;}
    }
    else if (!prefix.equals(other.prefix))
      return false;
    if (replacement == null) {
      if (other.replacement != null) {
        return false;
      }
    }
    else if (!replacement.equals(other.replacement)) {
      return false;
    }
    if (suffix == null) {
      if (other.suffix != null) {
        return false;
      }
    }
    else if (!suffix.equals(other.suffix)) {
      return false;
    }
    if (tag == null) {
      if (other.tag != null) {
        return false;
      }
    }
    else if (!tag.equals(other.tag)) {
      return false;
    }
    if (target == null) {
      if (other.target != null) {
        return false;
      }
    }
    else if (!target.equals(other.target)) {
      return false;
    }
    return true;
  }

  /**
   * Compares two <em>replacements</em>, using the <em>length</em> value of
   * each of the <em>strings</em> that represent the attributes of the
   * <em>replacements</em>. The result is zero if the <em>replacements</em>
   * have the same length. The length of the target is one thousand times more
   * important than the length of the replacement, the length of the tag
   * (along with its prefix and suffix) and the length of the exceptions. This
   * method is needed for sorting the order by which the targets are matched
   * against the tokens, insuring that the larger (and more specific) are
   * tested first.
   *
   * @param  other the other <code>Replacement</code> to be
   *         compared to this one
   * @return the value the corresponding to the lexicographic difference
   *         between the attributes of both <em>replacements</em>
   */
  public int compareTo(Replacement other) {
    // it must be used a "normalized" length of the targets to circumvent
    // situations where the use of regular expressions on them may alter the
    // intended order of both targets
    // as such, for now, all contents between squared brackets are reduced
    // to one unit in length, affecting thus the overall length of the targets
    int totalLength = this.computeNormalizedTargetLength(target) * 1000
        + exceptions.length() + replacement.length() + prefix.length()
        + tag.length() + suffix.length();
    int otherTotalLength =
        this.computeNormalizedTargetLength(other.getTarget()) * 1000
        + other.getExceptions().length() + other.getReplacement().length()
        + other.getPrefix().length() + other.getTag().length()
        + other.getSuffix().length();
    if (totalLength < otherTotalLength) {
      return 1;
    }
    else if (totalLength > otherTotalLength) {
      return -1;
    }
    else {
      return 0;
    }
  }

  /**
   * Creates and returns a copy of this object.
   *
   * @return a clone of this instance
   */
  public Replacement clone() {
    return new Replacement(prefix, target, suffix, tag, exceptions,
        replacement);
  }

  /**
   * This method ...
   * 
   * @param  replacementInput ...
   * @return ...
   * @throws ParserConfigurationException ...
   * @throws SAXException ...
   * @throws IOException ...
   */
  public static Replacement[] readReplacements(InputStream replacementInput)
      throws ParserConfigurationException, SAXException, IOException {
    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
    DocumentBuilder db = dbf.newDocumentBuilder();
    Document doc = db.parse(replacementInput);
    doc.getDocumentElement().normalize();
    String replacement = null;
    String exceptions = null;
    String target = null;
    String tag = null;
    String prefix = null;
    String suffix = null;
    NodeList entries = doc.getElementsByTagName("prefix");
    if (entries.getLength() > 0) {
      prefix = entries.item(0).getTextContent();
    }
    else {
      prefix = new String();
    }
    entries = doc.getElementsByTagName("suffix");
    if (entries.getLength() > 0) {
      suffix = entries.item(0).getTextContent();
    }
    else {
      suffix = new String();
    }
    entries = doc.getElementsByTagName("replacement");
    Node entry = null;
    Replacement[] replacements = new Replacement[entries.getLength()];
    for (int i = 0; i < entries.getLength(); i++) {
      entry = entries.item(i);
      replacement = entry.getTextContent();
      exceptions = ((Element) entry).getAttribute("exceptions");
      target = ((Element) entry).getAttribute("target");
      tag = ((Element) entry).getAttribute("tag");
      replacements[i] = new Replacement(prefix, target, suffix, tag, exceptions,
          replacement);
    }
    return replacements;
  }

  private int computeNormalizedTargetLength(String target) {
    int normalizedLength = target.length();
    Matcher matcher = Pattern.compile("\\[.*?\\]").matcher(target);
    while (matcher.find()) {
      normalizedLength -= matcher.group().trim().length() - 1;
    }
    return normalizedLength;
  }

  public static String fromPalavrasToUDTagset(String token, String word){
    Map<String, String> conversionTable = new HashMap<String, String>();

    // convert articles to articles

    conversionTable.put("X", "?");
    conversionTable.put("ADJ", "adj");
    conversionTable.put("ADV", "adv");
    conversionTable.put("SCONJ", "conj-s");
    conversionTable.put("CCONJ", "conj-c");
    conversionTable.put("PART", "ec");
    conversionTable.put("INTJ", "in");
    conversionTable.put("NOUN", "n");
    conversionTable.put("NUM", "num");
    conversionTable.put("ADV", "pp");
    conversionTable.put("DET", "pron-det");
    conversionTable.put("PRON", "pron");
    conversionTable.put("PROPN", "prop");
    conversionTable.put("ADP", "prp");
    conversionTable.put("PUNCT", "punc");
    conversionTable.put("VERBF", "v-fin");
    conversionTable.put("VERBG", "v-ger");
    conversionTable.put("VERBI", "v-inf");
    conversionTable.put("VERBP", "v-pcp");

    return conversionTable.get(token);

    //return tag;
  }
}
