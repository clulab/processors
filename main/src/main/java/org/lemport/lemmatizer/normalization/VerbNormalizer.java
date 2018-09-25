//package normalization;
package org.lemport.lemmatizer.normalization;

import java.util.Arrays;
import java.util.regex.Pattern;

import org.lemport.lemmatizer.replacement.Replacement;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public class VerbNormalizer extends Normalizer {
  private Pattern[] conjugationTargetsWithoutPrefixes = null;
  private Pattern[] conjugationTargets = null;
  private Pattern[] conjugationTags = null;
  private Pattern[] lexemeTargetsWithoutPrefixes = null;
  private Pattern[] lexemeTargets = null;
  private Pattern[] lexemeTags = null;
  private Pattern[] declensionExceptions = null;
  private Pattern[] declensionTargets = null;
  private Pattern[] declensionTags = null;
  private Replacement[] conjugations = null;
  private Replacement[] lexemes = null;
  private Replacement[] declensions = null;

  /**
   * Creates a new <code>VerbNormalizer</code> object ...
   * 
   * @param  conjugations ...
   * @param  lexemes ...
   * @param  declensions ...
   */
  public VerbNormalizer(Replacement[] conjugations, Replacement[] lexemes,
      Replacement[] declensions) {
    this.conjugations = conjugations;
    this.lexemes = lexemes;
    this.declensions = declensions;
    Arrays.sort(this.conjugations);
    Arrays.sort(this.lexemes);
    Arrays.sort(this.declensions);
    conjugationTargetsWithoutPrefixes = new Pattern[this.conjugations.length];
    conjugationTargets = new Pattern[this.conjugations.length];
    conjugationTags = new Pattern[this.conjugations.length];
    for (int i = 0; i < conjugations.length; i++) {
      conjugationTargetsWithoutPrefixes[i] = Pattern.compile(
          conjugations[i].getTarget() + conjugations[i].getSuffix());
      conjugationTargets[i] = Pattern.compile(conjugations[i].getPrefix()
          + conjugations[i].getTarget() + conjugations[i].getSuffix());
      conjugationTags[i] = Pattern.compile(conjugations[i].getTag());
    }
    lexemeTargetsWithoutPrefixes = new Pattern[this.lexemes.length];
    lexemeTargets = new Pattern[this.lexemes.length];
    lexemeTags = new Pattern[this.lexemes.length];
    for (int i = 0; i < lexemes.length; i++) {
      lexemeTargetsWithoutPrefixes[i] = Pattern.compile(
          lexemes[i].getTarget() + lexemes[i].getSuffix());
      lexemeTargets[i] = Pattern.compile(lexemes[i].getPrefix()
          + lexemes[i].getTarget() + lexemes[i].getSuffix());
      lexemeTags[i] = Pattern.compile(lexemes[i].getTag());
    }
    declensionExceptions = new Pattern[this.declensions.length];
    declensionTargets = new Pattern[this.declensions.length];
    declensionTags = new Pattern[this.declensions.length];
    for (int i = 0; i < declensions.length; i++) {
      declensionExceptions[i] = Pattern.compile(declensions[i].getExceptions());
      declensionTargets[i] = Pattern.compile(declensions[i].getPrefix()
          + declensions[i].getTarget() + declensions[i].getSuffix());
      declensionTags[i] = Pattern.compile(declensions[i].getTag());
    }
  }

  /**
   * This method retrieves the infinitive form of a given verb, if it
   * exists, when classified with a given <em>PoS tag</em>. Otherwise, it
   * returns the same token (in lower case).
   *
   * @param  token the token whose lemma is wanted
   * @param  tag the <em>PoS tag</em> of the token
   * @return the infinitive form of the token (when with the given tag)
   */
  public String normalize(String token, String tag) {
    String currentNormalization = token.toLowerCase();
    String normalization = currentNormalization;
    boolean matchFound = false;

    // from inflections to lemmas (without prefixes)
    if (!matchFound) {
      for (int i = 0; i < conjugations.length; i++) {
        if (conjugationTargetsWithoutPrefixes[i].matcher(
            currentNormalization).matches()
            && conjugationTags[i].matcher(tag.toLowerCase()).matches()) {
          if (currentNormalization.split(
              conjugations[i].getTarget()).length > 0) {
            String verbPrefix = token.split(conjugations[i].getTarget())[0];
            normalization = verbPrefix + conjugations[i].getReplacement();
          }
          else {
            normalization = conjugations[i].getReplacement();
          }
          matchFound = true;
          break;
        }
      }
    }

    // from inflections to lemmas (with prefixes)
    if (!matchFound) {
      for (int i = 0; i < conjugations.length; i++) {
        if (conjugationTargets[i].matcher(currentNormalization).matches()
            && conjugationTags[i].matcher(tag.toLowerCase()).matches()) {
          // check whether the current inflection is from a verb with a prefix
          // and appends it (the prefix) to the replacement
          if (currentNormalization.split(
              conjugations[i].getTarget()).length > 0) {
            String verbPrefix = token.split(conjugations[i].getTarget())[0];
            normalization = verbPrefix + conjugations[i].getReplacement();
          }
          else {
            normalization = conjugations[i].getReplacement();
          }
          matchFound = true;
          break;
        }
      }
    }

    // from lexemes to lemmas (without prefixes)
    if (!matchFound) {
      for (int i = 0; i < lexemes.length; i++) {
        if (lexemeTargetsWithoutPrefixes[i].matcher(
            currentNormalization).matches()
            && lexemeTags[i].matcher(tag.toLowerCase()).matches()) {
          if (currentNormalization.split(lexemes[i].getTarget()).length > 0) {
            String verbPrefix = token.split(lexemes[i].getTarget())[0];
            normalization = verbPrefix + lexemes[i].getReplacement();
          }
          else {
            normalization = lexemes[i].getReplacement();
          }
          matchFound = true;
          break;
        }
      }
    }

    // from lexemes to lemmas (with prefixes)
    if (!matchFound) {
      for (int i = 0; i < lexemes.length; i++) {
        if (lexemeTargets[i].matcher(currentNormalization).matches()
            && lexemeTags[i].matcher(tag.toLowerCase()).matches()) {
          // check whether the current lexeme is from a verb with a prefix
          // and appends it (the prefix) to the replacement
          if (currentNormalization.split(lexemes[i].getTarget()).length > 0) {
            String verbPrefix = token.split(lexemes[i].getTarget())[0];
            normalization = verbPrefix + lexemes[i].getReplacement();
          }
          else {
            normalization = lexemes[i].getReplacement();
          }
          matchFound = true;
          break;
        }
      }
    }

    // from declensions to lemmas (when everything else fails)
    if (!matchFound) {
      for (int i = 0; i < declensions.length; i++) {
        if (declensionTargets[i].matcher(normalization).matches()
            && declensionTags[i].matcher(tag.toLowerCase()).matches()
            && !declensionExceptions[i].matcher(normalization).matches()) {
          normalization = normalization.substring(0,
              normalization.length() - declensions[i].getTarget().length())
              + declensions[i].getReplacement();
          matchFound = true;
          break;
        }
      }
    }

    return normalization;
  }
}
