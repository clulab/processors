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
public class GenderNameNormalizer extends Normalizer {
  private Pattern[] nameTargets = null;
  private Pattern[] nameTags = null;
  private Replacement[] names = null;

  /**
   * Creates a new <code>GenderNormalizer</code> object ...
   * 
   * @param  names ...
   */
  public GenderNameNormalizer(Replacement[] names) {
    this.names = names;
    Arrays.sort(this.names);
    nameTargets = new Pattern[this.names.length];
    nameTags = new Pattern[this.names.length];
    for (int i = 0; i < names.length; i++) {
      nameTargets[i] = Pattern.compile(names[i].getPrefix() +
          names[i].getTarget() + names[i].getSuffix());
      nameTags[i] = Pattern.compile(names[i].getTag());
    }
  }

  /**
   * This method retrieves the masculine form of a given token, if it exists,
   * when classified with a given <em>PoS tag</em>. Otherwise, it retrieves
   * the same token (in lower case).
   *
   * @param  token the token whose lemma is wanted
   * @param  tag the <em>PoS tag</em> of the token
   * @return the masculine form of the token (when with the given tag)
   */
  public String normalize(String token, String tag) {
    String normalization = token.toLowerCase();
    // using gender specific nouns
    for (int i = 0; i < names.length; i++) {
      if (nameTargets[i].matcher(normalization).matches()
          && nameTags[i].matcher(tag.toLowerCase()).matches()) {
        normalization = names[i].getReplacement();
        break;
      }
    }
    return normalization;
  }
}
