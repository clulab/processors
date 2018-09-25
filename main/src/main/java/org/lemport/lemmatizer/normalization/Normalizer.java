//package normalization;
package org.lemport.lemmatizer.normalization;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public abstract class Normalizer {

  /**
   * This method retrieves the &quot;normal&quot; form of a token, if it
   * exists, when classified with a given <em>PoS tag</em>. Otherwise, it
   * returns the same token (in lower case).
   *
   * @param  token the token whose lemma is wanted
   * @param  tag the <em>PoS tag</em> of the token
   * @return the normal form of the token (when with the given tag)
   */
  public abstract String normalize(String token, String tag);
}
