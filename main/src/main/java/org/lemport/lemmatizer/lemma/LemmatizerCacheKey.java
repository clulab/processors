//package lemma;
package org.lemport.lemmatizer.lemma;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public class LemmatizerCacheKey {
  private String token = null;
  private String tag = null;

  /**
   * Creates a new ...
   * 
   * @param  token ...
   * @param  tag ...
   */
  public LemmatizerCacheKey(String token, String tag) {
    this.token = token;
    this.tag = tag;
  }

  /**
   * Creates a new ...
   * 
   * @return ...
   */
  public String getToken() {
    return token;
  }

  /**
   * Creates a new ...
   * 
   * @param  token ...
   */
  public void setToken(String token) {
    this.token = token;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public String getTag() {
    return tag;
  }

  /**
   * This method ...
   * 
   * @param  tag ...
   */
  public void setTag(String tag) {
    this.tag = tag;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((tag == null) ? 0 : tag.hashCode());
    result = prime * result + ((token == null) ? 0 : token.hashCode());
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
    LemmatizerCacheKey other = (LemmatizerCacheKey) obj;
    if (tag == null) {
      if (other.tag != null) {
        return false;
      }
    }
    else if (!tag.equals(other.tag)) {
      return false;
    }
    if (token == null) {
      if (other.token != null) {
        return false;
      }
    }
    else if (!token.equals(other.token)) {
      return false;
    }
    return true;
  }
}
