//package lemma;
package org.lemport.lemmatizer.lemma;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * This class ...
 *
 * @author   Ricardo Rodrigues
 * @version  0.9.9
 */
public class LemmatizerCache extends LinkedHashMap<LemmatizerCacheKey, String> {
  private static final long serialVersionUID = 1l;
  private int limit = 0;

  /**
   * Creates a new ...
   * 
   * @param  limit ...
   */
  public LemmatizerCache(int limit) {
    super(16, 0.75f, true);
    this.limit = limit;
  }

  /**
   * Creates a new ...
   * 
   * @param  initialCapacity ...
   * @param  limit ...
   */
  public LemmatizerCache(int initialCapacity, int limit) {
    super(initialCapacity, 0.75f, true);
    this.limit = limit;
  }

  /**
   * Creates a new ...
   * 
   * @param  initialCapacity ...
   * @param  loadFactor ...
   * @param  limit ...
   */
  public LemmatizerCache(int initialCapacity, float loadFactor, int limit) {
    super(initialCapacity, loadFactor, true);
    this.limit = limit;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  protected boolean removeEldestEntry(
      Map.Entry<LemmatizerCacheKey, String> eldest) {
    return size() > limit;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public int getLimit() {
    return limit;
  }

  /**
   * This method ...
   * 
   * @param limit ...
   */
  public void setLimit(int limit) {
    this.limit = limit;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public boolean isFull() {
    return size() >= limit;
  }

  /**
   * This method ...
   * 
   * @return ...
   */
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + limit;
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
    if (!super.equals(obj)) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    LemmatizerCache other = (LemmatizerCache) obj;
    if (limit != other.limit) {
      return false;
    }
    return true;
  }
}
