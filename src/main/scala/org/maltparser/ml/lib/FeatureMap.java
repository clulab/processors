package org.maltparser.ml.lib;

import java.io.Serializable;


import org.maltparser.core.helper.HashMap;

/**
 * The purpose of the feature map is to map MaltParser's column based features together with the symbol code from the symbol table to 
 * unique indices suitable for liblinear and libsvm.  A feature column position are combined together with the symbol code in a 
 * 64-bit key (Long), where 16 bits are reserved for the position and 48 bits are reserved for the symbol code.  
 * 
 * @author Johan Hall
 *
 */
public class FeatureMap  implements Serializable {
	private static final long serialVersionUID = 7526471155622776147L;
	private final HashMap<Long,Integer> map;
	private int featureCounter;
	
	/**
	 * Creates a feature map and sets the feature counter to 1
	 */
	public FeatureMap() {
		map = new HashMap<Long, Integer>();
		this.featureCounter = 1;
	}
	
	/**
	 * Adds a mapping from a combination of the position in the column-based feature vector and the symbol code to 
	 * an index value suitable for liblinear and libsvm.
	 * 
	 * @param featurePosition a position in the column-based feature vector
	 * @param code a symbol code
	 * @return the index value 
	 */
	public int addIndex(int featurePosition, int code) {
		final long key = ((((long)featurePosition) << 48) | (long)code);
		Integer index = map.get(key);
		if (index == null) {
			index = featureCounter++;
			map.put(key, index);
		}
		return index.intValue();
	}
	
	/**
	 * Return
	 * 
	 * @param featurePosition the position in the column-based feature vector
	 * @param code the symbol code suitable for liblinear and libsvm
	 * @return the index value if it exists, otherwise -1
	 */
	public int getIndex(int featurePosition, int code) {
		final Integer index = map.get(((((long)featurePosition) << 48) | (long)code));
		return (index == null)?-1:index;
	}
	
	
	public int addIndex(int featurePosition, int code1, int code2) {
		final long key = ((((long)featurePosition) << 48) | (((long)code1) << 24) | (long)code2);
		Integer index = map.get(key);
		if (index == null) {
			index = featureCounter++;
			map.put(key, index);
		}
		return index.intValue();
	}
	
	public int setIndex(long key, int index) {
		return map.put(key, index);
	}
	
	public int decrementIndex(Long key) {
		Integer index = map.get(key);
		if (index != null) {
			map.put(key, index - 1);
		}
		return (index != null)?index - 1 : -1;
	}
	
	public void decrementfeatureCounter() {
		featureCounter--;
	}
	
	public Integer removeIndex(long key) {
		return map.remove(key);
	}
	
	public int getIndex(int featurePosition, int code1, int code2) {
		final Integer index = map.get(((((long)featurePosition) << 48) | (((long)code1) << 24) | (long)code2));
		return (index == null)?-1:index;
	}
		
	/**
	 * @return the size of the map
	 */
	public int size() {
		return map.size();
	}
	
	
	public Long[] reverseMap() {
		Long[] reverseMap = new Long[map.size() +1];

		for (Long key : map.keySet()) {
			reverseMap[map.get(key)] = key;
		}
		return reverseMap;
	}
	
	
	
	public void setFeatureCounter(int featureCounter) {
		this.featureCounter = featureCounter;
	}

	/**
	 * @return the current value of the feature counter.
	 */
	public int getFeatureCounter() {
		return featureCounter;
	}
}
