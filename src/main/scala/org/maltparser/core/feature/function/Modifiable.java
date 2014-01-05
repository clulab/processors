package org.maltparser.core.feature.function;

import org.maltparser.core.exception.MaltChainedException;
/**
*
*
* @author Johan Hall
*/
public interface Modifiable extends FeatureFunction {
	/**
	 * Override the feature value of the feature function.
	 * 
	 * @param code an integer representation of the symbol
	 * @throws MaltChainedException
	 */
	public abstract void setFeatureValue(int code) throws MaltChainedException;
	/**
	 * Override the feature value of the feature function.
	 * 
	 * @param symbol an string representation of the symbol
	 * @throws MaltChainedException
	 */
	public abstract void setFeatureValue(String symbol) throws MaltChainedException;
}
