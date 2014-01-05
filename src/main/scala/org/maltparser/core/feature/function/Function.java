package org.maltparser.core.feature.function;

import org.maltparser.core.exception.MaltChainedException;
/**
*
*
* @author Johan Hall
*/
public interface Function {
	/**
	 * Initialize the feature function
	 * 
	 * @param arguments an array of arguments with the type returned by getParameterTypes()
	 * @throws MaltChainedException
	 */
	public void initialize(Object[] arguments) throws MaltChainedException;
	/**
	 * Returns an array of class types used by the feature extraction system to invoke initialize with
	 * correct arguments.
	 * 
	 * @return an array of class types
	 */
	public Class<?>[] getParameterTypes();
	/**
	 * Cause the feature function to update the feature value.
	 * 
	 * @throws MaltChainedException
	 */
	public void update() throws MaltChainedException;
}
