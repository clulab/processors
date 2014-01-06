package org.maltparserx.core.syntaxgraph;

import org.maltparserx.core.exception.MaltChainedException;
/**
*
*
* @author Johan Hall
*/
public interface Structure {
	/**
	 * Resets the structure.
	 * 
	 * @throws MaltChainedException
	 */
	public void clear() throws MaltChainedException;
}
