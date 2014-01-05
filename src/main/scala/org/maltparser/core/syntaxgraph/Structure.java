package org.maltparser.core.syntaxgraph;

import org.maltparser.core.exception.MaltChainedException;
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
