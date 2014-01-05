package org.maltparser.core.syntaxgraph;

import java.util.SortedSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.node.TokenNode;

/**
*
*
* @author Johan Hall
*/
public interface TokenStructure extends Structure, LabeledStructure { 
	/**
	 * Adds a token node with index <i>n + 1</i>, where <i>n</i> is the index of the last token node. 
	 * 
	 * @return the added token node.
	 * @throws MaltChainedException
	 */
	public TokenNode addTokenNode() throws MaltChainedException;
	/**
	 * Adds a token node with index <i>index</i>.
	 * 
	 * @param index the index of the token node.
	 * @return the added token node.
	 */
	public TokenNode addTokenNode(int index) throws MaltChainedException;
	/**
	 * Returns the token node with index <i>index</i>.
	 * 
	 * @param index the index of the token node.
	 * @return a token node with index <i>index</i>.
	 * @throws MaltChainedException
	 */
	public TokenNode getTokenNode(int index);
	/**
	 * Returns the number of token nodes in the token structure (sentence).
	 * 
	 * @return the number of token nodes in the token structure (sentence).
	 */
	public int nTokenNode();
	/**
	 * Returns a sorted set of integers {s,...,n}, where each index i identifies a token node. Index <i>s</i> 
	 * is the first token node and index <i>n</i> is the last token node. 
	 * 
	 * @return a sorted set of integers {s,...,n}.
	 */
	public SortedSet<Integer> getTokenIndices();
	/**
	 * Returns the index of the last token node.
	 * 
	 * @return the index of the last token node.
	 */
	public int getHighestTokenIndex();
	/**
	 *  Returns <i>true</i> if the token structure (sentence) has any token nodes, otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if the token structure (sentence) has any token nodes, otherwise <i>false</i>.
	 */
	public boolean hasTokens();
	/**
	 * Returns the sentence ID
	 * 
	 * @return the sentence ID
	 */
	public int getSentenceID();
	/**
	 * Sets the sentence ID
	 * 
	 * @param sentenceID a sentence ID
	 */
	public void setSentenceID(int sentenceID);
}
