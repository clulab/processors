package org.maltparser.core.syntaxgraph.node;

public interface TokenNode extends DependencyNode, PhraseStructureNode {
	/**
	 * Sets the predecessor token node in the linear order of the token nodes.
	 * 
	 * @param predecessor the predecessor token node
	 */
	public void setPredecessor(TokenNode predecessor);
	/**
	 * Sets the predecessor token node in the linear order of the token nodes.
	 * 
	 * @param successor the successor token node
	 */
	public void setSuccessor(TokenNode successor);
	/**
	 * Returns the predecessor token node in the linear order of the token nodes.
	 * 
	 * @return the predecessor token node in the linear order of the token nodes.
	 */
	public TokenNode getPredecessor();
	/**
	 * Returns the successor token node in the linear order of the token nodes.
	 * 
	 * @return the successor token node in the linear order of the token nodes.
	 */
	public TokenNode getSuccessor();
}
