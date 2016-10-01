package org.maltparserx.core.syntaxgraph.edge;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.syntaxgraph.Element;
import org.maltparserx.core.syntaxgraph.node.Node;
/**
*
*
* @author Johan Hall
*/
public interface Edge extends Element {
	public static final int DEPENDENCY_EDGE  = 1;
	public static final int PHRASE_STRUCTURE_EDGE = 2;
	public static final int SECONDARY_EDGE = 3;
	
	/**
	 * Sets the edge with a source node, a target node and a type (DEPENDENCY_EDGE, PHRASE_STRUCTURE_EDGE
	 * or SECONDARY_EDGE). 
	 * 
	 * @param source a source node
	 * @param target a target node
	 * @param type a type (DEPENDENCY_EDGE, PHRASE_STRUCTURE_EDGE or SECONDARY_EDGE)
	 * @throws MaltChainedException
	 */
	public void setEdge(Node source, Node target, int type) throws MaltChainedException;
	/**
	 * Returns the source node of the edge.
	 * 
	 * @return the source node of the edge.
	 */
	public Node getSource();
	/**
	 * Returns the target node of the edge.
	 * 
	 * @return the target node of the edge.
	 */
	public Node getTarget();
	/**
	 * Returns the edge type (DEPENDENCY_EDGE, PHRASE_STRUCTURE_EDGE or SECONDARY_EDGE).
	 * 
	 * @return the edge type (DEPENDENCY_EDGE, PHRASE_STRUCTURE_EDGE or SECONDARY_EDGE).
	 */
	public int getType();
}
