package org.maltparser.core.syntaxgraph.edge;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.Element;
import org.maltparser.core.syntaxgraph.node.Node;
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
