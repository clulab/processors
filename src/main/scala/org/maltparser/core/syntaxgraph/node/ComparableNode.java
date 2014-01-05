package org.maltparser.core.syntaxgraph.node;

import java.util.SortedSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.Element;
import org.maltparser.core.syntaxgraph.edge.Edge;

public interface ComparableNode extends Element, Comparable<ComparableNode> {
	/**
	 * Returns the index of the node.
	 * 
	 * @return the index of the node.
	 */
	public int getIndex();
	/**
	 * Returns the index of the node (only used internal by compareTo).
	 * 
	 * @return the index of the node (only used internal by compareTo).
	 */
	public int getCompareToIndex();
	/**
	 * Returns <i>true</i> if the node is a root node, otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if the node is a root node, otherwise <i>false</i>.
	 */
	public boolean isRoot();
	/**
	 * Returns the left-most proper terminal descendant node (excluding itself). 
	 * 
	 * @return the left-most proper terminal descendant node. 
	 * @throws MaltChainedException
	 */
	public ComparableNode getLeftmostProperDescendant() throws MaltChainedException;
	/**
	 * Returns the right-most proper terminal descendant node (excluding itself). 
	 * 
	 * @return the right-most proper terminal descendant node. 
	 * @throws MaltChainedException
	 */
	public ComparableNode getRightmostProperDescendant() throws MaltChainedException;
	/**
	 * Returns the index of the left-most proper terminal descendant node (excluding itself). 
	 * 
	 * @return the index of the left-most proper terminal descendant node. 
	 * @throws MaltChainedException
	 */
	public int getLeftmostProperDescendantIndex() throws MaltChainedException;
	/**
	 * Returns the index of the right-most proper terminal descendant node (excluding itself). 
	 * 
	 * @return the index of the right-most proper terminal descendant node. 
	 * @throws MaltChainedException
	 */
	public int getRightmostProperDescendantIndex() throws MaltChainedException;
	/**
	 * Returns the left-most terminal descendant node. 
	 * 
	 * @return the left-most terminal descendant node. 
	 * @throws MaltChainedException
	 */
	public ComparableNode getLeftmostDescendant() throws MaltChainedException;
	/**
	 * Returns the right-most terminal descendant node. 
	 * 
	 * @return the right-most terminal descendant node. 
	 * @throws MaltChainedException
	 */
	public ComparableNode getRightmostDescendant() throws MaltChainedException;
	/**
	 * Returns the index of the left-most terminal descendant node. 
	 * 
	 * @return the index of the left-most terminal descendant node. 
	 * @throws MaltChainedException
	 */
	public int getLeftmostDescendantIndex() throws MaltChainedException;
	/**
	 * Returns the index of the right-most terminal descendant node. 
	 * 
	 * @return the index of the right-most terminal descendant node. 
	 * @throws MaltChainedException
	 */
	public int getRightmostDescendantIndex() throws MaltChainedException;
	/**
	 * Returns the in degree of the node (number of incoming edges of all types of edges).
	 * 
	 * @return the in degree of the node (number of incoming edges of all types of edges).
	 */
	public int getInDegree();
	/**
	 * Returns the out degree of the node (number of outgoing edges of all types of edges).
	 * 
	 * @return the out degree of the node (number of outgoing edges of all types of edges).
	 */
	public int getOutDegree();
	/**
	 * Returns a sorted set of incoming secondary edges.
	 * 
	 * @return a sorted set of incoming secondary edges.
	 */
	public SortedSet<Edge> getIncomingSecondaryEdges();
	/**
	 * Returns a sorted set of outgoing secondary edges.
	 * 
	 * @return a sorted set of outgoing secondary edges.
	 */
	public SortedSet<Edge> getOutgoingSecondaryEdges();
}
