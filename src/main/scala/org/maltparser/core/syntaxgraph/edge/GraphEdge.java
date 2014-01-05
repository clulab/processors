package org.maltparser.core.syntaxgraph.edge;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.GraphElement;
import org.maltparser.core.syntaxgraph.node.Node;


/**
*
*
* @author Johan Hall
*/
public class GraphEdge extends GraphElement implements Edge, Comparable<GraphEdge> {
	private Node source = null;
	private Node target = null;
	private int type;
	
	public GraphEdge() { }
	
	public GraphEdge(Node source, Node target, int type) throws MaltChainedException {
		super();
		clear();
		setEdge(source, target, type);
	}
	
	/**
	 * Sets the edge with a source node, a target node and a type (DEPENDENCY_EDGE, PHRASE_STRUCTURE_EDGE
	 * or SECONDARY_EDGE). 
	 * 
	 * @param source a source node
	 * @param target a target node
	 * @param type a type (DEPENDENCY_EDGE, PHRASE_STRUCTURE_EDGE or SECONDARY_EDGE)
	 * @throws MaltChainedException
	 */
	public void setEdge(Node source, Node target, int type) throws MaltChainedException {
		this.source = source;
		this.target = target;
		if (type >= Edge.DEPENDENCY_EDGE && type <= Edge.SECONDARY_EDGE ) {
			this.type = type;
		}
		this.source.addOutgoingEdge(this);
		this.target.addIncomingEdge(this);
		setChanged(); 
		notifyObservers(this);
	}
	
	public void clear() throws MaltChainedException {
		super.clear();
		if (source != null) {
			this.source.removeOutgoingEdge(this);
		}
		if (target != null) {
			this.target.removeIncomingEdge(this);
		}
		this.source = null;
		this.target = null;
		this.type = -1;
	}
	
	/**
	 * Returns the source node of the edge.
	 * 
	 * @return the source node of the edge.
	 */
	public Node getSource() {
		return this.source;
	}
	
	/**
	 * Returns the target node of the edge.
	 * 
	 * @return the target node of the edge.
	 */
	public Node getTarget() {
		return this.target;
	}
	
	/**
	 * Returns the edge type (DEPENDENCY_EDGE, PHRASE_STRUCTURE_EDGE or SECONDARY_EDGE).
	 * 
	 * @return the edge type (DEPENDENCY_EDGE, PHRASE_STRUCTURE_EDGE or SECONDARY_EDGE).
	 */
	public int getType() {
		return this.type;
	}

	public int compareTo(GraphEdge that) {
		final int BEFORE = -1;
	    final int EQUAL = 0;
	    final int AFTER = 1;
	    
	    if (this == that) return EQUAL;
	    
	    if (this.target.getCompareToIndex() < that.target.getCompareToIndex()) return BEFORE;
	    if (this.target.getCompareToIndex() > that.target.getCompareToIndex()) return AFTER;
	    
	    if (this.source.getCompareToIndex() < that.source.getCompareToIndex()) return BEFORE;
	    if (this.source.getCompareToIndex() > that.source.getCompareToIndex()) return AFTER;
	    
	    if (this.type < that.type) return BEFORE;
	    if (this.type > that.type) return AFTER;
	    
		return super.compareTo(that);
	}
	
	
	public boolean equals(Object obj) {
		final GraphEdge e = (GraphEdge)obj;
		return this.type == e.getType() && this.source.equals(e.getSource()) && this.target.equals(e.getTarget()) && super.equals(obj); 
	}
	
	public int hashCode() {
		int hash = 7;
		hash = 31 * hash + type;
		hash = 31 * hash + (null == source ? 0 : source.hashCode());
		hash = 31 * hash + (null == target ? 0 : target.hashCode());
		return 31 * hash + super.hashCode();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(source.getIndex());
		sb.append("->");
		sb.append(target.getIndex());
		sb.append(' ');
		sb.append(super.toString());
		return sb.toString();
	}
}
