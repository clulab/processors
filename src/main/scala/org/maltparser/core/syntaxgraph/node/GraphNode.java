package org.maltparser.core.syntaxgraph.node;

import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.GraphElement;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;
import org.maltparser.core.syntaxgraph.edge.Edge;



/**
 * 
 * 
 * @author Johan Hall
 *
 */
public abstract class GraphNode extends GraphElement implements Node {
	protected SortedSet<Edge> incomingEdges;
	protected SortedSet<Edge> outgoingEdges;
	
	public GraphNode() throws MaltChainedException {
		super();
		incomingEdges = new TreeSet<Edge>();
		outgoingEdges = new TreeSet<Edge>();
	}
	
	public void addIncomingEdge(Edge in) throws MaltChainedException {
		if (in.getTarget() != this) {
			throw new SyntaxGraphException("The incoming edge's 'to' reference is not correct.");
		}
		incomingEdges.add(in);
	}
	
	public void addOutgoingEdge(Edge out) throws MaltChainedException {
		if (out.getSource() != this) {
			throw new SyntaxGraphException("The outgoing edge's 'from' reference is not correct");
		}
		outgoingEdges.add(out);
	}

	public void removeIncomingEdge(Edge in) throws MaltChainedException {
		if (in.getTarget() != this) {
			throw new SyntaxGraphException("The incoming edge's 'to' reference is not correct");
		}
		incomingEdges.remove(in);
	}

	public void removeOutgoingEdge(Edge out) throws MaltChainedException {
		if (out.getSource() != this) {
			throw new SyntaxGraphException("The outgoing edge's 'from' reference is not correct");
		}
		outgoingEdges.remove(out);
	}

	public int getLeftmostProperDescendantIndex() throws MaltChainedException {
		ComparableNode node = getLeftmostProperDescendant();
		return (node != null)?node.getIndex():-1;
	}
	
	public int getRightmostProperDescendantIndex() throws MaltChainedException {
		ComparableNode node = getRightmostProperDescendant();
		return (node != null)?node.getIndex():-1;
	}
	
	public int getLeftmostDescendantIndex() throws MaltChainedException {
		ComparableNode node = getLeftmostProperDescendant();
		return (node != null)?node.getIndex():this.getIndex();
	}
	
	public int getRightmostDescendantIndex() throws MaltChainedException {
		ComparableNode node = getRightmostProperDescendant();
		return (node != null)?node.getIndex():this.getIndex();
	}
	
	public Iterator<Edge> getIncomingEdgeIterator() {
		return incomingEdges.iterator();
	}
	
	public Iterator<Edge> getOutgoingEdgeIterator() {
		return outgoingEdges.iterator();
	}
	
	public void clear() throws MaltChainedException {
		super.clear();
		incomingEdges.clear();
		outgoingEdges.clear();
	}
	
	public int getInDegree() {
		return incomingEdges.size();
	}
	
	public int getOutDegree() {
		return outgoingEdges.size();
	}
	
	public SortedSet<Edge> getIncomingSecondaryEdges() {
		SortedSet<Edge> inSecEdges = new TreeSet<Edge>();
		for (Edge e : incomingEdges) {
			if (e.getType() == Edge.SECONDARY_EDGE) {
				inSecEdges.add(e);
			}
		}
		return inSecEdges;
	}
	
	public SortedSet<Edge> getOutgoingSecondaryEdges() {
		SortedSet<Edge> outSecEdges = new TreeSet<Edge>();
		for (Edge e : outgoingEdges) {
			if (e.getType() == Edge.SECONDARY_EDGE) {
				outSecEdges.add(e);
			}
		}
		return outSecEdges;
	}
	
	public int compareTo(ComparableNode o) {		
		return super.compareTo((GraphElement)o);
	}
	
	public abstract int getIndex();
	public abstract void setIndex(int index) throws MaltChainedException;
	public abstract boolean isRoot();
	
	public boolean equals(Object obj) {
		GraphNode v = (GraphNode)obj;
		return super.equals(obj) && incomingEdges.equals(v.incomingEdges) 
				&& outgoingEdges.equals(v.outgoingEdges); 
	}
	
	public int hashCode() {
		int hash = 7;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + (null == incomingEdges ? 0 : incomingEdges.hashCode());
		hash = 31 * hash + (null == outgoingEdges ? 0 : outgoingEdges.hashCode());
		return hash;
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(getIndex());
		sb.append(" [I:");
		for (Edge e : incomingEdges) {
			sb.append(e.getSource().getIndex());
			if (incomingEdges.last() != e) {
				sb.append(",");
			}
		}
		sb.append("][O:");
		for (Edge e : outgoingEdges) {
			sb.append(e.getTarget().getIndex());
			if (outgoingEdges.last() != e) {
				sb.append(",");
			}
		}
		sb.append("]");
		sb.append(super.toString());
		return sb.toString();
	}
}
