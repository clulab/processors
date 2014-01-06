package org.maltparserx.core.syntaxgraph.node;

import java.util.Iterator;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.syntaxgraph.Element;
import org.maltparserx.core.syntaxgraph.edge.Edge;

public interface Node extends ComparableNode, Element {
	public void addIncomingEdge(Edge in) throws MaltChainedException;
	public void addOutgoingEdge(Edge out) throws MaltChainedException;
	public void removeIncomingEdge(Edge in) throws MaltChainedException;
	public void removeOutgoingEdge(Edge out) throws MaltChainedException;
	public Iterator<Edge> getIncomingEdgeIterator();
	public Iterator<Edge> getOutgoingEdgeIterator();
	public void setIndex(int index) throws MaltChainedException;
}
