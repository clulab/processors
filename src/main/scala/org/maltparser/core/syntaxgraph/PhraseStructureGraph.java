package org.maltparser.core.syntaxgraph;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.pool.ObjectPoolList;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.edge.GraphEdge;
import org.maltparser.core.syntaxgraph.node.ComparableNode;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.core.syntaxgraph.node.Node;
import org.maltparser.core.syntaxgraph.node.NonTerminal;
import org.maltparser.core.syntaxgraph.node.NonTerminalNode;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;
import org.maltparser.core.syntaxgraph.node.Root;
import org.maltparser.core.syntaxgraph.node.TokenNode;
/**
*
*
* @author Johan Hall
*/
public class PhraseStructureGraph extends Sentence implements PhraseStructure { 
	protected final ObjectPoolList<Edge> edgePool;
	protected final SortedSet<Edge> graphEdges;
	protected final SortedMap<Integer, NonTerminal> nonTerminalNodes;
	protected final ObjectPoolList<NonTerminal> nonTerminalPool;
	protected final Root root;
	
	public PhraseStructureGraph(SymbolTableHandler symbolTables) throws MaltChainedException {
		super(symbolTables);

		root = new Root();
		root.setBelongsToGraph(this);
		
		graphEdges = new TreeSet<Edge>();
		edgePool = new ObjectPoolList<Edge>() {
			protected Edge create() { return new GraphEdge(); }
			public void resetObject(Edge o) throws MaltChainedException { o.clear(); }
		};
		
		nonTerminalNodes = new TreeMap<Integer,NonTerminal>();
		nonTerminalPool = new ObjectPoolList<NonTerminal>() {
			protected NonTerminal create() throws MaltChainedException { return new NonTerminal(); }
			public void resetObject(NonTerminal o) throws MaltChainedException { o.clear(); }
		};
	}
	
	public PhraseStructureNode addTerminalNode() throws MaltChainedException {
		return addTokenNode();
	}
	
	public PhraseStructureNode addTerminalNode(int index) throws MaltChainedException {
		return addTokenNode(index);
	}
	
	public PhraseStructureNode getTerminalNode(int index) {
		return getTokenNode(index);
	}
	
	public int nTerminalNode() {
		return nTokenNode();
	}
	
	public PhraseStructureNode addNonTerminalNode(int index) throws MaltChainedException {
		NonTerminal node = nonTerminalPool.checkOut();
		node.setIndex(index);
		node.setBelongsToGraph(this);
		nonTerminalNodes.put(index,node);
		return node;
	}
	
	public PhraseStructureNode addNonTerminalNode() throws MaltChainedException {
		int index = getHighestNonTerminalIndex();
		if (index > 0) {
			return addNonTerminalNode(index+1);
		}
		return addNonTerminalNode(1);
	}
	
	public PhraseStructureNode getNonTerminalNode(int index) throws MaltChainedException {
		return nonTerminalNodes.get(index);
	}
	
	public int getHighestNonTerminalIndex() {
		try {
			return nonTerminalNodes.lastKey();
		} catch (NoSuchElementException e) {
			return 0;
		}
	}
	
	public Set<Integer> getNonTerminalIndices() {
		return new TreeSet<Integer>(nonTerminalNodes.keySet());
	}
	
	public boolean hasNonTerminals() {
		return !nonTerminalNodes.isEmpty();
	}
	
	public int nNonTerminals() {
		return nonTerminalNodes.size();
	}
	
	public PhraseStructureNode getPhraseStructureRoot() {
		return root;
	}
	
	public Edge addPhraseStructureEdge(PhraseStructureNode parent, PhraseStructureNode child) throws MaltChainedException {
		if (parent == null || child == null) {
			throw new MaltChainedException("Parent or child node is missing.");
		} else if (parent instanceof NonTerminalNode && !child.isRoot()) {
			Edge e = edgePool.checkOut();
			e.setBelongsToGraph(this);
			e.setEdge((Node)parent, (Node)child, Edge.PHRASE_STRUCTURE_EDGE);
			graphEdges.add(e);
			return e;
		} else {
			throw new MaltChainedException("Parent or child node is not of correct node type.");
		}
	}
	
	public void removePhraseStructureEdge(PhraseStructureNode parent, PhraseStructureNode child) throws MaltChainedException {
		if (parent == null || child == null) {
			throw new MaltChainedException("Parent or child node is missing.");
		} else if (parent instanceof NonTerminalNode && !child.isRoot()) {
			for (Edge e : graphEdges) {
				if (e.getSource() == parent && e.getTarget() == child) {
					e.clear();
					graphEdges.remove(e);
					if (e instanceof GraphEdge) {
						edgePool.checkIn(e);
					}
				}
			}
		} else {
			throw new SyntaxGraphException("Head node is not a root node or a terminal node.");
		}
	}
	
	public Edge addSecondaryEdge(ComparableNode source, ComparableNode target) throws MaltChainedException {
		if (source == null || target == null) {
			throw new SyntaxGraphException("Head or dependent node is missing.");
		} else if (!target.isRoot()) {
			Edge e = edgePool.checkOut();
			e.setBelongsToGraph(this);
			e.setEdge((Node)source, (Node)target, Edge.SECONDARY_EDGE);
			graphEdges.add(e);
			return e;
		}
		return null;
	}
	
	public void removeSecondaryEdge(ComparableNode source, ComparableNode target) throws MaltChainedException {
		if (source == null || target == null) {
			throw new SyntaxGraphException("Head or dependent node is missing.");
		} else if (!target.isRoot()) {
			Iterator<Edge> ie = ((Node)target).getIncomingEdgeIterator();
			while (ie.hasNext()) {
				Edge e = ie.next();
				if (e.getSource() == source) {
					ie.remove();
					graphEdges.remove(e);
					edgePool.checkIn(e);
				}
			}
		}
	}
	
	public int nEdges() {
		return graphEdges.size();
	}
	
	public SortedSet<Edge> getEdges() {
		return graphEdges;
	}
	
	public boolean isContinuous() {
		for (int index : nonTerminalNodes.keySet()) {
			NonTerminalNode node = nonTerminalNodes.get(index);
			if (!node.isContinuous()) {
				return false;
			}
		}
		return true;
	}
	
	public boolean isContinuousExcludeTerminalsAttachToRoot() {
		for (int index : nonTerminalNodes.keySet()) {
			NonTerminalNode node = nonTerminalNodes.get(index);
			if (!node.isContinuousExcludeTerminalsAttachToRoot()) {
				return false;
			}
		}
		return true;
	}
	
//	public void makeContinuous() throws MaltChainedException {
//		if (root != null) {
//			root.reArrangeChildrenAccordingToLeftAndRightProperDesendant();
//		}
//	}
	
	public void clear() throws MaltChainedException {
		edgePool.checkInAll();
		graphEdges.clear();
		root.clear();
		root.setBelongsToGraph(this);
		nonTerminalPool.checkInAll();
		nonTerminalNodes.clear();
		super.clear();
	}
	
	public String toStringTerminalNode(TokenNode node) {
		final StringBuilder sb = new StringBuilder();
		final DependencyNode depnode = node;

		sb.append(node.toString().trim());
		if (depnode.hasHead()) {
			sb.append('\t');
			try {
				sb.append(depnode.getHead().getIndex());
				sb.append('\t');
				sb.append(depnode.getHeadEdge().toString());
			} catch (MaltChainedException e) {
				System.err.println(e);
			}
		}
		sb.append('\n');

		return sb.toString();
	}
	
	public String toStringNonTerminalNode(NonTerminalNode node) {
		final StringBuilder sb = new StringBuilder();

		sb.append(node.toString().trim());
		sb.append('\n');
		Iterator<Edge> ie = ((Node)node).getOutgoingEdgeIterator();
		while (ie.hasNext()) {
			Edge e = ie.next();
			if (e.getTarget() instanceof TokenNode) {
				sb.append("   T");
				sb.append(e.getTarget().getIndex());
			}
			if (e.getTarget() instanceof NonTerminalNode) {
				sb.append("   N");
				sb.append(e.getTarget().getIndex());
			}
			sb.append('\t');
			sb.append(e.toString());
			sb.append('\n');
		}
		return sb.toString();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		for (int index : terminalNodes.keySet()) {
			sb.append(toStringTerminalNode(terminalNodes.get(index)));
		}
		sb.append('\n');
		sb.append(toStringNonTerminalNode((NonTerminalNode)getPhraseStructureRoot()));
		for (int index : nonTerminalNodes.keySet()) {
			sb.append(toStringNonTerminalNode(nonTerminalNodes.get(index)));
		}
		
		return sb.toString();
	}
}
