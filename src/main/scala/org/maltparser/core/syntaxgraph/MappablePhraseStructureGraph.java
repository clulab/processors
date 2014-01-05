package org.maltparser.core.syntaxgraph;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Observable;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;


import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.SystemLogger;
import org.maltparser.core.pool.ObjectPoolList;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.syntaxgraph.ds2ps.LosslessMapping;
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
public class MappablePhraseStructureGraph extends Sentence implements DependencyStructure, PhraseStructure {
	private final ObjectPoolList<Edge> edgePool;
	private final SortedSet<Edge> graphEdges;
	private Root root;
	private boolean singleHeadedConstraint;
	private final SortedMap<Integer, NonTerminal> nonTerminalNodes;
	private final ObjectPoolList<NonTerminal> nonTerminalPool;
	private LosslessMapping mapping;
	private RootLabels rootLabels;
	
	public MappablePhraseStructureGraph(SymbolTableHandler symbolTables) throws MaltChainedException {
		super(symbolTables);
		setSingleHeadedConstraint(true);
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
		clear();
	}

	public DependencyNode addDependencyNode() throws MaltChainedException {
		return addTokenNode();
	}
	
	public DependencyNode addDependencyNode(int index) throws MaltChainedException {
		if (index == 0) {
			return root;
		}
		return addTokenNode(index);
	}
	
	public DependencyNode getDependencyNode(int index) throws MaltChainedException {
		if (index == 0) {
			return root;
		} 
		return getTokenNode(index);
	}
	
	public int nDependencyNode() {
		return nTokenNode() + 1;
	}
	
	public int getHighestDependencyNodeIndex() {
		if (hasTokens()) {
			return getHighestTokenIndex();
		}
		return 0;
	}
	
	public Edge addDependencyEdge(int headIndex, int dependentIndex) throws MaltChainedException {
		DependencyNode head = null;
		DependencyNode dependent = null;
		if (headIndex == 0) {
			head = root;
		} else if (headIndex > 0) {
			head = getOrAddTerminalNode(headIndex);
		}
		
		if (dependentIndex > 0) {
			dependent = getOrAddTerminalNode(dependentIndex);
		}
		return addDependencyEdge(head, dependent);
	}
	
	public Edge addDependencyEdge(DependencyNode head, DependencyNode dependent) throws MaltChainedException {
		if (head == null || dependent == null || head.getBelongsToGraph() != this || dependent.getBelongsToGraph() != this) {
			throw new SyntaxGraphException("Head or dependent node is missing.");
		} else if (!dependent.isRoot()) {
			if (singleHeadedConstraint && dependent.hasHead()) {
				throw new SyntaxGraphException("The dependent already have a head. ");
			}
			DependencyNode hc = ((DependencyNode)head).findComponent();
			DependencyNode dc = ((DependencyNode)dependent).findComponent();
			if (hc != dc) {
				link(hc, dc);
				numberOfComponents--;		
			}
			Edge e = edgePool.checkOut();
			e.setBelongsToGraph(this);
			e.setEdge((Node)head, (Node)dependent, Edge.DEPENDENCY_EDGE);
			graphEdges.add(e);
			return e;
		} else {
			throw new SyntaxGraphException("Head node is not a root node or a terminal node.");
		}
	}
	
	public Edge moveDependencyEdge(int newHeadIndex, int dependentIndex) throws MaltChainedException {
		DependencyNode newHead = null;
		DependencyNode dependent = null;
		if (newHeadIndex == 0) {
			newHead = root;
		} else if (newHeadIndex > 0) {
			newHead = terminalNodes.get(newHeadIndex);
		}
		
		if (dependentIndex > 0) {
			dependent = terminalNodes.get(dependentIndex);
		}
		return moveDependencyEdge(newHead, dependent);
	}
	
	public Edge moveDependencyEdge(DependencyNode newHead, DependencyNode dependent) throws MaltChainedException {
		if (dependent == null || !dependent.hasHead() || newHead.getBelongsToGraph() != this || dependent.getBelongsToGraph() != this) {
			return null;
		}
		Edge headEdge = dependent.getHeadEdge();

		LabelSet labels = null;
		if (headEdge.isLabeled()) { 
			labels = checkOutNewLabelSet();
			for (SymbolTable table : headEdge.getLabelTypes()) {
				labels.put(table, headEdge.getLabelCode(table));
			}
		}
		headEdge.clear();
		headEdge.setBelongsToGraph(this);
		headEdge.setEdge((Node)newHead, (Node)dependent, Edge.DEPENDENCY_EDGE);
		if (labels != null) {
			headEdge.addLabel(labels);
			labels.clear();
			checkInLabelSet(labels);
		}
		return headEdge;
	}
	
	public void removeDependencyEdge(int headIndex, int dependentIndex) throws MaltChainedException {
		Node head = null;
		Node dependent = null;
		if (headIndex == 0) {
			head = root;
		} else if (headIndex > 0) {
			head = terminalNodes.get(headIndex);
		}
		
		if (dependentIndex > 0) {
			dependent = terminalNodes.get(dependentIndex);
		}
		removeDependencyEdge(head, dependent);
	}
	
	protected void removeDependencyEdge(Node head, Node dependent) throws MaltChainedException {
		if (head == null || dependent == null || head.getBelongsToGraph() != this || dependent.getBelongsToGraph() != this) {
			throw new SyntaxGraphException("Head or dependent node is missing.");
		} else if (!dependent.isRoot()) {
			Iterator<Edge> ie = dependent.getIncomingEdgeIterator();
			while (ie.hasNext()) {
				Edge e = ie.next();
				if (e.getSource() == head) {
					ie.remove();
					graphEdges.remove(e);
					edgePool.checkIn(e);
				}
			} 
		} else {
			throw new SyntaxGraphException("Head node is not a root node or a terminal node.");
		}
	}

	public Edge addSecondaryEdge(ComparableNode source, ComparableNode target) throws MaltChainedException {
		if (source == null || target == null || source.getBelongsToGraph() != this || target.getBelongsToGraph() != this) {
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
		if (source == null || target == null || source.getBelongsToGraph() != this || target.getBelongsToGraph() != this) {
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
	
	public boolean hasLabeledDependency(int index) throws MaltChainedException {
		return (getDependencyNode(index).hasHead() && getDependencyNode(index).getHeadEdge().isLabeled());
	}
	
	public boolean isConnected() {
		return (numberOfComponents == 1);
	}
	
	public boolean isProjective()  throws MaltChainedException {
		for (int i : terminalNodes.keySet()) {
			if (!terminalNodes.get(i).isProjective()) {
				return false;
			}
		}
		return true;
	}
	
	public boolean isTree() {
		return isConnected() && isSingleHeaded();
	}
	
	public boolean isSingleHeaded() {
		for (int i : terminalNodes.keySet()) {
			if (!terminalNodes.get(i).hasAtMostOneHead()) {
				return false;
			}
		}
		return true;
	}
	
	public boolean isSingleHeadedConstraint() {
		return singleHeadedConstraint;
	}

	public void setSingleHeadedConstraint(boolean singleHeadedConstraint) {
		this.singleHeadedConstraint = singleHeadedConstraint;
	}
	
	public int nNonProjectiveEdges() throws MaltChainedException {
		int c = 0;
		for (int i : terminalNodes.keySet()) {
			if (!terminalNodes.get(i).isProjective()) {
				c++;
			}
		}
		return c;
	}
	
	public int nEdges() {
		return graphEdges.size();
	}
	
	public SortedSet<Edge> getEdges() {
		return graphEdges;
	}
	
	public SortedSet<Integer> getDependencyIndices() {
		SortedSet<Integer> indices = new TreeSet<Integer>(terminalNodes.keySet());
		indices.add(0);
		return indices;
	}
	
	protected DependencyNode link(DependencyNode x, DependencyNode y) {
		if (x.getRank() > y.getRank()) {
			y.setComponent(x);
		} else {
			x.setComponent(y);
			if (x.getRank() == y.getRank()) {
				y.setRank(y.getRank()+1);
			}
			return y;
		}
		return x;
	}
	
	public void linkAllTerminalsToRoot() throws MaltChainedException {
		clear();

		for (int i : terminalNodes.keySet()) {
			DependencyNode node = terminalNodes.get(i);
			addDependencyEdge(root,node);
		}
	}
	
	public void linkAllTreesToRoot() throws MaltChainedException {
		for (int i : terminalNodes.keySet()) {
			if (!terminalNodes.get(i).hasHead()) {
				Edge e = addDependencyEdge(root,terminalNodes.get(i));
				mapping.updatePhraseStructureGraph(this, e, false);
			}
		}
	}
	
	public LabelSet getDefaultRootEdgeLabels() throws MaltChainedException {
		if (rootLabels == null) {
			return null;
		}
		return rootLabels.getDefaultRootLabels();
	}
	
	public String getDefaultRootEdgeLabelSymbol(SymbolTable table) throws MaltChainedException {
		if (rootLabels == null) {
			return null;
		}
		return rootLabels.getDefaultRootLabelSymbol(table);
	}
	
	public int getDefaultRootEdgeLabelCode(SymbolTable table) throws MaltChainedException {
		if (rootLabels == null) {
			return -1;
		}
		return rootLabels.getDefaultRootLabelCode(table);
	}
	
	public void setDefaultRootEdgeLabel(SymbolTable table, String defaultRootSymbol) throws MaltChainedException {
		if (rootLabels == null) {
			rootLabels = new RootLabels();
		}
		rootLabels.setDefaultRootLabel(table, defaultRootSymbol);
	}
	
	public void setDefaultRootEdgeLabels(String rootLabelOption, SortedMap<String, SymbolTable> edgeSymbolTables) throws MaltChainedException {
		if (rootLabels == null) {
			rootLabels = new RootLabels();
		}
		rootLabels.setRootLabels(rootLabelOption, edgeSymbolTables);
	}
	
	public void clear() throws MaltChainedException {
		edgePool.checkInAll();
		graphEdges.clear();
		root.clear();
		root.setBelongsToGraph(this);
		nonTerminalPool.checkInAll();
		nonTerminalNodes.clear();
		if (mapping != null) {
			mapping.clear();	
		}
		super.clear();
		
		numberOfComponents++;
	}
	
	public DependencyNode getDependencyRoot() {
		return root;
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
			throw new MaltChainedException("Parent or child node is missing in sentence "+getSentenceID());
		} else if (parent.getBelongsToGraph() != this || child.getBelongsToGraph() != this) {
			throw new MaltChainedException("Parent or child node is not a member of the graph in sentence "+getSentenceID());
		} else if (parent == child) {
			throw new MaltChainedException("It is not allowed to add a phrase structure edge connecting the same node in sentence "+getSentenceID());
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
	
	public void update(Observable  o, Object arg)  {
		if (o instanceof Edge && mapping != null) {
			try {
				mapping.update(this, (Edge)o, arg);
			} catch (MaltChainedException ex) {
				if (SystemLogger.logger().isDebugEnabled()) {
					SystemLogger.logger().debug("",ex);
				} else {
					SystemLogger.logger().error(ex.getMessageChain());
				}
				System.exit(1);
			}
		}
	}

	public LosslessMapping getMapping() {
		return mapping;
	}

	public void setMapping(LosslessMapping mapping) {
		this.mapping = mapping;
	}

	public void addLabel(Element element, String labelFunction, String label) throws MaltChainedException {
		super.addLabel(element, labelFunction, label);
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
		StringBuilder sb = new StringBuilder();
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
