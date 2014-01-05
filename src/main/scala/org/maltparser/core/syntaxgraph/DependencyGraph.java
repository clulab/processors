package org.maltparser.core.syntaxgraph;

import java.util.Iterator;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.pool.ObjectPoolList;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.edge.GraphEdge;
import org.maltparser.core.syntaxgraph.node.ComparableNode;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.core.syntaxgraph.node.Node;
import org.maltparser.core.syntaxgraph.node.Root;
/**
 *
 *
 * @author Johan Hall
 */
public class DependencyGraph extends Sentence implements DependencyStructure { 
	private final ObjectPoolList<Edge> edgePool;
	private final SortedSet<Edge> graphEdges;
	private final Root root;
	private boolean singleHeadedConstraint;
	private RootLabels rootLabels;
	
	public DependencyGraph(SymbolTableHandler symbolTables) throws MaltChainedException {
		super(symbolTables);
		setSingleHeadedConstraint(true);
		root = new Root();
		root.setBelongsToGraph(this);
		graphEdges = new TreeSet<Edge>();
		edgePool = new ObjectPoolList<Edge>() {
			protected Edge create() { return new GraphEdge(); }
			public void resetObject(Edge o) throws MaltChainedException { o.clear(); }
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
		} else { // if (headIndex > 0) {
			head = getOrAddTerminalNode(headIndex);
		}
		
		if (dependentIndex > 0) {
			dependent = getOrAddTerminalNode(dependentIndex);
		}
		return addDependencyEdge(head, dependent);
	}
	
	protected Edge addDependencyEdge(DependencyNode head, DependencyNode dependent) throws MaltChainedException {
		if (head == null || dependent == null) {
			throw new SyntaxGraphException("Head or dependent node is missing.");
		} else if (!dependent.isRoot()) {
			if (singleHeadedConstraint && dependent.hasHead()) {
				return moveDependencyEdge(head, dependent);
			}
			final DependencyNode hc = ((DependencyNode)head).findComponent();
			final DependencyNode dc = ((DependencyNode)dependent).findComponent();
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
	
	protected Edge moveDependencyEdge(DependencyNode newHead, DependencyNode dependent) throws MaltChainedException {
		if (dependent == null || !dependent.hasHead()) {
			return null;
		}
		Edge headEdge = dependent.getHeadEdge();
		final LabelSet labels = checkOutNewLabelSet();
		for (SymbolTable table : headEdge.getLabelTypes()) {
			labels.put(table, headEdge.getLabelCode(table));
		}
		headEdge.clear();
		headEdge.setBelongsToGraph(this);
		headEdge.setEdge((Node)newHead, (Node)dependent, Edge.DEPENDENCY_EDGE);
		headEdge.addLabel(labels);
		labels.clear();
		checkInLabelSet(labels);
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
		if (head == null || dependent == null) {
			throw new SyntaxGraphException("Head or dependent node is missing.");
		} else if (!dependent.isRoot()) {
			Iterator<Edge> ie = dependent.getIncomingEdgeIterator();
			
			while (ie.hasNext()) {
				Edge e = ie.next();
				if (e.getSource() == head) {
					graphEdges.remove(e);
					ie.remove();
					edgePool.checkIn(e);
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
			final Iterator<Edge> ie = ((Node)target).getIncomingEdgeIterator();
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
	
//	public boolean hasLabeledDependency(int index, SymbolTable table) {
//		return (!getDependencyNode(index).isRoot() && getDependencyNode(index).getLabelCode(table) != null);
//	}

	public boolean hasLabeledDependency(int index) throws MaltChainedException {
		return (getDependencyNode(index).hasHead() && getDependencyNode(index).getHeadEdge().isLabeled());
	}
	
	public boolean isConnected() {
		return (numberOfComponents == 1);
	}
	
	public boolean isProjective() throws MaltChainedException {
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
	
	public void linkAllTreesToRoot() throws MaltChainedException {
		for (int i : terminalNodes.keySet()) {
			if (!terminalNodes.get(i).hasHead()) {
				addDependencyEdge(root,terminalNodes.get(i));
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
		super.clear();
		numberOfComponents++;
	}
	
	public DependencyNode getDependencyRoot() {
		return root;
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		for (int index : terminalNodes.keySet()) {
			sb.append(terminalNodes.get(index).toString().trim());
			sb.append('\n');
		}
		sb.append('\n');
		return sb.toString();
	}
}
