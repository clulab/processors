package org.maltparser.core.syntaxgraph.node;

import java.util.NoSuchElementException;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.SystemLogger;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.LabelSet;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;
import org.maltparser.core.syntaxgraph.edge.Edge;


public class Token extends GraphNode implements TokenNode, DependencyNode, PhraseStructureNode {
	/**
	 * the previous terminal node in the linear precedence
	 */
	protected TokenNode predecessor = null;
	/**
	 * the next terminal node in the linear precedence
	 */
	protected TokenNode successor = null;

	/**
	 * a reference to a node where the node is part of a component. If the node is unconnected it will reference to it self. 
	 */
	protected DependencyNode component;
	protected int rank;
	
	protected int index;
	
	protected PhraseStructureNode parent;
	protected final SortedSet<DependencyNode> heads;
	protected final SortedSet<DependencyNode> leftDependents;
	protected final SortedSet<DependencyNode> rightDependents;
	
	
	public Token() throws MaltChainedException { 
		parent = null;
		heads = new TreeSet<DependencyNode>();
		leftDependents = new TreeSet<DependencyNode>();
		rightDependents = new TreeSet<DependencyNode>();
		clear();
	}
	
	/**
	 * Sets the predecessor terminal node in the linear order of the terminal nodes.
	 * 
	 * @param predecessor the predecessor terminal node
	 */
	public void setPredecessor(TokenNode predecessor) {
		this.predecessor = predecessor;
	}
	
	/**
	 * Sets the predecessor terminal node in the linear order of the terminal nodes.
	 * 
	 * @param successor the successor terminal node
	 */
	public void setSuccessor(TokenNode successor) {
		this.successor = successor;
	}
	
	/**
	 * Returns the predecessor terminal node in the linear order of the terminal nodes.
	 * 
	 * @return the predecessor terminal node in the linear order of the terminal nodes.
	 */
	public TokenNode getPredecessor() {
		return predecessor;
	}

	/**
	 * Returns the successor terminal node in the linear order of the terminal nodes.
	 * 
	 * @return the successor terminal node in the linear order of the terminal nodes.
	 */
	public TokenNode getSuccessor() {
		return successor;
	}
	
	public int getRank() {
		return rank;
	}
	
	public void setRank(int r) {
		this.rank = r;
	}
	
	public DependencyNode findComponent() {
		return findComponent(this);
	}
	
	private DependencyNode findComponent(DependencyNode x) {	
		if (x != x.getComponent()) {
			x.setComponent(findComponent(x.getComponent()));
		}
		return x.getComponent();
	}
	
	public DependencyNode getComponent() {
		return component;
	}
	
	public void setComponent(DependencyNode x) {
		this.component = x;
	}
	
	public void addIncomingEdge(Edge in) throws MaltChainedException {
		super.addIncomingEdge(in);
		if (in.getSource() != null) {
			if (in.getType() == Edge.DEPENDENCY_EDGE && in.getSource() instanceof DependencyNode) {
				heads.add((DependencyNode)in.getSource());
			} else if (in.getType() == Edge.PHRASE_STRUCTURE_EDGE && in.getSource() instanceof PhraseStructureNode) {
				parent = (PhraseStructureNode)in.getSource();
			}
		}
	}
	
	public void removeIncomingEdge(Edge in) throws MaltChainedException {
		super.removeIncomingEdge(in);
		if (in.getSource() != null) {
			if (in.getType() == Edge.DEPENDENCY_EDGE && in.getSource() instanceof DependencyNode) {
				heads.remove((DependencyNode)in.getSource());
			} else if (in.getType() == Edge.PHRASE_STRUCTURE_EDGE && in.getSource() instanceof PhraseStructureNode) {
				if (in.getSource() == parent) {
					this.parent = null;
				}
			}
		}
	}
	
	public void addOutgoingEdge(Edge out) throws MaltChainedException {
		super.addOutgoingEdge(out);
		if (out.getType() == Edge.DEPENDENCY_EDGE && out.getTarget() instanceof DependencyNode) {	
			final DependencyNode dependent = (DependencyNode)out.getTarget();
			if (compareTo(dependent) > 0) {
				leftDependents.add((DependencyNode)dependent);
			} else if (compareTo(dependent) < 0) {
				rightDependents.add((DependencyNode)dependent);
			}
		}
	}
	
	public void removeOutgoingEdge(Edge out) throws MaltChainedException {
		super.removeOutgoingEdge(out);
		if (out.getType() == Edge.DEPENDENCY_EDGE && out.getTarget() instanceof DependencyNode) {
			final DependencyNode dependent = (DependencyNode)out.getTarget();
			if (compareTo(dependent) > 0) {
				leftDependents.remove((DependencyNode)dependent);
			} else if (compareTo(dependent) < 0) {
				rightDependents.remove((DependencyNode)dependent);
			}
		}
	}
	
	public void setIndex(int index) throws MaltChainedException {
		if (index > 0) {
			this.index = index;
		} else {
			throw new SyntaxGraphException("A terminal node must have a positive integer value and not index "+index+". ");
		}
	}
	
	public int getIndex() {
		return index;
	}
	
	public int getCompareToIndex() {
		return index;
	}
	
	public boolean isRoot() {
		return false;
	}
	
	public DependencyNode getAncestor() throws MaltChainedException {
		if (!this.hasHead()) {
			return this;
		}
		
		DependencyNode tmp = this;
		while (tmp.hasHead()) {
			tmp = tmp.getHead();
		}
		return tmp;
	}
	
	public DependencyNode getProperAncestor() throws MaltChainedException {
		if (!this.hasHead()) {
			return null;
		}
		
		DependencyNode tmp = this;
		while (tmp.hasHead()) {
			tmp = tmp.getHead();
		}
		return tmp;
	}
	
	public ComparableNode getLeftmostProperDescendant() throws MaltChainedException {
		ComparableNode candidate = null;
		ComparableNode tmp = null;
		for (DependencyNode ldep : leftDependents) {
			if (candidate == null) {
				candidate = ldep;
			} else if (ldep.getIndex() < candidate.getIndex() ) {
				candidate = ldep;
			}
			tmp = ((Token)ldep).getLeftmostProperDescendant();
			if (tmp == null) {
				continue;
			}
			if (candidate == null) {
				candidate = tmp;
			} else if (tmp.getIndex() < candidate.getIndex() ) {
				candidate = tmp;
			}
			if (candidate.getIndex() == 1) {
				return candidate;
			}
		}
		for (DependencyNode rdep : rightDependents) {
			if (candidate == null) {
				candidate = rdep;
			} else if (rdep.getIndex() < candidate.getIndex() ) {
				candidate = rdep;
			}
			tmp = ((Token)rdep).getLeftmostProperDescendant();
			if (tmp == null) {
				continue;
			}
			if (candidate == null) {
				candidate = tmp;
			} else if (tmp.getIndex() < candidate.getIndex() ) {
				candidate = tmp;
			}
			if (candidate.getIndex() == 1) {
				return candidate;
			}
		}
		return candidate;
	}
	
	public ComparableNode getRightmostProperDescendant() throws MaltChainedException {
		ComparableNode candidate = null;
		ComparableNode tmp = null;
		for (DependencyNode ldep : leftDependents) {
			if (candidate == null) {
				candidate = ldep;
			} else if (ldep.getIndex() > candidate.getIndex() ) {
				candidate = ldep;
			}
			tmp = ((Token)ldep).getRightmostProperDescendant();
			if (tmp == null) {
				continue;
			}
			if (candidate == null) {
				candidate = tmp;
			} else if (tmp.getIndex() > candidate.getIndex() ) {
				candidate = tmp;
			}
		}
		for (DependencyNode rdep : rightDependents) {
			if (candidate == null) {
				candidate = rdep;
			} else if (rdep.getIndex() > candidate.getIndex() ) {
				candidate = rdep;
			}
			tmp = ((Token)rdep).getRightmostProperDescendant();
			if (tmp == null) {
				continue;
			}
			if (candidate == null) {
				candidate = tmp;
			} else if (tmp.getIndex() > candidate.getIndex() ) {
				candidate = tmp;
			}
		}
		return candidate;
	}
	
	public ComparableNode getLeftmostDescendant() throws MaltChainedException {
		ComparableNode candidate = this;
		ComparableNode tmp = null;
		for (DependencyNode ldep : leftDependents) {
			if (candidate == null) {
				candidate = ldep;
			} else if (ldep.getIndex() < candidate.getIndex() ) {
				candidate = ldep;
			}
			tmp = ((Token)ldep).getLeftmostDescendant();
			if (tmp == null) {
				continue;
			}
			if (candidate == null) {
				candidate = tmp;
			} else if (tmp.getIndex() < candidate.getIndex() ) {
				candidate = tmp;
			}
			if (candidate.getIndex() == 1) {
				return candidate;
			}
		}
		for (DependencyNode rdep : rightDependents) {
			if (candidate == null) {
				candidate = rdep;
			} else if (rdep.getIndex() < candidate.getIndex() ) {
				candidate = rdep;
			}
			tmp = ((Token)rdep).getLeftmostDescendant();
			if (tmp == null) {
				continue;
			}
			if (candidate == null) {
				candidate = tmp;
			} else if (tmp.getIndex() < candidate.getIndex() ) {
				candidate = tmp;
			}
			if (candidate.getIndex() == 1) {
				return candidate;
			}
		}
		return candidate;
	}
	
	public ComparableNode getRightmostDescendant() throws MaltChainedException {
		ComparableNode candidate = this;
		ComparableNode tmp = null;
		for (DependencyNode ldep : leftDependents) {
			if (candidate == null) {
				candidate = ldep;
			} else if (ldep.getIndex() > candidate.getIndex() ) {
				candidate = ldep;
			}
			tmp = ((Token)ldep).getRightmostDescendant();
			if (tmp == null) {
				continue;
			}
			if (candidate == null) {
				candidate = tmp;
			} else if (tmp.getIndex() > candidate.getIndex() ) {
				candidate = tmp;
			}
		}
		for (DependencyNode rdep : rightDependents) {
			if (candidate == null) {
				candidate = rdep;
			} else if (rdep.getIndex() > candidate.getIndex() ) {
				candidate = rdep;
			}
			tmp = ((Token)rdep).getRightmostDescendant();
			if (tmp == null) {
				continue;
			}
			if (candidate == null) {
				candidate = tmp;
			} else if (tmp.getIndex() > candidate.getIndex() ) {
				candidate = tmp;
			}
		}
		return candidate;
	}
	
	public PhraseStructureNode getParent() {
		return parent;
	}

	public Edge getParentEdge() throws MaltChainedException {
		for (Edge e : incomingEdges) {
			if (e.getSource() == parent && e.getType() == Edge.PHRASE_STRUCTURE_EDGE) {
				return e;
			}
		}
		return null;
	}
	
	public String getParentEdgeLabelSymbol(SymbolTable table) throws MaltChainedException {
		for (Edge e : incomingEdges) {
			if (e.getSource() == parent && e.getType() == Edge.PHRASE_STRUCTURE_EDGE) {
				return e.getLabelSymbol(table);
			}
		}
		return null;
	}

	public int getParentEdgeLabelCode(SymbolTable table) throws MaltChainedException {
		for (Edge e : incomingEdges) {
			if (e.getSource() == parent && e.getType() == Edge.PHRASE_STRUCTURE_EDGE) {
				return e.getLabelCode(table);
			}
		}
		return -1;
	}
	
	public boolean hasParentEdgeLabel(SymbolTable table) throws MaltChainedException {
		for (Edge e : incomingEdges) {
			if (e.getSource() == parent && e.getType() == Edge.PHRASE_STRUCTURE_EDGE) {
				return e.hasLabel(table);
			}
		}
		return false;
	}
	
	public boolean hasAtMostOneHead() {
		return heads.size() <= 1;
	}
	
	public boolean hasAncestorInside(int left, int right) throws MaltChainedException {
		DependencyNode tmp = this;
		if (tmp.getHead() != null) {
			tmp = tmp.getHead();
			if (tmp.getIndex() >= left && tmp.getIndex() <= right) {
				return true;
			}
		}
		return false;
	}
	
	public Set<Edge> getHeadEdges() throws MaltChainedException {
		return incomingEdges;
	}

	public Set<DependencyNode> getHeads() throws MaltChainedException {
		return heads;
	}

	public boolean hasHead() {
		return heads.size() != 0;
	}
	
	public DependencyNode getHead() throws MaltChainedException {
		if (heads.size() == 0) {
			return null;
		}
		if (heads.size() == 1) {
			for (DependencyNode head : heads) {
				return head;
			}
		}

		if (heads.size() > 1) {
			throw new SyntaxGraphException("The dependency node is multi-headed and it is ambigious to return a single-head dependency node. ");
		}
		// heads.first();

		return null;
	}
	
	public Edge getHeadEdge() throws MaltChainedException {
		if (heads.size() == 0) {
			return null;
		}
		if (incomingEdges.size() == 1 && incomingEdges.first() instanceof DependencyNode) {
			return incomingEdges.first();
		}
		if (heads.size() == 1) {
			for (Edge e : incomingEdges) {
				if (e.getSource() == heads.first()) {
					return e;
				}
			}
		}
		return null;
	}
	
	public void addHeadEdgeLabel(SymbolTable table, String symbol) throws MaltChainedException {
		if (hasHead()) {
			getHeadEdge().addLabel(table, symbol);
		}
	}
	
	public void addHeadEdgeLabel(SymbolTable table, int code) throws MaltChainedException {
		if (hasHead()) {
			getHeadEdge().addLabel(table, code);
		}
	}
	
	public void addHeadEdgeLabel(LabelSet labelSet) throws MaltChainedException {
		if (hasHead()) {
			getHeadEdge().addLabel(labelSet);
		}
	}
	
	public boolean hasHeadEdgeLabel(SymbolTable table) throws MaltChainedException {
		if (!hasHead()) {
			return false;
		}
		return getHeadEdge().hasLabel(table);
	}
	
	public String getHeadEdgeLabelSymbol(SymbolTable table) throws MaltChainedException {
		return getHeadEdge().getLabelSymbol(table);
	}
	
	public int getHeadEdgeLabelCode(SymbolTable table) throws MaltChainedException {
		if (!hasHead()) {
			return 0;
		}
		return getHeadEdge().getLabelCode(table);
	}
	
	public boolean isHeadEdgeLabeled() throws MaltChainedException {
		if (!hasHead()) {
			return false;
		}
		return getHeadEdge().isLabeled();
	}
	
	public int nHeadEdgeLabels() throws MaltChainedException {
		if (!hasHead()) {
			return 0;
		}
		return getHeadEdge().nLabels();
	}
	
	public Set<SymbolTable> getHeadEdgeLabelTypes() throws MaltChainedException {
		return getHeadEdge().getLabelTypes();
	}
	
	public LabelSet getHeadEdgeLabelSet() throws MaltChainedException {
		return getHeadEdge().getLabelSet();
	}
	
	public boolean hasDependent() {
		return hasLeftDependent() || hasRightDependent();
	}
	
	/**
	 * Returns <code>true</code> if the node has one or more left dependents, otherwise <code>false</code>.
	 * 
	 * @return <code>true</code> if the node has one or more left dependents, otherwise <code>false</code>.
	 */
	public boolean hasLeftDependent() {
		return !leftDependents.isEmpty();
	}
	
	/**
	 * Returns the left dependent at the position <code>index</code>, where <code>index==0</code> equals the left most dependent.
	 * 
	 * @param index the index
	 * @return the left dependent at the position <code>index</code>, where <code>index==0</code> equals the left most dependent
	 */
	public DependencyNode getLeftDependent(int index) {
		if (0 <= index && index < leftDependents.size()) {
			int i = 0;
//			DependencyNode candidate = null;
			
			for (DependencyNode node : leftDependents) {
//				candidate = node;
				if (i == index) {
					return node;
				}
				i++;
			}
		}
		return null;
	}
	
	/**
	 * Return the number of left dependents
	 * 
	 * @return the number of left dependents
	 */
	public int getLeftDependentCount() {
		return leftDependents.size();
	}
	
	public SortedSet<DependencyNode> getLeftDependents() {
		return leftDependents;
	}
	
	/**
	 * Returns the left sibling if it exists, otherwise <code>null</code>
	 * 
	 * @return the left sibling if it exists, otherwise <code>null</code>
	 */
	public DependencyNode getLeftSibling() throws MaltChainedException {
		if (getHead() == null) {
			return null;
		}

		DependencyNode candidate = null;
		for (DependencyNode node : getHead().getLeftDependents()) {
			if (node == this) {
				return candidate;
			}
			candidate = node;
		}
		for (DependencyNode node : getHead().getRightDependents()) {
			if (node == this) {
				return candidate;
			}
			candidate = node;
		}
		return null;
	}
	
	/**
	 * Returns the left sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 * 
	 * @return the left sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 */
	public DependencyNode getSameSideLeftSibling() throws MaltChainedException {
		if (getHead() == null) {
			return null;
		} else if (this.getIndex() < getHead().getIndex()) {
			try {
				return getHead().getLeftDependents().headSet(this).last();
			} catch (NoSuchElementException e) {
				return null;
			}
		} else if (this.getIndex() > getHead().getIndex()) {
			try {
				return getHead().getRightDependents().headSet(this).last();
			} catch (NoSuchElementException e) {
				return null;
			}
		}
		return null;
	}
	
	/**
	 * Returns the closest left dependent to the node it self, if not found <code>null</code> is returned.
	 * 
	 * @return the closest left dependent to the node it self, if not found <code>null</code> is returned.
	 */
	public DependencyNode getClosestLeftDependent() {
		try {
			return leftDependents.last();
		} catch (NoSuchElementException e) {
			return null;
		}
	}
	
	public DependencyNode getLeftmostDependent() {
		for (DependencyNode dep : leftDependents) {
			return dep;
		}
		return null;
//		try {
//			return leftDependents.first();
//		} catch (NoSuchElementException e) {
//			return null;
//		}
	}
	
	public DependencyNode getRightDependent(int index) {
		int size = rightDependents.size();
		if (index < size) {
			return rightDependents.toArray(new DependencyNode[size])[size - 1 - index];
		}
		return null;
//		if (0 <= index && index < rightDependents.size()) {
//			int i = 0;
//			DependencyNode candidate = null;
//			
//			for (DependencyNode node : rightDependents) {
//				candidate = node;
//				if (i == index) {
//					return candidate;
//				}
//				i++;
//			}
//		}
//		return null;
	}
	
	/**
	 * Return the number of right dependents
	 * 
	 * @return the number of right dependents
	 */
	public int getRightDependentCount() {
		return rightDependents.size();
	}
	
	/**
	 * Returns a sorted set of right dependents.
	 * 
	 * @return a sorted set of right dependents.
	 */
	public SortedSet<DependencyNode> getRightDependents() {
		return rightDependents;
	}
	
	/**
	 * Returns the right sibling if it exists, otherwise <code>null</code>
	 * 
	 * @return the right sibling if it exists, otherwise <code>null</code>
	 */
	public DependencyNode getRightSibling() throws MaltChainedException {
		if (getHead() == null) {
			return null;
		}

		for (DependencyNode node : getHead().getLeftDependents()) {
			if (node.getIndex() > this.getIndex()) {
				return node;
			}
		}
		for (DependencyNode node : getHead().getRightDependents()) {
			if (node.getIndex() > this.getIndex()) {
				return node;
			}
		}
		return null;
	}
	
	/**
	 * Returns the right sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 * 
	 * @return the right sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 */
	public DependencyNode getSameSideRightSibling() throws MaltChainedException {
		if (getHead() == null) {
			return null;
		} else if (this.getIndex() < getHead().getIndex()) {
			final SortedSet<DependencyNode> tailSet = getHead().getLeftDependents().tailSet(this);
			if (tailSet.size() <= 1)  {
				return null;
			}
			return tailSet.toArray(new DependencyNode[tailSet.size()])[1];
		} else if (this.getIndex() > getHead().getIndex()) {
			final SortedSet<DependencyNode> tailSet = getHead().getRightDependents().tailSet(this);
			if (tailSet.size() <= 1)  {
				return null;
			}
			return tailSet.toArray(new DependencyNode[tailSet.size()])[1];
		}
		return null;
	}
	
	/**
	 * Returns the closest right dependent to the node it self, if not found <code>null</code> is returned.
	 * 
	 * @return the closest right dependent to the node it self, if not found <code>null</code> is returned.
	 */
	public DependencyNode getClosestRightDependent() {
		for (DependencyNode dep : rightDependents) {
			return dep;
		}
		return null;
//		try {
//			return rightDependents.first();
//		} catch (NoSuchElementException e) {
//			return null;
//		}
	}
	
	public DependencyNode getRightmostDependent() {
		int n = rightDependents.size();
		int i = 1;
		for (DependencyNode node : rightDependents) {
			if (i == n) {
				return node;
			}
			i++;
		}
		return null;
//		try {
//			return rightDependents.last();
//		} catch (NoSuchElementException e) {
//			return null;
//		}
	}
	
	protected void getDependencyDominationSet(SortedSet<DependencyNode> dominationSet) {
		if (leftDependents.size() > 0 || rightDependents.size() > 0) {
			dominationSet.addAll(leftDependents);
			dominationSet.addAll(rightDependents);
			
			for (DependencyNode node : leftDependents) {
				((Token)node).getDependencyDominationSet(dominationSet);
			}
			for (DependencyNode node : rightDependents) {
				((Token)node).getDependencyDominationSet(dominationSet);
			}
		}
	}
	
//	private SortedSet<DependencyNode> getDependencyDominationSet() {
//		SortedSet<DependencyNode> dominationSet = new TreeSet<DependencyNode>();
//		getDependencyDominationSet(dominationSet);
//		return dominationSet;
//	}
	
	
	/**
	 * Returns <code>true</code> if the node has one or more right dependents, otherwise <code>false</code>.
	 * 
	 * @return <code>true</code> if the node has one or more right dependents, otherwise <code>false</code>.
	 */
	public boolean hasRightDependent() {
		return !rightDependents.isEmpty();
	}

	public boolean isProjective() throws MaltChainedException {
		if (hasHead() && !getHead().isRoot()) {
			final DependencyNode head = getHead();
			if (getHead().getIndex() < this.getIndex()) {
				TokenNode terminals = ((TokenNode)head);
				DependencyNode tmp = null;
				while (true) {
					if (terminals == null || terminals.getSuccessor() == null) {
						return false;
					}
					if (terminals.getSuccessor() == this) {
						break;
					}
					tmp = terminals = terminals.getSuccessor();
					while (tmp != this && tmp != head) {
						if (!tmp.hasHead()) {
							return false;
						}
						tmp = tmp.getHead();
					}
				}
			} else {
				TokenNode terminals = ((TokenNode)this);
				DependencyNode tmp = null;
				while (true) {
					if (terminals == null || terminals.getSuccessor() == null) {
						return false;
					}
					if (terminals.getSuccessor() == head) {
						break;
					}
					tmp = terminals = terminals.getSuccessor();
					while (tmp != this && tmp != head) {
						if (!tmp.hasHead()) {
							return false;
						}
						tmp = tmp.getHead();
					}
				}
			}
		}
		return true;
	}

	public int getDependencyNodeDepth() throws MaltChainedException {
		DependencyNode tmp = this;
		int depth = 0;
		while (tmp.hasHead()) {
			depth++;
			tmp = tmp.getHead();
		}
		return depth;
	}
	
	public void clear() throws MaltChainedException {
		super.clear();
		predecessor = null;
		successor = null;
		component = this;
		rank = 0;
		parent = null;
		heads.clear();
		leftDependents.clear();
		rightDependents.clear();
	}
	
	@Override
	public int compareTo(ComparableNode that) {
		final int BEFORE = -1;
	    final int EQUAL = 0;
	    final int AFTER = 1;
	    if (this == that) return EQUAL;

	    if (that instanceof TokenNode) {
		    if (this.index < that.getCompareToIndex()) return BEFORE;
		    if (this.index > that.getCompareToIndex()) return AFTER;
		    return super.compareTo(that);
	    }
	    if (that instanceof NonTerminalNode) {
	    	try {
			    final int thisLCorner = this.index;
			    final int thatLCorner = that.getLeftmostProperDescendantIndex();
			    final int thisRCorner = this.index;
			    final int thatRCorner = that.getRightmostProperDescendantIndex();

//			    if (thisLCorner == -1 || thatLCorner == -1) {
//				    if (thisRCorner < thatRCorner) return BEFORE;
//				    if (thisRCorner > thatRCorner) return AFTER;
//			    }
//			    if (thisRCorner == -1 || thatRCorner == -1) {
//				    if (thisLCorner < thatLCorner) return BEFORE;
//				    if (thisLCorner > thatLCorner) return AFTER;
//			    }
			    
			    if (thisLCorner != -1 && thatLCorner != -1 && thisRCorner != -1 && thatRCorner != -1) {
				    if (thisLCorner < thatLCorner && thisRCorner < thatRCorner) return BEFORE;
				    if (thisLCorner > thatLCorner && thisRCorner > thatRCorner) return AFTER;
				    if (thisLCorner > thatLCorner && thisRCorner < thatRCorner) return BEFORE;
				    if (thisLCorner < thatLCorner && thisRCorner > thatRCorner) return AFTER;
			    } else {
				    if (thisLCorner != -1 && thatLCorner != -1) {
					    if (thisLCorner < thatLCorner) return BEFORE;
					    if (thisLCorner > thatLCorner) return AFTER;
				    }
				    if (thisRCorner != -1 && thatRCorner != -1) {
					    if (thisRCorner < thatRCorner) return BEFORE;
					    if (thisRCorner > thatRCorner) return AFTER;
				    }
			    }
	    		
	    		
	    		
//			    final int thisLCorner = this.index;
//			    final int thatLCorner = that.getLeftmostDescendantIndex();
//			    final int thisRCorner = this.index;
//			    final int thatRCorner = that.getRightmostDescendantIndex();
//
//			    if (thisLCorner == -1 || thatLCorner == -1) {
//				    if (thisRCorner < thatRCorner) return BEFORE;
//				    if (thisRCorner > thatRCorner) return AFTER;
//			    }
//			    if (thisRCorner == -1 || thatRCorner == -1) {
//				    if (thisLCorner < thatLCorner) return BEFORE;
//				    if (thisLCorner > thatLCorner) return AFTER;
//			    }
//			    if (thisLCorner < thatLCorner && thisRCorner < thatRCorner) return BEFORE;
//			    if (thisLCorner > thatLCorner && thisRCorner > thatRCorner) return AFTER;
//			    if (thisLCorner > thatLCorner && thisRCorner < thatRCorner) return BEFORE;
//			    if (thisLCorner < thatLCorner && thisRCorner > thatRCorner) return AFTER;
	    		
	    		
	    		
//		    	int corner = that.getLeftmostDescendantIndex();
//		    	if (corner != -1) {
//		    		if (this.index < corner) return BEFORE;
//		    		if (this.index > corner) return AFTER;
//		    	}
//		    	corner = that.getRightmostDescendantIndex();
//		    	if (corner != -1) {
//			    	if (this.index < corner) return BEFORE;
//			    	if (this.index > corner) return AFTER;
//		    	}
	    	} catch (MaltChainedException e) {
				if (SystemLogger.logger().isDebugEnabled()) {
					SystemLogger.logger().debug("",e);
				} else {
					SystemLogger.logger().error(e.getMessageChain());
				}
				System.exit(1);
	    	}
	    }
	    if (this.index < that.getCompareToIndex()) return BEFORE;
	    if (this.index > that.getCompareToIndex()) return AFTER;
		return super.compareTo(that);
	}

	public boolean equals(Object obj) {
		Token v = (Token)obj;
		if (!(this.predecessor == v.predecessor && this.successor == v.successor)) return false;
		return super.equals(obj);
	}
	
	public int hashCode() {
		int hash = 7;
		hash = 31 * hash + (null == predecessor ? 0 : predecessor.hashCode());
		hash = 31 * hash + (null == successor ? 0 : successor.hashCode());
		return 31 * hash + super.hashCode();
	}
	

	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		return sb.toString();
	}
}
