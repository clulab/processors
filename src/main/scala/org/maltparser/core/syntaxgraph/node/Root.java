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
import org.maltparser.core.syntaxgraph.TokenStructure;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.headrules.Direction;
import org.maltparser.core.syntaxgraph.headrules.HeadRules;


public class Root extends GraphNode implements DependencyNode, PhraseStructureNode, NonTerminalNode {
	protected final SortedSet<DependencyNode> leftDependents;
	protected final SortedSet<DependencyNode> rightDependents;
	protected final SortedSet<PhraseStructureNode> children;

	/**
	 * a reference to a node where the node is part of a component. If the node is unconnected it will reference to it self. 
	 */
	protected DependencyNode component;
	protected int rank;
	public Root() throws MaltChainedException {
		super();
		leftDependents = new TreeSet<DependencyNode>();
		rightDependents = new TreeSet<DependencyNode>();
		children = new TreeSet<PhraseStructureNode>();
		clear();
	}
	
	public void addIncomingEdge(Edge in) throws MaltChainedException { 
		throw new SyntaxGraphException("It is not allowed for a root node to have an incoming edge");
	}
	
	public void removeIncomingEdge(Edge in) { }

	public void addOutgoingEdge(Edge out) throws MaltChainedException {
		super.addOutgoingEdge(out);
		if (out.getTarget() != null) {
			if (out.getType() == Edge.DEPENDENCY_EDGE && out.getTarget() instanceof DependencyNode) {
				Node dependent = out.getTarget();
				if (compareTo(dependent) > 0) {
					leftDependents.add((DependencyNode)dependent);
				} else if (compareTo(dependent) < 0) {
					rightDependents.add((DependencyNode)dependent);
				}
			} else if (out.getType() == Edge.PHRASE_STRUCTURE_EDGE && out.getTarget() instanceof PhraseStructureNode) {
				children.add((PhraseStructureNode)out.getTarget());
			}
		}
	}
	
	public void removeOutgoingEdge(Edge out) throws MaltChainedException {
		super.removeOutgoingEdge(out);
		if (out.getTarget() != null) {
			if (out.getType() == Edge.DEPENDENCY_EDGE && out.getTarget() instanceof DependencyNode) {
				Node dependent = out.getTarget();
				if (compareTo(dependent) > 0) {
					leftDependents.remove((DependencyNode)dependent);
				} else if (compareTo(dependent) < 0) {
					rightDependents.remove((DependencyNode)dependent);
				}
			} else if (out.getType() == Edge.PHRASE_STRUCTURE_EDGE && out.getTarget() instanceof PhraseStructureNode) {
				children.remove((PhraseStructureNode)out.getTarget());
			}
		}
	}
	
	public DependencyNode getAncestor() throws MaltChainedException {
		return this;
	}
	
	public DependencyNode getProperAncestor() throws MaltChainedException {
		return null;
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
	
	
	public boolean isContinuous() {
		return true;
	}
	
	public boolean isContinuousExcludeTerminalsAttachToRoot() {
		return true;
	}
	
	public PhraseStructureNode getParent() {
		return null;
	}
	
	public Edge getParentEdge() throws MaltChainedException {
		return null;
	}
	
	public String getParentEdgeLabelSymbol(SymbolTable table) throws MaltChainedException {
		return null;
	}
	
	public int getParentEdgeLabelCode(SymbolTable table) throws MaltChainedException {
		return -1;
	}
	
	public boolean hasParentEdgeLabel(SymbolTable table) throws MaltChainedException {
		return false;
	}
	
	public SortedSet<PhraseStructureNode> getChildren() {
		return new TreeSet<PhraseStructureNode>(children);
	}

	public PhraseStructureNode getChild(int index) {
		if (index >= 0 && index < children.size()) {
			return children.toArray(new PhraseStructureNode[children.size()])[index];
		}
		return null;
	}
	
	public PhraseStructureNode getLeftChild() {
		for (PhraseStructureNode node : children) {
			return node;
		}
		return null;
	}
	
	public PhraseStructureNode getRightChild() {
		int n = children.size();
		int i = 1;
		for (PhraseStructureNode node : children) {
			if (i == n) {
				return node;
			}
		}
		return null;
	}
	
	public int nChildren() {
		return children.size();
	}
	
	public boolean hasNonTerminalChildren() {
		for (PhraseStructureNode node : children) {
			if (node instanceof NonTerminal) {
				return true;
			}
		}
		return false;
	}
	
	public boolean hasTerminalChildren() {
		for (PhraseStructureNode node : children) {
			if (node instanceof Token) {
				return true;
			}
		}
		return false;
	}
	
	public int getHeight() {
		int max = -1;
		for (PhraseStructureNode node : children) {
			if (node instanceof Token) {
				if (max < 0) {
					max = 0;
				}
			} else {
				int nodeheight = ((NonTerminalNode)node).getHeight();
				if (max < nodeheight) {
					max = nodeheight;
				}
			}
		}
		return max + 1;
	}
	
	public TokenNode getLexicalHead(HeadRules headRules) throws MaltChainedException {
		return identifyHead(headRules);
	}

	public PhraseStructureNode getHeadChild(HeadRules headRules) throws MaltChainedException {
		return identifyHeadChild(headRules);
	}
	
	public TokenNode getLexicalHead() throws MaltChainedException {
		return identifyHead(null);
	}

	public PhraseStructureNode getHeadChild() throws MaltChainedException {
		return identifyHeadChild(null);
	}
	
	private PhraseStructureNode identifyHeadChild(HeadRules headRules) throws MaltChainedException {
		PhraseStructureNode headChild = (headRules == null)?null:headRules.getHeadChild(this);
		if (headChild == null) {
			Direction direction = (headRules == null)?Direction.LEFT:headRules.getDefaultDirection(this);
			if (direction == Direction.LEFT) {
				if ((headChild = leftmostTerminalChild()) == null) {
					headChild = leftmostNonTerminalChild();
				}
			} else {
				if ((headChild = rightmostTerminalChild()) == null) {
					headChild = rightmostNonTerminalChild();
				}
			}
		}
		return headChild;
	}
	
	public TokenNode identifyHead(HeadRules headRules) throws MaltChainedException {
		PhraseStructureNode headChild = identifyHeadChild(headRules);
		TokenNode lexicalHead = null;
		if (headChild instanceof NonTerminalNode) {
			lexicalHead = ((NonTerminalNode)headChild).identifyHead(headRules);
		} else if (headChild instanceof TokenNode) {
			lexicalHead = (TokenNode)headChild;
		}
		for (PhraseStructureNode node : children) {
			if (node != headChild && node instanceof NonTerminalNode) {
				((NonTerminalNode)node).identifyHead(headRules);
			}
		}

		return lexicalHead;
	}
	
	private PhraseStructureNode leftmostTerminalChild() {
		for (PhraseStructureNode node : children) {
			if (node instanceof TokenNode) {
				return node;
			}
		}
		return null;
	}
	
	private PhraseStructureNode leftmostNonTerminalChild() {
		for (PhraseStructureNode node : children) {
			if (node instanceof NonTerminalNode) {
				return node;
			}
		}
		return null;
	}
	
	private PhraseStructureNode rightmostTerminalChild() {
		try {
			if (children.last() instanceof TokenNode) {
				return children.last();
			}
		} catch (NoSuchElementException e) { }
		
		PhraseStructureNode candidate = null;
		for (PhraseStructureNode node : children) {
			if (node instanceof TokenNode) {
				candidate = node;
			}
		}
		return candidate;
	}
	
	private PhraseStructureNode rightmostNonTerminalChild() {
		try {
			if (children.last() instanceof NonTerminalNode) {
				return children.last();
			}
		} catch (NoSuchElementException e) { }
		
		PhraseStructureNode candidate = null;
		for (PhraseStructureNode node : children) {
			if (node instanceof NonTerminalNode) {
				candidate = node;
			}
		}
		return candidate;
	}
	
	public boolean hasAtMostOneHead() {
		return true;
	}
	
	public boolean hasAncestorInside(int left, int right) throws MaltChainedException {
		return false;
	}
	
	public boolean hasHead() {
		return false;
	}
	
	
	public DependencyNode getHead() throws MaltChainedException {
		return null;
	}
	
	public Edge getHeadEdge() throws MaltChainedException {
		return null;
	}
	
	public void addHeadEdgeLabel(SymbolTable table, String symbol) throws MaltChainedException { }
	
	public void addHeadEdgeLabel(SymbolTable table, int code) throws MaltChainedException { }
	
	public void addHeadEdgeLabel(LabelSet labelSet) throws MaltChainedException { }
	
	public int getHeadEdgeLabelCode(SymbolTable table) throws MaltChainedException {
		return 0;
	}

	public LabelSet getHeadEdgeLabelSet() throws MaltChainedException {
		return null;
	}


	public String getHeadEdgeLabelSymbol(SymbolTable table) throws MaltChainedException {
		return null;
	}

	public Set<SymbolTable> getHeadEdgeLabelTypes() throws MaltChainedException {
		return null;
	}

	public boolean hasHeadEdgeLabel(SymbolTable table) throws MaltChainedException {
		return false;
	}

	public boolean isHeadEdgeLabeled() throws MaltChainedException {
		return false;
	}

	public int nHeadEdgeLabels() throws MaltChainedException {
		return 0;
	}

	public Set<Edge> getHeadEdges() throws MaltChainedException {
		return null;
	}

	public Set<DependencyNode> getHeads() throws MaltChainedException {
		return null;
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
		return null;
	}
	
	/**
	 * Returns the left sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 * 
	 * @return the left sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 */
	public DependencyNode getSameSideLeftSibling() throws MaltChainedException {
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
	
	public SortedSet<DependencyNode> getRightDependents() {
		return rightDependents;
	}

	/**
	 * Returns the right sibling if it exists, otherwise <code>null</code>
	 * 
	 * @return the right sibling if it exists, otherwise <code>null</code>
	 */
	public DependencyNode getRightSibling() throws MaltChainedException {
		return null;
	}
	
	/**
	 * Returns the right sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 * 
	 * @return the right sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 */
	public DependencyNode getSameSideRightSibling() throws MaltChainedException {
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
	
	/**
	 * Returns <code>true</code> if the node has one or more right dependents, otherwise <code>false</code>.
	 * 
	 * @return <code>true</code> if the node has one or more right dependents, otherwise <code>false</code>.
	 */
	public boolean hasRightDependent() {
		return !rightDependents.isEmpty();
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
	
	public boolean isProjective() throws MaltChainedException {
		return true;
	}
	
	public int getDependencyNodeDepth() throws MaltChainedException {
		return 0;
	}
	@Override
	public int getIndex() {
		return 0;
	}

	public int getCompareToIndex() {
		return 0;
	}
	
	@Override
	public boolean isRoot() {
		return true;
	}
	
	public ComparableNode getLeftmostProperDescendant() throws MaltChainedException {
		NonTerminalNode node = this;
		ComparableNode candidate = null;
		while (node != null) {
			candidate = node.getLeftChild();
			if (candidate == null || candidate instanceof TokenNode) {
				break;
			}
			node = (NonTerminalNode)candidate;
		}
		
		if (candidate == null && candidate instanceof NonTerminalNode) {
			candidate = null;
			DependencyNode dep = null;
			for (int index : ((TokenStructure)getBelongsToGraph()).getTokenIndices()) {
				dep = ((TokenStructure)getBelongsToGraph()).getTokenNode(index);
				while (dep != null) {
					if (dep == this ) {
						return dep;
					}
					dep = dep.getHead();
				}
			}
		}
		return candidate;	
	}
	
	public ComparableNode getRightmostProperDescendant() throws MaltChainedException {
		NonTerminalNode node = this;
		ComparableNode candidate = null;
		while (node != null) {
			candidate = node.getRightChild();
			if (candidate == null || candidate instanceof TokenNode) {
				break;
			}
			node = (NonTerminalNode)candidate;
		}
		if (candidate == null && candidate instanceof NonTerminalNode) {
			candidate = null;
			DependencyNode dep = null;
			for (int i = ((TokenStructure)getBelongsToGraph()).getHighestTokenIndex(); i > 0; i--) {
				dep = ((TokenStructure)getBelongsToGraph()).getTokenNode(i);
				while (dep != null) {
					if (dep == this ) {
						return dep;
					}
					dep = dep.getHead();
				}
			}
		}
		return candidate;
	}
	
	public ComparableNode getLeftmostDescendant() throws MaltChainedException {
		return getLeftmostProperDescendant();
	}
	
	public ComparableNode getRightmostDescendant() throws MaltChainedException {
		return getRightmostProperDescendant();
	}
	
//	public void reArrangeChildrenAccordingToLeftAndRightProperDesendant() throws MaltChainedException {
//		int i = 0;
//		int leftMostCorner = -1;
//		int leftMostCornerChildIndex = -1;
//		while (i < children.size()) {	
//			if (leftMostCorner == -1) {
//				leftMostCorner = getChild(i).getLeftmostProperDescendantIndex();
//				leftMostCornerChildIndex = i;
//			} else if (getChild(i) instanceof NonTerminal && getChild(i).getLeftmostProperDescendantIndex() < leftMostCorner) {
//				NonTerminalNode child = (NonTerminal)getChild(i);
//				PhraseStructureNode leftMostCornerChild = getChild(leftMostCornerChildIndex);
//				if (leftMostCornerChild.getParent() != null) {
//					LabelSet labelSet = leftMostCornerChild.getParentEdge().getLabelSet();
//					((PhraseStructure)getBelongsToGraph()).removePhraseStructureEdge(this, leftMostCornerChild);
//					Edge e = ((PhraseStructure)getBelongsToGraph()).addPhraseStructureEdge(child, leftMostCornerChild);
//					e.addLabel(labelSet);
//				}
//				child.reArrangeChildrenAccordingToLeftAndRightProperDesendant();
//				i = -1;
//				leftMostCorner = -1;
//				leftMostCornerChildIndex = -1;
//			} else {
//				leftMostCorner = getChild(i).getLeftmostProperDescendantIndex();
//				leftMostCornerChildIndex = i;
//			}
//			i++;
//		}
//
//		for (int j = 0, n=children.size(); j < n; j++) {
//			if (getChild(j) instanceof NonTerminalNode) {
//				((NonTerminalNode)getChild(j)).reArrangeChildrenAccordingToLeftAndRightProperDesendant();
//			}
//		}
//	}
	
	@Override
	public void setIndex(int index) throws MaltChainedException { }
	
	public void clear() throws MaltChainedException {
		super.clear();
		component = this;
		rank = 0;
		leftDependents.clear();
		rightDependents.clear();
		children.clear();
	}
	
	public int compareTo(ComparableNode o) {
		final int BEFORE = -1;
	    final int EQUAL = 0;
	    final int AFTER = 1;
	    if ( this == o ) return EQUAL;
	    try {
		    final int thisLCorner = this.getLeftmostProperDescendantIndex();
		    final int thatLCorner = (o instanceof TokenNode)?o.getCompareToIndex():o.getLeftmostProperDescendantIndex();
		    final int thisRCorner = this.getRightmostProperDescendantIndex();
		    final int thatRCorner = (o instanceof TokenNode)?o.getCompareToIndex():o.getRightmostProperDescendantIndex();

//		    if (thisLCorner == -1 || thatLCorner == -1) {
//			    if (thisRCorner < thatRCorner) return BEFORE;
//			    if (thisRCorner > thatRCorner) return AFTER;
//		    }
//		    if (thisRCorner == -1 || thatRCorner == -1) {
//			    if (thisLCorner < thatLCorner) return BEFORE;
//			    if (thisLCorner > thatLCorner) return AFTER;
//		    }
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
	    	
//		    final int thisLCorner = this.getLeftmostDescendantIndex();
//		    final int thatLCorner = (o instanceof TerminalNode)?o.getCompareToIndex():o.getLeftmostDescendantIndex();
//		    final int thisRCorner = this.getRightmostDescendantIndex();
//		    final int thatRCorner = (o instanceof TerminalNode)?o.getCompareToIndex():o.getRightmostDescendantIndex();
//
//		    if (thisLCorner == -1 || thatLCorner == -1) {
//			    if (thisRCorner < thatRCorner) return BEFORE;
//			    if (thisRCorner > thatRCorner) return AFTER;
//		    }
//		    if (thisRCorner == -1 || thatRCorner == -1) {
//			    if (thisLCorner < thatLCorner) return BEFORE;
//			    if (thisLCorner > thatLCorner) return AFTER;
//		    }
//		    if (thisLCorner < thatLCorner && thisRCorner < thatRCorner) return BEFORE;
//		    if (thisLCorner > thatLCorner && thisRCorner > thatRCorner) return AFTER;
//		    if (thisLCorner > thatLCorner && thisRCorner < thatRCorner) return BEFORE;
//		    if (thisLCorner < thatLCorner && thisRCorner > thatRCorner) return AFTER;
	    	
	    	
//		    int thisCorner = this.getLeftmostDescendantIndex();
//		    int thatCorner = (o instanceof TerminalNode)?o.getCompareToIndex():o.getLeftmostDescendantIndex();
//		    if (thisCorner != -1 && thatCorner != -1) {
//			    if (thisCorner < thatCorner) return BEFORE;
//			    if (thisCorner > thatCorner) return AFTER;
//		    }
//		    thisCorner = this.getRightmostDescendantIndex();
//		    thatCorner = (o instanceof TerminalNode)?o.getCompareToIndex():o.getRightmostDescendantIndex();
//		    if (thisCorner != -1 && thatCorner != -1) {
//			    if (thisCorner < thatCorner) return BEFORE;
//			    if (thisCorner > thatCorner) return AFTER;
//		    }
		} catch (MaltChainedException e) {
			if (SystemLogger.logger().isDebugEnabled()) {
				SystemLogger.logger().debug("",e);
			} else {
				SystemLogger.logger().error(e.getMessageChain());
			}
			System.exit(1);
		}
	    if (0 < o.getCompareToIndex()) return BEFORE;
	    if (0 > o.getCompareToIndex()) return AFTER;
		return super.compareTo(o);
//		
//		if (o instanceof TerminalNode) {
//			if (0 == o.getIndex()) {
//				
//			} else if (0 < o.getIndex()) {
//				return -1;
//			} else {
//				return 1;
//			}
//		} else if (o instanceof Root) {
//			return super.compareTo(o);
//		} else if (o instanceof NonTerminalNode) {
//			int lcorner = ((NonTerminalNode)o).getLeftCornerIndex();
//			if (0 == lcorner) {
//				int rcorner = ((NonTerminalNode)o).getRightCornerIndex();
//				if (0 == rcorner) {
//					if (0 == o.getIndex()) {
//						return super.compareTo(o);
//					} else if (0 < o.getIndex()) {
//						return 1;
//					} else {
//						return -1;
//					}
//				} else if (0 < rcorner) {
//					return -1;
//				} else {
//					return 1;
//				}
//			} else if (0 < lcorner) {
//				return -1;
//			} else {
//				return 1;
//			}
//		}
//		return super.compareTo(o);
	}
	
	public boolean equals(Object obj) {
		return super.equals(obj);
	}
	
	public int hashCode() {
		return 31 * 7 + super.hashCode();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		return sb.toString();
	}
}
