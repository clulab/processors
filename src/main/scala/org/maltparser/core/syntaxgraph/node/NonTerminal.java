package org.maltparser.core.syntaxgraph.node;

import java.util.NoSuchElementException;
import java.util.SortedSet;
import java.util.TreeSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.SystemLogger;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;
import org.maltparser.core.syntaxgraph.TokenStructure;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.headrules.Direction;
import org.maltparser.core.syntaxgraph.headrules.HeadRules;

public class NonTerminal extends GraphNode implements PhraseStructureNode, NonTerminalNode {
	public final static int INDEX_OFFSET = 10000000;
	protected final SortedSet<PhraseStructureNode> children;
	protected PhraseStructureNode parent;
	protected int index;
	
	public NonTerminal() throws MaltChainedException {
		super();
		index = -1;
		children = new TreeSet<PhraseStructureNode>();
	}
	
	public void addIncomingEdge(Edge in) throws MaltChainedException {
		super.addIncomingEdge(in);
		if (in.getType() == Edge.PHRASE_STRUCTURE_EDGE && in.getSource() instanceof PhraseStructureNode) {
			parent = (PhraseStructureNode)in.getSource();
		}
	}
	
	public void removeIncomingEdge(Edge in) throws MaltChainedException {
		super.removeIncomingEdge(in);
		if (in.getType() == Edge.PHRASE_STRUCTURE_EDGE && in.getSource() instanceof PhraseStructureNode) {
			if (in.getSource() == parent) {
				this.parent = null;
			}
		}
	}
	
	public void addOutgoingEdge(Edge out) throws MaltChainedException {
		super.addOutgoingEdge(out);
		if (out.getType() == Edge.PHRASE_STRUCTURE_EDGE && out.getTarget() instanceof PhraseStructureNode) {
			children.add((PhraseStructureNode)out.getTarget());
//			boolean notSorted = true;
//			PhraseStructureNode prev = children.first();
//			for (PhraseStructureNode node : children) {
//				if (prev != node) {
//					if (node.compareTo(prev) == -1) {
//						notSorted = false;
//						System.out.println("NS");
//						break;
//					}
//				} 
//				prev = node;
//			}
//			if (notSorted == false) {
//				SortedSet<PhraseStructureNode> tmp = new TreeSet<PhraseStructureNode>(children);
//				children.clear();
//				for (PhraseStructureNode node : tmp) {
//					children.add(node);
//				}
//			}
		}
	}
	
	public void removeOutgoingEdge(Edge out) throws MaltChainedException {
		super.removeOutgoingEdge(out);
		if (out.getType() == Edge.PHRASE_STRUCTURE_EDGE && out.getTarget() instanceof PhraseStructureNode) {
			children.remove(out.getTarget());
		}
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
	
	public SortedSet<PhraseStructureNode> getChildren() {
		return new TreeSet<PhraseStructureNode>(children);
	}

	public PhraseStructureNode getChild(int index) {
		if (index >= 0 && index < children.size()) {
			int i = 0;
			for (PhraseStructureNode node : children) {
				if (i == index) {
					return node;
				}
				i++;
			}
//			return children.toArray(new PhraseStructureNode[children.size()])[index];
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
			i++;
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
	
	public int getIndex() {
		return index;
	}
	
	public int getCompareToIndex() {
		return index + INDEX_OFFSET;
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
	
//	protected void getPhraseDominationSet(SortedSet<PhraseStructureNode> dominationSet) {
//		for (PhraseStructureNode node : children) {
//			if (node instanceof TerminalNode) {
//				dominationSet.add(node);
//			} else {
//				((NonTerminal)node).getPhraseDominationSet(dominationSet);
//			}
//		}
//	}
//	
//	private SortedSet<PhraseStructureNode> getPhraseDominationSet() {
//		SortedSet<PhraseStructureNode> dominationSet = new TreeSet<PhraseStructureNode>();
//		getPhraseDominationSet(dominationSet);
//		return dominationSet;
//	}
	

	
	public boolean isContinuous() {
		int lcorner = getLeftmostProperDescendant().getIndex();
		int rcorner = getRightmostProperDescendant().getIndex();
		
		if (lcorner == rcorner) {
			return true;
		}
		
		TokenNode terminal = ((TokenStructure)getBelongsToGraph()).getTokenNode(lcorner);
		while (terminal.getIndex() != rcorner) {
			PhraseStructureNode tmp = terminal.getParent();
			while (true) {
				if (tmp == this) {
					break;
				}
				if (tmp == null) {
					return false;
				}
				tmp = tmp.getParent();
			}
			terminal = terminal.getSuccessor();
		}
		
		return true;
	}

	public boolean isContinuousExcludeTerminalsAttachToRoot() {
		int lcorner = getLeftmostProperDescendant().getIndex();
		int rcorner = getRightmostProperDescendant().getIndex();
		
		if (lcorner == rcorner) {
			return true;
		}
		
		TokenNode terminal = ((TokenStructure)getBelongsToGraph()).getTokenNode(lcorner);
		while (terminal.getIndex() != rcorner) {
			if (terminal.getParent() != null && terminal.getParent().isRoot()) {
				terminal = terminal.getSuccessor();
				continue;
			}
			PhraseStructureNode tmp = terminal.getParent();
			while (true) {
				if (tmp == this) {
					break;
				}
				if (tmp == null) {
					return false;
				}
				tmp = tmp.getParent();
			}
			terminal = terminal.getSuccessor();
		}
		return true;
	}
	
	@Override
	public boolean isRoot() {
		return false;
	}

	
	public ComparableNode getLeftmostProperDescendant() {
		NonTerminalNode node = this;
		PhraseStructureNode candidate = null;
		while (node != null) {
			candidate = node.getLeftChild();
			if (candidate == null || candidate instanceof TokenNode) {
				return candidate;
			}
			node = (NonTerminalNode)candidate;
		}
		return null;
	}
	
	public ComparableNode getRightmostProperDescendant() {
		NonTerminalNode node = this;
		PhraseStructureNode candidate = null;
		while (node != null) {
			candidate = node.getRightChild();
			if (candidate == null || candidate instanceof TokenNode) {
				return candidate;
			}
			node = (NonTerminalNode)candidate;
		}
		return null;
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
	public void setIndex(int index) throws MaltChainedException {
		if (index > 0) { 
			this.index = index; //INDEX_OFFSET+index;
		} else {
			throw new SyntaxGraphException("The index must be a positive index");
		}
		
	}

	public void clear() throws MaltChainedException {
		super.clear();
		children.clear();
		parent = null;
		index = -1;
	}
	
	@Override
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
	    	
//		    System.out.println(thisLCorner + " " + thatLCorner + " " +thisRCorner + " " + thatRCorner);
	    	
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
	    if (this.getCompareToIndex() < o.getCompareToIndex()) return BEFORE;
	    if (this.getCompareToIndex() > o.getCompareToIndex()) return AFTER;
		return super.compareTo(o);
	}
	
	public boolean equals(Object obj) {
		if (this == obj) return true;
		if (!(obj instanceof NonTerminal)) return false;
		return super.equals(obj);
	}
	
	public int hashCode() {
		return 31 * 7 + super.hashCode();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(getIndex());
		sb.append('\t');
		if (getLabelTypes() != null) {
			for (SymbolTable table : getLabelTypes()) {
				try {
					sb.append(getLabelSymbol(table));
				} catch (MaltChainedException e) {
					System.err.println("Print error : "+e.getMessageChain());
				}
				sb.append('\t');
			}
		}
		return sb.toString();
	}
	
}
