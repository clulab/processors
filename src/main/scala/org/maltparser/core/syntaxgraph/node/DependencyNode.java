package org.maltparser.core.syntaxgraph.node;

import java.util.Set;
import java.util.SortedSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.LabelSet;
import org.maltparser.core.syntaxgraph.edge.Edge;



public interface DependencyNode extends ComparableNode {
	/**
	 * Returns <i>true</i> if the node has at most one head, otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if the node has at most one head, otherwise <i>false</i>.
	 */
	public boolean hasAtMostOneHead();
	/**
	 * Returns <i>true</i> if the node has one or more head(s), otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if the node has one or more head(s), otherwise <i>false</i>.
	 */ 
	public boolean hasHead();
	public Set<DependencyNode> getHeads() throws MaltChainedException;
	public Set<Edge> getHeadEdges() throws MaltChainedException;
	

	/**
	 * Returns the head dependency node if it exists, otherwise <i>null</i>. If there exists more
	 * than one head the first head is returned according to the linear order of the terminals 
	 * or the root if it is one of the heads.
	 * 
	 * @return the head dependency node if it exists, otherwise <i>null</i>.
	 * @throws MaltChainedException
	 */
	public DependencyNode getHead() throws MaltChainedException;
	/**
	 * Returns the edge between the head and the node if it exists, otherwise <i>null</i>. If there exists more
	 * than one head edge the first head edge is returned according to the linear order of the terminals 
	 * or the root if it is one of the heads.
	 * 
	 * @return the edge between the head and the node if it exists, otherwise <i>null</i>.
	 * @throws MaltChainedException
	 */
	public Edge getHeadEdge() throws MaltChainedException;
	public boolean hasAncestorInside(int left, int right) throws MaltChainedException;
	public void addHeadEdgeLabel(SymbolTable table, String symbol) throws MaltChainedException;
	public void addHeadEdgeLabel(SymbolTable table, int code) throws MaltChainedException;
	public void addHeadEdgeLabel(LabelSet labelSet) throws MaltChainedException;
	public boolean hasHeadEdgeLabel(SymbolTable table) throws MaltChainedException;
	public String getHeadEdgeLabelSymbol(SymbolTable table) throws MaltChainedException;
	public int getHeadEdgeLabelCode(SymbolTable table) throws MaltChainedException;
	public boolean isHeadEdgeLabeled() throws MaltChainedException;
	public int nHeadEdgeLabels() throws MaltChainedException;
	public Set<SymbolTable> getHeadEdgeLabelTypes() throws MaltChainedException;
	public LabelSet getHeadEdgeLabelSet() throws MaltChainedException;
	public DependencyNode getAncestor() throws MaltChainedException;
	public DependencyNode getProperAncestor() throws MaltChainedException;
	
	public boolean hasDependent();
	/**
	 * Returns <i>true</i> if the node has one or more left dependents, otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if the node has one or more left dependents, otherwise <i>false</i>.
	 */
	public boolean hasLeftDependent();
	/**
	 * Returns the left dependent at the position <i>index</i>, where <i>index==0</i> equals the left most dependent.
	 * 
	 * @param index the index
	 * @return the left dependent at the position <i>index</i>, where <i>index==0</i> equals the left most dependent
	 */
	public DependencyNode getLeftDependent(int index);
	/**
	 * Return the number of left dependents
	 * 
	 * @return the number of left dependents
	 */
	public int getLeftDependentCount();
	/**
	 * Returns a sorted set of left dependents.
	 * 
	 * @return a sorted set of left dependents.
	 */
	public SortedSet<DependencyNode> getLeftDependents();
	/**
	 * Returns the left sibling if it exists, otherwise <code>null</code>
	 * 
	 * @return the left sibling if it exists, otherwise <code>null</code>
	 */
	public DependencyNode getLeftSibling() throws MaltChainedException;
	/**
	 * Returns the left sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 * 
	 * @return the left sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 */
	public DependencyNode getSameSideLeftSibling() throws MaltChainedException;
	/**
	 * Returns the closest left dependent to the node it self, if not found <code>null</code> is returned.
	 * 
	 * @return the closest left dependent to the node it self, if not found <code>null</code> is returned.
	 */
	public DependencyNode getClosestLeftDependent();
	public DependencyNode getLeftmostDependent();
	public DependencyNode getRightDependent(int index);
	/**
	 * Return the number of right dependents
	 * 
	 * @return the number of right dependents
	 */
	public int getRightDependentCount();
	/**
	 * Returns a sorted set of right dependents.
	 * 
	 * @return a sorted set of right dependents.
	 */
	public SortedSet<DependencyNode> getRightDependents();
	/**
	 * Returns the right sibling if it exists, otherwise <code>null</code>
	 * 
	 * @return the right sibling if it exists, otherwise <code>null</code>
	 */
	public DependencyNode getRightSibling() throws MaltChainedException;
	/**
	 * Returns the right sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 * 
	 * @return the right sibling at the same side of head as the node it self. If not found <code>null</code is returned
	 */
	public DependencyNode getSameSideRightSibling() throws MaltChainedException;
	/**
	 * Returns the closest right dependent to the node it self, if not found <code>null</code> is returned.
	 * 
	 * @return the closest right dependent to the node it self, if not found <code>null</code> is returned.
	 */
	public DependencyNode getClosestRightDependent();
	public DependencyNode getRightmostDependent();
	public boolean hasRightDependent();
	/**
	 * Returns <i>true</i> if the head edge is projective, otherwise <i>false</i>. Undefined if the node has 
	 * more than one head.
	 * 
	 * @return <i>true</i> if the head edge is projective, otherwise <i>false</i>.
	 * @throws MaltChainedException
	 */
	public boolean isProjective() throws MaltChainedException;
	/**
	 * Returns the depth of the node. The root node has the depth 0.
	 * @return the depth of the node.
	 * @throws MaltChainedException
	 */
	public int getDependencyNodeDepth() throws MaltChainedException;
	public int getRank();
	public void setRank(int r);
	public DependencyNode findComponent();
	public DependencyNode getComponent();
	public void setComponent(DependencyNode x);
}
