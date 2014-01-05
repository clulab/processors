package org.maltparser.core.syntaxgraph;

import java.util.SortedMap;
import java.util.SortedSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
/**
*
*
* @author Johan Hall
*/
public interface DependencyStructure extends TokenStructure, SecEdgeStructure {
	public DependencyNode addDependencyNode() throws MaltChainedException;
	public DependencyNode addDependencyNode(int index) throws MaltChainedException;
	/**
	 * Returns the dependency node identified by <i>index</i> if it exists, otherwise <i>null</i>.
	 * 
	 * @param index the index of the dependency node
	 * @return the dependency node identified by <i>index</i> if it exists, otherwise <i>null</i>.
	 * @throws MaltChainedException
	 */
	public DependencyNode getDependencyNode(int index) throws MaltChainedException;
	public int nDependencyNode();


	public int getHighestDependencyNodeIndex();
	/**
	 * Adds an edge from the head to the dependent identified by the indices of the dependency nodes.
	 * 
	 * @param headIndex the index of the head dependency node
	 * @param dependentIndex the index of the dependent dependency node
	 * @return the edge that have been added.
	 * @throws MaltChainedException
	 */
	public Edge addDependencyEdge(int headIndex, int dependentIndex) throws MaltChainedException;
	/**
	 * Replace the head of the dependent with a new head. The labels are not affected.
	 * 
	 * @param newHeadIndex the index of the new head dependency node
	 * @param dependentIndex the index of the dependent dependency node
	 * @return the edge that have been moved.
	 * @throws MaltChainedException
	 */
	public Edge moveDependencyEdge(int newHeadIndex, int dependentIndex) throws MaltChainedException;
	/**
	 * Remove an edge from the head to the dependent identified by the indices of the dependency nodes.
	 * @param headIndex the index of the head dependency node
	 * @param dependentIndex the index of the dependent dependency node
	 * @throws MaltChainedException
	 */
	public void removeDependencyEdge(int headIndex, int dependentIndex) throws MaltChainedException;
	/**
	 * Returns the number of edges
	 * 
	 * @return  the number of edges
	 */
	public int nEdges();
	public SortedSet<Edge> getEdges();
	/**
	 * Returns a sorted set of integers {0,s,..n} , where each index i identifies a dependency node. Index 0
	 * should always be the root dependency node and index s is the first terminal node and index n is the
	 * last terminal node.  
	 * 
	 * @return a sorted set of integers
	 */
	public SortedSet<Integer> getDependencyIndices();
	/**
	 * Returns the root of the dependency structure.
	 * 
	 * @return the root of the dependency structure.
	 */
	public DependencyNode getDependencyRoot();
	/**
	 * Returns <i>true</i> if the head edge of the dependency node with <i>index</i> is labeled, otherwise <i>false</i>.
	 * 
	 * @param index the index of the dependency node
	 * @return <i>true</i> if the head edge of the dependency node with <i>index</i> is labeled, otherwise <i>false</i>.
	 * @throws MaltChainedException
	 */
	public boolean hasLabeledDependency(int index) throws MaltChainedException;
	/**
	 * Returns <i>true</i> if all nodes in the dependency structure are connected, otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if all nodes in the dependency structure are connected, otherwise <i>false</i>.
	 */
	public boolean isConnected();
	/**
	 * Returns <i>true</i> if all edges in the dependency structure are projective, otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if all edges in the dependency structure are projective, otherwise <i>false</i>.
	 * @throws MaltChainedException
	 */
	public boolean isProjective() throws MaltChainedException;
	/**
	 * Returns <i>true</i> if all dependency nodes have at most one incoming edge, otherwise <i>false</i>.
	 * 
	 * @return  <i>true</i> if all dependency nodes have at most one incoming edge, otherwise <i>false</i>.
	 */
	public boolean isSingleHeaded();
	/**
	 * Returns <i>true</i> if the dependency structure are a tree (isConnected() && isSingleHeaded()), otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if the dependency structure are a tree (isConnected() && isSingleHeaded()), otherwise <i>false</i>.
	 */ 
	public boolean isTree();
	/**
	 * Returns the number of non-projective edges in the dependency structure.
	 * 
	 * @return the number of non-projective edges in the dependency structure.
	 * @throws MaltChainedException
	 */
	public int nNonProjectiveEdges() throws MaltChainedException;
	/**
	 * Links all subtrees to the root of the dependency structure.
	 * 
	 * @throws MaltChainedException
	 */
	public void linkAllTreesToRoot() throws MaltChainedException;
	
	
	public LabelSet getDefaultRootEdgeLabels() throws MaltChainedException;
	/**
	 * Returns the default edge label of the root as a string value.
	 * 
	 * @param table the symbol table that identifies the label type.
	 * @return the default edge label of the root.
	 * @throws MaltChainedException
	 */
	public String getDefaultRootEdgeLabelSymbol(SymbolTable table) throws MaltChainedException;
	/**
	 * Returns the default edge label of the root as an integer value.
	 * 
	 * @param table the symbol table that identifies the label type.
	 * @return the default edge label of the root as an integer value.
	 * @throws MaltChainedException
	 */
	public int getDefaultRootEdgeLabelCode(SymbolTable table) throws MaltChainedException;
	/**
	 * Sets the default edge label of the root.
	 * 
	 * @param table the symbol table that identifies the label type.
	 * @param defaultRootSymbol the default root edge label
	 * @throws MaltChainedException
	 */
	public void setDefaultRootEdgeLabel(SymbolTable table, String defaultRootSymbol) throws MaltChainedException;
	/**
	 * Sets the default edge label of the root according to the default root label option
	 * 
	 * @param rootLabelOption the default root label option
	 * @param edgeSymbolTables a sorted map that maps the symbol table name to the symbol table object.
	 * @throws MaltChainedException
	 */
	public void setDefaultRootEdgeLabels(String rootLabelOption, SortedMap<String, SymbolTable> edgeSymbolTables) throws MaltChainedException;
}
