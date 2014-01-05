package org.maltparser.core.syntaxgraph;

import java.util.Set;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTable;
/**
*
*
* @author Johan Hall
*/
public interface Element {
	/**
	 * Adds a label (a string value) to the symbol table and to the graph element. 
	 * 
	 * @param table the symbol table
	 * @param symbol a label symbol
	 * @throws MaltChainedException
	 */
	public void addLabel(SymbolTable table, String symbol) throws MaltChainedException;
	/**
	 * Adds a label (an integer value) to the symbol table and to the graph element.
	 * 
	 * @param table the symbol table
	 * @param code a label code
	 * @throws MaltChainedException
	 */
	public void addLabel(SymbolTable table, int code) throws MaltChainedException;
	/**
	 * Adds the labels of the label set to the label set of the graph element.
	 * 
	 * @param labelSet a label set.
	 * @throws MaltChainedException
	 */
	public void addLabel(LabelSet labelSet) throws MaltChainedException;
	/**
	 * Returns <i>true</i> if the graph element has a label for the symbol table, otherwise <i>false</i>.
	 * 
	 * @param table the symbol table
	 * @return <i>true</i> if the graph element has a label for the symbol table, otherwise <i>false</i>.
	 * @throws MaltChainedException
	 */
	public boolean hasLabel(SymbolTable table) throws MaltChainedException;
	/**
	 * Returns the label symbol(a string representation) of the symbol table if it exists, otherwise 
	 * an exception is thrown.
	 * 
	 * @param table the symbol table
	 * @return the label (a string representation) of the symbol table if it exists.
	 * @throws MaltChainedException
	 */
	public String getLabelSymbol(SymbolTable table) throws MaltChainedException;
	/**
	 * Returns the label code (an integer representation) of the symbol table if it exists, otherwise 
	 * an exception is thrown.
	 * 
	 * @param table the symbol table
	 * @return the label code (an integer representation) of the symbol table if it exists
	 * @throws MaltChainedException
	 */
	public int getLabelCode(SymbolTable table) throws MaltChainedException;
	/**
	 * Returns <i>true</i> if the graph element has one or more labels, otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if the graph element has one or more labels, otherwise <i>false</i>.
	 */
	public boolean isLabeled();
	/**
	 * Returns the number of labels of the graph element.
	 * 
	 * @return the number of labels of the graph element.
	 */
	public int nLabels();
	/**
	 * Returns a set of symbol tables (labeling functions or label types) that labels the graph element.
	 * 
	 * @return a set of symbol tables (labeling functions or label types)
	 */
	public Set<SymbolTable> getLabelTypes();
	/**
	 * Returns the label set.
	 * 
	 * @return the label set.
	 */
	public LabelSet getLabelSet();
	
	public void removeLabel(SymbolTable table) throws MaltChainedException;
	public void removeLabels() throws MaltChainedException;
	
	/**
	 * Returns the graph (structure) in which the graph element belongs to. 
	 * 
	 * @return the graph (structure) in which the graph element belongs to. 
	 */
	public SyntaxGraph getBelongsToGraph();
	/**
	 * Sets the graph (structure) in which the graph element belongs to. 
	 * 
	 * @param belongsToGraph a graph (structure).
	 */
	public void setBelongsToGraph(SyntaxGraph belongsToGraph);
	/**
	 * Resets the graph element.
	 * 
	 * @throws MaltChainedException
	 */
	public void clear() throws MaltChainedException;
}
