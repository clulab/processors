package org.maltparser.core.syntaxgraph;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTableHandler;
/**
*
*
* @author Johan Hall
*/
public interface LabeledStructure {
	/**
	 * Returns the symbol table handler. 
	 * 
	 * @return the symbol table handler. 
	 */
	public SymbolTableHandler getSymbolTables();
	/**
	 * Sets the symbol table handler.
	 * 
	 * @param symbolTables a symbol table handler.
	 */
	public void setSymbolTables(SymbolTableHandler symbolTables);
	/**
	 * Adds a label <i>label</i> to the graph element <i>element</i>
	 * 
	 * @param element a graph element <i>element</i> (a node or a edge).
	 * @param tableName the name of the symbol table.
	 * @param label the string value of the label.
	 * @throws MaltChainedException
	 */
	public void addLabel(Element element, String tableName, String label) throws MaltChainedException;
	/**
	 * Checks out a new label set from the structure.
	 * 
	 * @return a new label set.
	 * @throws MaltChainedException
	 */
	public LabelSet checkOutNewLabelSet() throws MaltChainedException;
	/**
	 * Checks in a label set. 
	 * 
	 * @param labelSet a label set. 
	 * @throws MaltChainedException
	 */
	public void checkInLabelSet(LabelSet labelSet) throws MaltChainedException;
}
