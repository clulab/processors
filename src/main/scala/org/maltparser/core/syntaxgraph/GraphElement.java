package org.maltparser.core.syntaxgraph;

import java.util.Observable;
import java.util.Set;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTable;

/**
*
*
* @author Johan Hall
*/
public abstract class GraphElement extends Observable implements Element {
	private SyntaxGraph belongsToGraph;
	private LabelSet labelSet;

	public GraphElement() {
		belongsToGraph = null;
		labelSet = null;
	}
	
	/**
	 * Adds a label (a string value) to the symbol table and to the graph element. 
	 * 
	 * @param table the symbol table
	 * @param symbol a label symbol
	 * @throws MaltChainedException
	 */
	public void addLabel(SymbolTable table, String symbol) throws MaltChainedException {
		table.addSymbol(symbol);
		addLabel(table, table.getSymbolStringToCode(symbol));
	}
	
	/**
	 * Adds a label (an integer value) to the symbol table and to the graph element.
	 * 
	 * @param table the symbol table
	 * @param code a label code
	 * @throws MaltChainedException
	 */
	public void addLabel(SymbolTable table, int code) throws MaltChainedException {
		if (table.getSymbolCodeToString(code) != null) {
			if (labelSet == null) {
				if (belongsToGraph == null) {
					throw new SyntaxGraphException("The graph element doesn't belong to any graph. ");
				}
				labelSet = belongsToGraph.checkOutNewLabelSet();
			}
			labelSet.put(table, code);
			setChanged(); 
			notifyObservers(table);
		}
	}
	
	/**
	 * Adds the labels of the label set to the label set of the graph element.
	 * 
	 * @param labels a label set.
	 * @throws MaltChainedException
	 */
	public void addLabel(LabelSet labels) throws MaltChainedException {
		if (labels != null) {
			for (SymbolTable table : labels.keySet()) {
				addLabel(table, labels.get(table));
			}
		}
	}
	
	/**
	 * Returns <i>true</i> if the graph element has a label for the symbol table, otherwise <i>false</i>.
	 * 
	 * @param table the symbol table
	 * @return <i>true</i> if the graph element has a label for the symbol table, otherwise <i>false</i>.
	 * @throws MaltChainedException
	 */
	public boolean hasLabel(SymbolTable table) throws MaltChainedException {
		if (labelSet != null) {
			return labelSet.containsKey(table);
		}
		return false;
	}
	
	/**
	 * Returns the label symbol(a string representation) of the symbol table if it exists, otherwise 
	 * an exception is thrown.
	 * 
	 * @param table the symbol table
	 * @return the label (a string representation) of the symbol table if it exists.
	 * @throws MaltChainedException
	 */
	public String getLabelSymbol(SymbolTable table) throws MaltChainedException {
		Integer code = labelSet.get(table);
		if (code == null) {
			throw new SyntaxGraphException("No label symbol available for label '"+table.getName()+"'.");
		}
		return table.getSymbolCodeToString(code);
	}
	
	/**
	 * Returns the label code (an integer representation) of the symbol table if it exists, otherwise 
	 * an exception is thrown.
	 * 
	 * @param table the symbol table
	 * @return the label code (an integer representation) of the symbol table if it exists
	 * @throws MaltChainedException
	 */
	public int getLabelCode(SymbolTable table) throws MaltChainedException {
		Integer code = labelSet.get(table);
		if (code == null) {
			throw new SyntaxGraphException("No label symbol available for label '"+table.getName()+"'.");
		}
//		if (code != null) {
//			return -1;
//		}
		return code.intValue();
	}
	
	/**
	 * Returns <i>true</i> if the graph element has one or more labels, otherwise <i>false</i>.
	 * 
	 * @return <i>true</i> if the graph element has one or more labels, otherwise <i>false</i>.
	 */
	public boolean isLabeled() {
		if (labelSet == null) {
			return false;
		}
		return labelSet.size() > 0;
	}
	
	/**
	 * Returns the number of labels of the graph element.
	 * 
	 * @return the number of labels of the graph element.
	 */
	public int nLabels() {
		if (labelSet == null) {
			return 0;
		}
		return labelSet.size();
	}
	
	/**
	 * Returns a set of symbol tables (labeling functions or label types) that labels the graph element.
	 * 
	 * @return a set of symbol tables (labeling functions or label types)
	 */
	public Set<SymbolTable> getLabelTypes() {
		if (labelSet == null) {
			return null;
		}
		return labelSet.keySet();
	}
	
	/**
	 * Returns the label set.
	 * 
	 * @return the label set.
	 */
	public LabelSet getLabelSet() {
		return labelSet;
	}
	
	public void removeLabel(SymbolTable table) throws MaltChainedException {
		if (labelSet != null) {
			labelSet.remove(table);
		}
	}
	
	public void removeLabels() throws MaltChainedException {
		if (labelSet != null && belongsToGraph != null) {
			belongsToGraph.checkInLabelSet(labelSet);
		}
		labelSet = null;
	}
	
	/**
	 * Returns the graph (structure) in which the graph element belongs to. 
	 * 
	 * @return the graph (structure) in which the graph element belongs to. 
	 */
	public SyntaxGraph getBelongsToGraph() {
		return belongsToGraph;
	}
	
	/**
	 * Sets the graph (structure) in which the graph element belongs to. 
	 * 
	 * @param belongsToGraph a graph (structure).
	 */
	public void setBelongsToGraph(SyntaxGraph belongsToGraph) {
		this.belongsToGraph = belongsToGraph;
		addObserver(belongsToGraph);
	}
	

	/**
	 * Resets the graph element.
	 * 
	 * @throws MaltChainedException
	 */
	public void clear() throws MaltChainedException {
		if (labelSet != null && belongsToGraph != null) {
			belongsToGraph.checkInLabelSet(labelSet);
		}
		labelSet = null;
		deleteObserver(belongsToGraph);
		belongsToGraph = null;
	}
	
	public boolean equals(Object obj) {
		GraphElement ge = (GraphElement)obj;
		return belongsToGraph == ge.getBelongsToGraph() && labelSet == null ? ge.getLabelSet() == null : labelSet.equals(ge.getLabelSet()); 
	}
	
	public int hashCode() {
		int hash = 7;
		hash = 31 * hash + (null == belongsToGraph ? 0 : belongsToGraph.hashCode());
		return 31 * hash + (null == labelSet ? 0 : labelSet.hashCode());
	}
	
	public int compareTo(GraphElement o) {
		final int BEFORE = -1;
	    final int EQUAL = 0;
	    final int AFTER = 1;
	    if ( this == o ) return EQUAL;
		
	    if (this.labelSet == null && o.labelSet != null) return BEFORE;
	    if (this.labelSet != null && o.labelSet == null) return AFTER;
	    if (this.labelSet == null && o.labelSet == null) return EQUAL;
	    
	    int comparison = EQUAL;
		for (SymbolTable table : labelSet.keySet()) {
			Integer ocode = o.labelSet.get(table);
			Integer tcode = labelSet.get(table);
			if (ocode != null && tcode != null) {
				if (!ocode.equals(tcode)) {
					try {
						comparison = table.getSymbolCodeToString(tcode).compareTo(table.getSymbolCodeToString(ocode));
						if ( comparison != EQUAL ) return comparison;
					} catch (MaltChainedException e) { }
				}
			}
		}
		return EQUAL;
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		if (labelSet != null) {
			for (SymbolTable table : labelSet.keySet()) {
				try {
					sb.append(table.getName());
					sb.append(':');
					sb.append(getLabelSymbol(table));
				} catch (MaltChainedException e) {
					System.err.println("Print error : "+e.getMessageChain());
				}
				sb.append(' ');
			}
		}
		return sb.toString();
	}
}
