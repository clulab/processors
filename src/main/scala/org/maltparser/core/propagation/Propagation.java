package org.maltparser.core.propagation;

import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.propagation.spec.PropagationSpec;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.DependencyNode;

/**
 * A propagation object propagate a column value from one node to a column in another node based on the propagation specification. 
 * 
 * @author Johan Hall
 *
 */
public class Propagation {
	/**
	 * 
	 */
	private SymbolTable fromTable;
	private SymbolTable toTable;
	private SymbolTable deprelTable;
	private SortedSet<String> forSet;
	private SortedSet<String> overSet;
	
	private Pattern symbolSeparator;
	
	/**
	 * Creates a propagation object based on the propagation specification
	 * 
	 * @param spec a propagation specification
	 * @param dataFormatInstance a data format instance
	 * @throws MaltChainedException
	 */
	public Propagation(PropagationSpec spec, DataFormatInstance dataFormatInstance) throws MaltChainedException {
		ColumnDescription fromColumn = dataFormatInstance.getColumnDescriptionByName(spec.getFrom());
		if (fromColumn == null) {
			throw new PropagationException("The symbol table '"+spec.getFrom()+" does not exists.");
		}
		fromTable = fromColumn.getSymbolTable();

		ColumnDescription toColumn = dataFormatInstance.getColumnDescriptionByName(spec.getTo());
		if (toColumn == null) {
			toColumn = dataFormatInstance.addInternalColumnDescription(spec.getTo(), fromColumn);
			toTable = toColumn.getSymbolTable();
		}

		
		forSet = new TreeSet<String>();
		if (spec.getFor() != null && spec.getFor().length() > 0) {
			String[] items = spec.getFor().split("\\|");
			
			for (String item : items) {
				forSet.add(item);
			}
		}
		
		overSet = new TreeSet<String>();
		if (spec.getOver() != null && spec.getOver().length() > 0) {
			String[] items = spec.getOver().split("\\|");
			
			for (String item : items) {
				overSet.add(item);
			}
		}
		
		ColumnDescription deprelColumn = dataFormatInstance.getColumnDescriptionByName("DEPREL");
		deprelTable = deprelColumn.getSymbolTable();
		symbolSeparator = Pattern.compile("\\|");
	}

	/**
	 * Propagate columns according to the propagation specification
	 * 
	 * @param e an edge 
	 * @throws MaltChainedException
	 */
	public void propagate(Edge e) throws MaltChainedException {
		if (e != null && e.hasLabel(deprelTable) && !e.getSource().isRoot()) {
			if (overSet.size() == 0 || overSet.contains(e.getLabelSymbol(deprelTable))) {
				DependencyNode to = (DependencyNode)e.getSource();
				DependencyNode from = (DependencyNode)e.getTarget();
				String fromSymbol = null;
				if (e.hasLabel(fromTable)) {
					fromSymbol = e.getLabelSymbol(fromTable);
				} else if (from.hasLabel(fromTable)) {
					fromSymbol = from.getLabelSymbol(fromTable);
				}
				
				String propSymbol = null;
				if (to.hasLabel(toTable)) {
					propSymbol = union(fromSymbol, to.getLabelSymbol(toTable));
				} else {
					if (forSet.size() == 0 || forSet.contains(fromSymbol)) {
						propSymbol = fromSymbol;
					}
				}
				if (propSymbol != null) {
					to.addLabel(toTable, propSymbol);
				}
			}
		}
	}
	
	private String union(String fromSymbol, String toSymbol) {
		SortedSet<String> symbolSet = new TreeSet<String>();
		
		if (fromSymbol != null && fromSymbol.length() != 0) {
			String[] fromSymbols = symbolSeparator.split(fromSymbol);
			for (int i = 0; i < fromSymbols.length; i++) {
				if (forSet.size() == 0 || forSet.contains(fromSymbols[i])) {
					symbolSet.add(fromSymbols[i]);
				}
			}
		}
		if (toSymbol != null && toSymbol.length() != 0) {
			String[] toSymbols = symbolSeparator.split(toSymbol);
			for (int i = 0; i < toSymbols.length; i++) {
				symbolSet.add(toSymbols[i]);
			}
		}
		
		if (symbolSet.size() > 0) {
			StringBuilder sb = new StringBuilder();
			for (String symbol : symbolSet) {
				sb.append(symbol);
				sb.append('|');
			}
			sb.setLength(sb.length()-1);
			return sb.toString();
		}

		
		return "";
	}
	@Override
	public String toString() {
		return "Propagation [forSet=" + forSet + ", fromTable=" + fromTable
				+ ", overSet=" + overSet + ", toTable=" + toTable + "]";
	}
}
