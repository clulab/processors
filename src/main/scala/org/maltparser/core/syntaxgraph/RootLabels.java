package org.maltparser.core.syntaxgraph;

import java.util.SortedMap;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTable;

/**
*
*
* @author Johan Hall
*/
public class RootLabels {
	public final static String DEFAULT_ROOTSYMBOL = "ROOT";
	private final LabelSet rootLabelCodes;

	public RootLabels() {
		rootLabelCodes = new LabelSet();
	}
	
	public void setRootLabels(String rootLabelOption, SortedMap<String, SymbolTable> edgeSymbolTables) throws MaltChainedException {
		if (edgeSymbolTables == null) {
			return;
		} else if (rootLabelOption == null || rootLabelOption.trim().length() == 0) {
			for (SymbolTable table : edgeSymbolTables.values()) {
				rootLabelCodes.put(table, table.addSymbol(RootLabels.DEFAULT_ROOTSYMBOL));
			}
		} else if (rootLabelOption.trim().indexOf(',') == -1) {
			int index = rootLabelOption.trim().indexOf('=');
			if (index == -1) {
				for (SymbolTable table : edgeSymbolTables.values()) {
					rootLabelCodes.put(table, table.addSymbol(rootLabelOption.trim()));
				}
			} else {
				String name = rootLabelOption.trim().substring(0, index);
				if (edgeSymbolTables.get(name) == null) {
					throw new SyntaxGraphException("The symbol table '"+ name+"' cannot be found when defining the root symbol. ");
				} else {
					rootLabelCodes.put(edgeSymbolTables.get(name), edgeSymbolTables.get(name).addSymbol(rootLabelOption.trim().substring(index+1)));
					if (edgeSymbolTables.size() > 1) {
						for (SymbolTable table : edgeSymbolTables.values()) {
							if (!table.getName().equals(name)) {
								rootLabelCodes.put(table, table.addSymbol(RootLabels.DEFAULT_ROOTSYMBOL));					
							}
						}
					}
				}
			}
		} else {
			String[] items = rootLabelOption.trim().split(",");
			for (int i=0; i<items.length; i++) {
				int index = items[i].trim().indexOf('=');
				if (index == -1) {
					throw new SyntaxGraphException("The root symbol is undefinied. ");
				} else {
					String name = items[i].trim().substring(0, index);
					if (edgeSymbolTables.get(name) == null) {
						throw new SyntaxGraphException("The symbol table'"+ name+"' cannot be found when defining the root symbol. ");
					} else {
						rootLabelCodes.put(edgeSymbolTables.get(name), edgeSymbolTables.get(name).addSymbol(items[i].trim().substring(index+1)));
					}
				}
			}
			for (SymbolTable table : edgeSymbolTables.values()) {
				if (!rootLabelCodes.containsKey(table)) {
					rootLabelCodes.put(table, table.addSymbol(RootLabels.DEFAULT_ROOTSYMBOL));					
				}
			}
		}
	}
	
	public void setDefaultRootLabel(SymbolTable table, String defaultRootSymbol) throws MaltChainedException {
		rootLabelCodes.put(table, table.addSymbol(defaultRootSymbol));	
	}
	
	public Integer getDefaultRootLabelCode(SymbolTable table) throws MaltChainedException {
		Integer res = rootLabelCodes.get(table);
		if (res == null) {
			return table.addSymbol(RootLabels.DEFAULT_ROOTSYMBOL);
		}
		return res;
	}
	
	public LabelSet getDefaultRootLabels() throws MaltChainedException {
		return new LabelSet(rootLabelCodes);
	}
	
	public String getDefaultRootLabelSymbol(SymbolTable table) throws MaltChainedException {
		return table.getSymbolCodeToString(getDefaultRootLabelCode(table));
	}
	
	
	public boolean checkRootLabelCodes(LabelSet rlc) {
		if (rlc == null && rootLabelCodes == null) {
			return true; // or false ?
		} else if ((rlc == null && rootLabelCodes != null) || (rlc != null && rootLabelCodes == null)) {
			return false;
		} else if (rlc.size() != rootLabelCodes.size()) {
			return false;
		} else {
			for (SymbolTable table : rootLabelCodes.keySet()) {
				if (!rootLabelCodes.get(table).equals(rlc.get(table))) {
					return false;
				}
			}
			return true;
		}
	}
}
