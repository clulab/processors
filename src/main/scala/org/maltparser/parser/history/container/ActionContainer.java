package org.maltparser.parser.history.container;


import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.Table;
/**
*
* @author Johan Hall
**/
public class ActionContainer {
	private int actionCode;
	private String actionSymbol;
	private Table table;
	private String name;
	
	public ActionContainer(TableContainer tableContainer) {
		this.table = tableContainer.getTable();
		this.name = tableContainer.getTableContainerName();
		clear();
	}
	
	public void clear() {
		actionCode = -1;
		actionSymbol = null;
	}

	public String getActionSymbol() {
		return actionSymbol;
	}
	
	public int getActionCode() {
		return actionCode;
	}
	
	public String setAction(int code) throws MaltChainedException {
		if (actionCode != code) {
			if (code < 0) {
				clear();
			} else {
				actionSymbol = table.getSymbolCodeToString(code);
				if (actionSymbol == null) {
					clear();
				} else {
					actionCode = code;
				}
			}
		}
		return actionSymbol;
	}
	
	public int setAction(String symbol) throws MaltChainedException {
		if (symbol == null) {
			clear();
		} else {
			actionCode = table.getSymbolStringToCode(symbol);
			if (actionCode == -1) {
				clear();
			} else {
				actionSymbol = symbol;
			}
		}
		return actionCode;
	}
	
	public Table getTable() {
		return table;
	}
	
	public String getTableName() {
		if (table == null) {
			return null;
		}
		return table.getName();		
	}
	
	public String getTableContainerName() {
		return name;
	}


	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(name);
		sb.append(" -> ");
		sb.append(actionSymbol);
		sb.append(" = ");
		sb.append(actionCode);
		return sb.toString();
	}
}
