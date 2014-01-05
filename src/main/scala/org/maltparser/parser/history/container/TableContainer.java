package org.maltparser.parser.history.container;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.Table;
/**
*
* @author Johan Hall
**/
public class TableContainer {
	public enum RelationToNextDecision { COMBINED, SEQUANTIAL, BRANCHED, SWITCHED, NONE }
	protected int cachedCode;
	protected final StringBuilder cachedSymbol;
	protected Table table;
	protected String name;
	private RelationToNextDecision relationToNextDecision;
	
	public TableContainer(Table table, String name, char decisionSeparator) {
		this.table = table;
		this.name = name;
		switch (decisionSeparator) {
		case '+':
			this.relationToNextDecision = RelationToNextDecision.COMBINED;
			break;
		case ',':
			this.relationToNextDecision = RelationToNextDecision.SEQUANTIAL;
			break;
		case ';':
			this.relationToNextDecision = RelationToNextDecision.BRANCHED;
			break;
		case '#':
			this.relationToNextDecision = RelationToNextDecision.BRANCHED;
			break;
		case '?':
			this.relationToNextDecision = RelationToNextDecision.SWITCHED;
			break;
		default:
			this.relationToNextDecision = RelationToNextDecision.NONE;
		}
		cachedSymbol = new StringBuilder();
		cachedCode = -1;
	}

	
	public void clearCache() {
		cachedCode = -1;
		cachedSymbol.setLength(0);
	}

	public String getSymbol(int code) throws MaltChainedException {
		if (code < 0 && !containCode(code)) {
			clearCache();
			return null;
		}
		if (cachedCode != code) {
			clearCache();
			cachedCode = code;
			cachedSymbol.append(table.getSymbolCodeToString(cachedCode));
		}
		return cachedSymbol.toString();
	}
	
	public int getCode(String symbol) throws MaltChainedException {
		if (cachedSymbol == null || !cachedSymbol.equals(symbol)) {
			clearCache();
			cachedSymbol.append(symbol);
			cachedCode = table.getSymbolStringToCode(symbol);
		}
		return cachedCode;
	}
	
	public boolean containCode(int code) throws MaltChainedException {
		if (cachedCode != code) {
			clearCache();
			cachedSymbol.append(table.getSymbolCodeToString(code));
			if (cachedSymbol == null) {
				return false;
			}
			cachedCode = code;
		}
		return true;
	}
	
	public boolean containSymbol(String symbol) throws MaltChainedException {
		if (cachedSymbol == null || !cachedSymbol.equals(symbol)) {
			clearCache();
			cachedCode = table.getSymbolStringToCode(symbol);
			if (cachedCode < 0) {
				return false;
			}
			cachedSymbol.append(symbol);
		}
		return true;
	}
	
	public boolean continueWithNextDecision(int code) throws MaltChainedException {
		if (table instanceof DecisionPropertyTable) {
			return ((DecisionPropertyTable)table).continueWithNextDecision(code);
		}
		return true;
	}
	
	public boolean continueWithNextDecision(String symbol) throws MaltChainedException {
		if (table instanceof DecisionPropertyTable) {
			return ((DecisionPropertyTable)table).continueWithNextDecision(symbol);
		}
		return true;
	}
	
	public Table getTable() {
		return table;
	}
	
	public String getTableName() {
		return table != null?table.getName():null;		
	}
	
	public String getTableContainerName() {
		return name;
	}

	public RelationToNextDecision getRelationToNextDecision() {
		return relationToNextDecision;
	}


	protected void setTable(Table table) {
		this.table = table;
	}

	protected void setName(String name) {
		this.name = name;
	}
	
	public int size() {
		return table.size();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(name);
		sb.append(" -> " );
		sb.append(cachedSymbol);
		sb.append(" = ");
		sb.append(cachedCode);
		return sb.toString();
	}
}
