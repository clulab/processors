package org.maltparser.core.symbol.nullvalue;

import java.util.SortedMap;
import java.util.TreeMap;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.HashMap;
import org.maltparser.core.symbol.SymbolException;
import org.maltparser.core.symbol.SymbolTable;
/**


@author Johan Hall
@since 1.0
 */
public abstract class NullValues {
	protected enum NullValueDegree {
		NONE, ONE, ROOTNODE, NOVALUE
	};
	public enum NullValueId {
		NO_NODE, ROOT_NODE, NO_VALUE
	};
	protected HashMap<NullValueId, String> nullValue2SymbolMap;
	protected HashMap<NullValueId, Integer> nullValue2CodeMap;
	protected HashMap<String, Integer> symbol2CodeMap;
	protected SortedMap<Integer, String> code2SymbolMap;
	protected SymbolTable table;
	protected NullValueDegree nullValueEncoding;
	protected String nullValueStrategy;
	protected int nextCode;
	
	public NullValues(SymbolTable table) {
		setSymbolTable(table);
		nullValue2SymbolMap = new HashMap<NullValueId, String>();
		nullValue2CodeMap = new HashMap<NullValueId, Integer>();
		symbol2CodeMap = new HashMap<String, Integer>();
		code2SymbolMap = new TreeMap<Integer, String>();
	}
	
	private void setSymbolTable(SymbolTable table) {
		this.table = table; 
	}
	
	public SymbolTable getSymbolTable() {
		return table;
	}
	
	public String getNullValueStrategy() {
		return nullValueStrategy;
	}

	protected void setNullValueStrategy(String nullValueStrategy) {
		this.nullValueStrategy = nullValueStrategy;
	}

	public NullValueDegree getNullValueEncoding() {
		return nullValueEncoding;
	}
	
	public int getNextCode() {
		return nextCode;
	}

	protected void setNextCode(int nextCode) {
		this.nextCode = nextCode;
	}
	
	public boolean isNullValue(int code) {
		return code2SymbolMap.containsKey(code);
	}
	
	public boolean isNullValue(String symbol) {
		return symbol2CodeMap.containsKey(symbol);
	}
	
	public boolean isNullValue(StringBuilder symbol) {
		return symbol2CodeMap.containsKey(symbol);
	}
	
	public int nullvalueToCode(NullValueId nullValueIdentifier) throws MaltChainedException {
		if (!nullValue2CodeMap.containsKey(nullValueIdentifier)) {
			throw new SymbolException("Illegal null-value identifier. ");
		}
		return nullValue2CodeMap.get(nullValueIdentifier);
	}
	
	public String nullvalueToSymbol(NullValueId nullValueIdentifier) throws MaltChainedException {
		if (!nullValue2SymbolMap.containsKey(nullValueIdentifier)) {
			throw new SymbolException("Illegal null-value identifier. ");
		}
		return nullValue2SymbolMap.get(nullValueIdentifier);
	}
	
	public int symbolToCode(String symbol) {
		if (!symbol2CodeMap.containsKey(symbol)) {
			return -1;
		}
		return symbol2CodeMap.get(symbol);
	}
	
	public int symbolToCode(StringBuilder symbol) {
		if (!symbol2CodeMap.containsKey(symbol)) {
			return -1;
		}
		return symbol2CodeMap.get(symbol);
	}
	
	public String codeToSymbol(int code) {
		if (!code2SymbolMap.containsKey(code)) {
			return null;
		}
		return code2SymbolMap.get(code);
	}
	
	protected abstract void setNullValueEncoding(String nullValueStrategy);
	protected abstract void makeNullValues();
	
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		
		NullValues nl = (NullValues)obj;
		if (!nullValueStrategy.equalsIgnoreCase(nl.getNullValueStrategy())) { return false; }
		if (nextCode != nl.getNextCode()) { return false; }
		if (!nullValue2SymbolMap.equals(nl.nullValue2SymbolMap)) { return false; }
		if (!nullValue2CodeMap.equals(nl.nullValue2CodeMap)) { return false; }	
		if (!code2SymbolMap.equals(nl.code2SymbolMap)) { return false; }
		if (!symbol2CodeMap.equals(nl.symbol2CodeMap)) { return false; }
		return true;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("Null-values:\n");
		sb.append("  Strategy: "+ nullValueStrategy);
		sb.append("  NO_NODE -> "+ nullValue2CodeMap.get(NullValueId.NO_NODE) + " " + nullValue2SymbolMap.get(NullValueId.NO_NODE)+"\n");
		sb.append("  ROOT_NODE -> "+ nullValue2CodeMap.get(NullValueId.ROOT_NODE) + " " + nullValue2SymbolMap.get(NullValueId.ROOT_NODE)+"\n");
		sb.append("  NO_VALUE -> "+ nullValue2CodeMap.get(NullValueId.NO_VALUE) + " " + nullValue2SymbolMap.get(NullValueId.NO_VALUE)+"\n");
		return sb.toString();
	}
}
