package org.maltparser.core.symbol.nullvalue;

import org.maltparser.core.symbol.SymbolTable;
/**


@author Johan Hall
@since 1.0
 */
public class OutputNullValues extends NullValues {

	public OutputNullValues(String nullValueStrategy, SymbolTable table) {
		super(table);

		setNullValueEncoding(nullValueStrategy);
		makeNullValues();
	}
	
	protected void setNullValueEncoding(String nullValueStrategy) {
		setNullValueStrategy(nullValueStrategy);
		if (nullValueStrategy.equalsIgnoreCase("none")) {
			nullValueEncoding = NullValueDegree.NONE;
		} else if (nullValueStrategy.equalsIgnoreCase("rootnode")) {
			nullValueEncoding = NullValueDegree.ROOTNODE;
		} else if (nullValueStrategy.equalsIgnoreCase("novalue")) {
			nullValueEncoding = NullValueDegree.NOVALUE;
		} else {
			nullValueEncoding = NullValueDegree.ONE;
		}
	}
	
	protected void makeNullValues() {
		if (nullValueEncoding == NullValueDegree.NONE || nullValueEncoding == NullValueDegree.ONE) {
			nullValue2SymbolMap.put(NullValueId.NO_NODE, "#null#");
			nullValue2SymbolMap.put(NullValueId.ROOT_NODE, "#null#");
			nullValue2SymbolMap.put(NullValueId.NO_VALUE, "#null#");
			nullValue2CodeMap.put(NullValueId.NO_NODE, 0);
			nullValue2CodeMap.put(NullValueId.ROOT_NODE, 0);
			nullValue2CodeMap.put(NullValueId.NO_VALUE, 0);
			symbol2CodeMap.put("#null#", 0);
			code2SymbolMap.put(0, "#null#");
			setNextCode(1);
		} else if (nullValueEncoding == NullValueDegree.ROOTNODE) {
			nullValue2SymbolMap.put(NullValueId.NO_NODE, "#null#");
			nullValue2SymbolMap.put(NullValueId.ROOT_NODE, "#rootnode#");
			nullValue2SymbolMap.put(NullValueId.NO_VALUE, "#rootnode#"); // maybe #null#
			nullValue2CodeMap.put(NullValueId.NO_NODE, 0);
			nullValue2CodeMap.put(NullValueId.ROOT_NODE, 1);
			nullValue2CodeMap.put(NullValueId.NO_VALUE, 1); // maybe 0
			symbol2CodeMap.put("#null#", 0);
			symbol2CodeMap.put("#rootnode#", 1);
			code2SymbolMap.put(0, "#null#");
			code2SymbolMap.put(1, "#rootnode#");
			setNextCode(2);
		} else if (nullValueEncoding == NullValueDegree.NOVALUE) {
			nullValue2SymbolMap.put(NullValueId.NO_NODE, "#null#");
			nullValue2SymbolMap.put(NullValueId.ROOT_NODE, "#rootnode#");
			nullValue2SymbolMap.put(NullValueId.NO_VALUE, "#novalue#"); 
			nullValue2CodeMap.put(NullValueId.NO_NODE, 0);
			nullValue2CodeMap.put(NullValueId.ROOT_NODE, 1);
			nullValue2CodeMap.put(NullValueId.NO_VALUE, 2); 
			symbol2CodeMap.put("#null#", 0);
			symbol2CodeMap.put("#rootnode#", 1);
			symbol2CodeMap.put("#novalue#", 2);
			code2SymbolMap.put(0, "#null#");
			code2SymbolMap.put(1, "#rootnode#");
			code2SymbolMap.put(1, "#novalue#");
			setNextCode(3);
		}
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		
		return sb.toString();
	}
}
