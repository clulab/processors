package org.maltparser.core.syntaxgraph;

import java.util.LinkedHashMap;

import org.maltparser.core.symbol.SymbolTable;
/**
*
*
* @author Johan Hall
*/
public class LabelSet extends LinkedHashMap<SymbolTable,Integer> {
	public static final long serialVersionUID = 8045567022124816378L;
	public LabelSet() {
		super();
	}
	public LabelSet(int initialCapacity) {
		super(initialCapacity);
	}
	public LabelSet(int initialCapacity, float loadFactor) {
		super(initialCapacity,loadFactor);
	}
	public LabelSet(LabelSet labelSet) {
		super(labelSet);
	}
}
