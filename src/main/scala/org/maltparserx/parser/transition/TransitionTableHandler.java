package org.maltparserx.parser.transition;


import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.helper.HashMap;
import org.maltparserx.core.symbol.Table;
import org.maltparserx.core.symbol.TableHandler;
/**
*
* @author Johan Hall
* @since 1.1
**/
public class TransitionTableHandler implements TableHandler{
	private final HashMap<String, TransitionTable> transitionTables;

	public TransitionTableHandler() {
		transitionTables = new HashMap<String, TransitionTable>();
	}
	
	public Table addSymbolTable(String tableName) throws MaltChainedException {
		TransitionTable table = transitionTables.get(tableName);
		if (table == null) {
			table = new TransitionTable(tableName);
			transitionTables.put(tableName, table);
		}
		return table;
	}

	public Table getSymbolTable(String tableName) throws MaltChainedException {
		return transitionTables.get(tableName);
	}
}
