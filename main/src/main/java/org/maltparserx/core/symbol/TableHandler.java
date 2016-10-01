package org.maltparserx.core.symbol;

import org.maltparserx.core.exception.MaltChainedException;

public interface TableHandler {
	public Table getSymbolTable(String tableName) throws MaltChainedException;
	public Table addSymbolTable(String tableName) throws MaltChainedException;
}
