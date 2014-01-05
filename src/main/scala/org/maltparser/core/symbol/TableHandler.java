package org.maltparser.core.symbol;

import org.maltparser.core.exception.MaltChainedException;

public interface TableHandler {
	public Table getSymbolTable(String tableName) throws MaltChainedException;
	public Table addSymbolTable(String tableName) throws MaltChainedException;
}
