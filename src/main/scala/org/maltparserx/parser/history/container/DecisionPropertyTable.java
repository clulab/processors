package org.maltparserx.parser.history.container;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.symbol.Table;
/**
*
* @author Johan Hall
* @since 1.1
**/
public interface DecisionPropertyTable {
	public boolean continueWithNextDecision(int code) throws MaltChainedException;
	public boolean continueWithNextDecision(String symbol) throws MaltChainedException;
	public Table getTableForNextDecision(int code) throws MaltChainedException;
	public Table getTableForNextDecision(String symbol) throws MaltChainedException;
}
