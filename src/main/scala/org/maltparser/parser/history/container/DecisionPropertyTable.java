package org.maltparser.parser.history.container;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.Table;
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
