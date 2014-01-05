package org.maltparser.parser.history.action;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.history.container.TableContainer;
import org.maltparser.parser.history.container.TableContainer.RelationToNextDecision;
import org.maltparser.parser.history.kbest.KBestList;
/**
*
* @author Johan Hall
* @since 1.1
**/
public interface SingleDecision extends GuideDecision {
	public void addDecision(int code) throws MaltChainedException;
	public void addDecision(String symbol) throws MaltChainedException;
	public int getDecisionCode() throws MaltChainedException;
	public String getDecisionSymbol() throws MaltChainedException;
	public int getDecisionCode(String symbol) throws MaltChainedException;
	public KBestList getKBestList() throws MaltChainedException;
	public boolean updateFromKBestList() throws MaltChainedException;
	public boolean continueWithNextDecision() throws MaltChainedException;
	public TableContainer getTableContainer();
	public RelationToNextDecision getRelationToNextDecision();
}
