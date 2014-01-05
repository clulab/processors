package org.maltparser.parser.history;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * 
 * @author Johan Hall
*/
public interface HistoryNode {
	public HistoryNode getPreviousNode();
	public GuideUserAction getAction();
	public void setAction(GuideUserAction action);
	public void setPreviousNode(HistoryNode node);
//	public double getScore();
//	public void setScore(double score);
	public int getPosition();
	public void clear() throws MaltChainedException;
}
