package org.maltparserx.parser.history;


import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.parser.history.action.GuideUserAction;

/**
 * 
 * @author Johan Hall
*/
public abstract class HistoryStructure {
	public HistoryStructure() {}

	public abstract HistoryNode getNewHistoryNode(HistoryNode previousNode, GuideUserAction action) throws MaltChainedException;
	public abstract void clear() throws MaltChainedException;
	public abstract void toFile() throws MaltChainedException;
	public abstract void close() throws MaltChainedException;
}
