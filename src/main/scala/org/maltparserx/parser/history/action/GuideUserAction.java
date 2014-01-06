package org.maltparserx.parser.history.action;

import java.util.ArrayList;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.parser.history.GuideUserHistory;
import org.maltparserx.parser.history.container.ActionContainer;
import org.maltparserx.parser.history.kbest.ScoredKBestList;
/**
*
* @author Johan Hall
* @since 1.1
**/
public interface GuideUserAction {
	public void addAction(ArrayList<ActionContainer> actionContainers) throws MaltChainedException;
	public void addAction(ActionContainer[] actionContainers) throws MaltChainedException;
	public void getAction(ArrayList<ActionContainer> actionContainers) throws MaltChainedException;
	public void getAction(ActionContainer[] actionContainers) throws MaltChainedException;
	public void getKBestLists(ArrayList<ScoredKBestList> kbestListContainers) throws MaltChainedException;
	public void getKBestLists(ScoredKBestList[] kbestListContainers) throws MaltChainedException;
	public int numberOfActions();
	public GuideUserHistory getGuideUserHistory();
}
