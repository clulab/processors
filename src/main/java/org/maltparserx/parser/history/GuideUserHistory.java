package org.maltparserx.parser.history;

import java.util.ArrayList;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.parser.history.action.GuideUserAction;
import org.maltparserx.parser.history.container.ActionContainer;
/**
*
* @author Johan Hall
* @since 1.1
**/
public interface GuideUserHistory {
	public GuideUserAction getEmptyGuideUserAction() throws MaltChainedException; 
	public ArrayList<ActionContainer> getActionContainers();
	public ActionContainer[] getActionContainerArray();
	public int getNumberOfDecisions();
	public void clear() throws MaltChainedException; 
}
