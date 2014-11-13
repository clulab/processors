package org.maltparserx.parser.history.action;

import org.maltparserx.parser.history.GuideHistory;

/**
*
* @author Johan Hall
* @since 1.1
**/
public interface GuideDecision extends ActionDecision {
	public GuideHistory getGuideHistory();
	public int numberOfDecisions();
}
