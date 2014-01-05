package org.maltparser.parser.history.action;

import org.maltparser.parser.history.GuideHistory;

/**
*
* @author Johan Hall
* @since 1.1
**/
public interface GuideDecision extends ActionDecision {
	public GuideHistory getGuideHistory();
	public int numberOfDecisions();
}
