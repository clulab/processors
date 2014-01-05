package org.maltparser.parser.history.action;

import org.maltparser.core.exception.MaltChainedException;

/**
*
* @author Johan Hall
* @since 1.1
**/
public interface MultipleDecision extends GuideDecision {
	public SingleDecision getSingleDecision(int decisionIndex) throws MaltChainedException;
}
