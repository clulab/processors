package org.maltparserx.parser.history;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.symbol.TableHandler;
import org.maltparserx.parser.history.action.GuideDecision;

/**
*
* @author Johan Hall
* @since 1.1
**/
public interface GuideHistory {
	public GuideDecision getEmptyGuideDecision() throws MaltChainedException; // During classification time
	public int getNumberOfDecisions();
	public TableHandler getTableHandler(String name);
	public void setKBestListClass(Class<?> kBestListClass) throws MaltChainedException;
	public Class<?> getKBestListClass();
	public int getKBestSize();
	public void setKBestSize(int kBestSize);
	public void setSeparator(String separator) throws MaltChainedException;
}
