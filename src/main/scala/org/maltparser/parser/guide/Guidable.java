package org.maltparser.parser.guide;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.history.action.GuideUserAction;
/**
*
* @author Johan Hall
* @since 1.1
**/
public interface Guidable {
	public void setInstance(GuideUserAction action) throws MaltChainedException;
	public void predict(GuideUserAction action) throws MaltChainedException;
	public boolean predictFromKBestList(GuideUserAction action) throws MaltChainedException;
}
