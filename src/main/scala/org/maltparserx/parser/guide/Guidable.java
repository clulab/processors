package org.maltparserx.parser.guide;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.parser.history.action.GuideUserAction;
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
