package org.maltparserx.parser.guide;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.FeatureModelManager;
import org.maltparserx.core.feature.FeatureVector;
import org.maltparserx.parser.history.GuideHistory;
import org.maltparserx.parser.history.action.GuideDecision;

public interface ClassifierGuide extends Guide {
	public enum GuideMode { BATCH, CLASSIFY}
	
	public void addInstance(GuideDecision decision) throws MaltChainedException;
	public void noMoreInstances() throws MaltChainedException;
	public void predict(GuideDecision decision) throws MaltChainedException;
	public FeatureVector predictExtract(GuideDecision decision) throws MaltChainedException;
	public FeatureVector extract() throws MaltChainedException;
	public boolean predictFromKBestList(GuideDecision decision) throws MaltChainedException;
	
	public GuideMode getGuideMode();
	public GuideHistory getHistory();
	public FeatureModelManager getFeatureModelManager();
}
