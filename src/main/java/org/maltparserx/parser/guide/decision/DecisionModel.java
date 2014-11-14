package org.maltparserx.parser.guide.decision;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.FeatureModel;
import org.maltparserx.core.feature.FeatureVector;
import org.maltparserx.parser.guide.Model;
import org.maltparserx.parser.history.action.GuideDecision;
/**
*
* @author Johan Hall
* @since 1.1
**/
public interface DecisionModel extends Model {
	public void updateFeatureModel() throws MaltChainedException;
//	public void updateCardinality() throws MaltChainedException;
	
	public void addInstance(GuideDecision decision) throws MaltChainedException;
	public boolean predict(GuideDecision decision) throws MaltChainedException;
	public FeatureVector predictExtract(GuideDecision decision) throws MaltChainedException;
	public FeatureVector extract() throws MaltChainedException;
	public boolean predictFromKBestList(GuideDecision decision) throws MaltChainedException;
	
	public FeatureModel getFeatureModel();
	public int getDecisionIndex();
}
