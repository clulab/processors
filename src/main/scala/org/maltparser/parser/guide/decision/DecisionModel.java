package org.maltparser.parser.guide.decision;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureModel;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.parser.guide.Model;
import org.maltparser.parser.history.action.GuideDecision;
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
