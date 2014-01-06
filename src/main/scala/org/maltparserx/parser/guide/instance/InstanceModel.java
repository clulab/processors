package org.maltparserx.parser.guide.instance;


import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.FeatureVector;
import org.maltparserx.parser.guide.Model;
import org.maltparserx.parser.history.action.SingleDecision;

public interface InstanceModel extends Model {
	public void addInstance(SingleDecision decision) throws MaltChainedException;
	public boolean predict(SingleDecision decision) throws MaltChainedException;
	public FeatureVector predictExtract(SingleDecision decision) throws MaltChainedException;
	public FeatureVector extract() throws MaltChainedException;
	public void train() throws MaltChainedException;
	public void increaseFrequency();
	public void decreaseFrequency();
}
