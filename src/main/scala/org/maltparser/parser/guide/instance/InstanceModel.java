package org.maltparser.parser.guide.instance;


import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.parser.guide.Model;
import org.maltparser.parser.history.action.SingleDecision;

public interface InstanceModel extends Model {
	public void addInstance(SingleDecision decision) throws MaltChainedException;
	public boolean predict(SingleDecision decision) throws MaltChainedException;
	public FeatureVector predictExtract(SingleDecision decision) throws MaltChainedException;
	public FeatureVector extract() throws MaltChainedException;
	public void train() throws MaltChainedException;
	public void increaseFrequency();
	public void decreaseFrequency();
}
