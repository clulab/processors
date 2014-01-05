package org.maltparser.ml;

import java.io.BufferedWriter;
import java.util.ArrayList;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.parser.history.action.SingleDecision;


public interface LearningMethod {
	public static final int BATCH = 0;
	public static final int CLASSIFY = 1;
	public void addInstance(SingleDecision decision, FeatureVector featureVector) throws MaltChainedException;
	public void finalizeSentence(DependencyStructure dependencyGraph)  throws MaltChainedException;
	public void noMoreInstances() throws MaltChainedException;
	public void train(FeatureVector featureVector) throws MaltChainedException;
	public void moveAllInstances(LearningMethod method, FeatureFunction divideFeature, ArrayList<Integer> divideFeatureIndexVector) throws MaltChainedException;
	public void terminate() throws MaltChainedException;
	public boolean predict(FeatureVector features, SingleDecision decision) throws MaltChainedException;
	public BufferedWriter getInstanceWriter();
	public void increaseNumberOfInstances();
	public void decreaseNumberOfInstances();
}
