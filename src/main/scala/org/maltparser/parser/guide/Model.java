package org.maltparser.parser.guide;


import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
/**
*
* @author Johan Hall
* @since 1.0
**/
public interface Model {
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException;
	public void noMoreInstances() throws MaltChainedException;
	public void terminate() throws MaltChainedException;
	
	public ClassifierGuide getGuide();
	public String getModelName() throws MaltChainedException;
}
