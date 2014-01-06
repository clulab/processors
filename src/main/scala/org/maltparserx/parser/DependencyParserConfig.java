package org.maltparserx.parser;

import org.maltparserx.core.config.Configuration;
import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.propagation.PropagationManager;
import org.maltparserx.core.syntaxgraph.DependencyStructure;
import org.maltparserx.parser.guide.ClassifierGuide;
/**
 * @author Johan Hall
 *
 */
public interface DependencyParserConfig extends Configuration {
	public void parse(DependencyStructure graph) throws MaltChainedException;
	public void oracleParse(DependencyStructure goldGraph, DependencyStructure oracleGraph) throws MaltChainedException;
	public ClassifierGuide getGuide();
	public Algorithm getAlgorithm();
	public PropagationManager getPropagationManager();
}
