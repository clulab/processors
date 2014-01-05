package org.maltparser.parser;

import org.maltparser.core.config.Configuration;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.propagation.PropagationManager;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.parser.guide.ClassifierGuide;
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
