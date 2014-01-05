package org.maltparser.parser;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.parser.guide.OracleGuide;
/**
 * @author Johan Hall
 *
 */
public abstract class Trainer extends Algorithm {
	/**
	 * Creates a parser trainer
	 * 
	 * @param manager a reference to the single malt configuration
	 * @throws MaltChainedException
	 */
	public Trainer(DependencyParserConfig manager) throws MaltChainedException {
		super(manager);
	}
	
	/**
	 * Trains a parser using the gold-standard dependency graph and returns a parsed dependency graph
	 * 
	 * @param goldDependencyGraph a old-standard dependency graph
	 * @param parseDependencyGraph a empty dependency graph
	 * @return a parsed dependency graph
	 * @throws MaltChainedException
	 */
	public abstract DependencyStructure parse(DependencyStructure goldDependencyGraph, DependencyStructure parseDependencyGraph) throws MaltChainedException;
	/**
	 * Returns the oracle guide.
	 * 
	 * @return the oracle guide.
	 */
	public abstract OracleGuide getOracleGuide();
	public abstract void train() throws MaltChainedException;
	
}
