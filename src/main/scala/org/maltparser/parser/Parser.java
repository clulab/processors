package org.maltparser.parser;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
/**
 * @author Johan Hall
 *
 */
public abstract class Parser extends Algorithm {
	
	/**
	 * Creates a parser
	 * 
	 * @param manager a reference to the single malt configuration
	 * @throws MaltChainedException
	 */
	public Parser(DependencyParserConfig manager) throws MaltChainedException {
		super(manager);
	}
	/**
	 * Parses the empty dependency graph
	 * 
	 * @param parseDependencyGraph a dependency graph
	 * @return a parsed dependency graph
	 * @throws MaltChainedException
	 */
	public abstract DependencyStructure parse(DependencyStructure parseDependencyGraph) throws MaltChainedException;
}
