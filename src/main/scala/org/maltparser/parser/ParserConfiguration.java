package org.maltparser.parser;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.parser.history.HistoryNode;
/**
 * @author Johan Hall
 *
 */
public abstract class ParserConfiguration {
	protected HistoryNode historyNode;

	
	/**
	 * Creates a parser configuration
	 */
	public ParserConfiguration() {
		setHistoryNode(null);
	}

	public HistoryNode getHistoryNode() {
		return historyNode;
	}

	public void setHistoryNode(HistoryNode historyNode) {
		this.historyNode = historyNode;
	}
	
	/**
	 * Sets the dependency structure
	 * 
	 * @param dependencyStructure a dependency structure
	 * @throws MaltChainedException
	 */
	public abstract void setDependencyGraph(DependencyStructure dependencyStructure) throws MaltChainedException;
	/**
	 * Returns true if the parser configuration is in a terminal state, otherwise false.
	 * 
	 * @return true if the parser configuration is in a terminal state, otherwise false.
	 * @throws MaltChainedException
	 */
	public abstract boolean isTerminalState() throws MaltChainedException;
	/**
	 * Returns the dependency structure
	 * 
	 * @return the dependency structure
	 */
	public abstract DependencyStructure getDependencyGraph();
	/**
	 * Clears the parser configuration
	 * 
	 * @throws MaltChainedException
	 */
	public abstract void clear() throws MaltChainedException;
	/**
	 * Initialize the parser configuration with the same state as the parameter config
	 * 
	 * @param config a parser configuration
	 * @throws MaltChainedException
	 */
	public abstract void initialize(ParserConfiguration config) throws MaltChainedException;
}
