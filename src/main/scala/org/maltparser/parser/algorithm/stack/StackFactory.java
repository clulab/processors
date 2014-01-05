package org.maltparser.parser.algorithm.stack;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.Function;
import org.maltparser.parser.AbstractParserFactory;
import org.maltparser.parser.Algorithm;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.ParserConfiguration;
/**
 * @author Johan Hall
 *
 */
public abstract class StackFactory implements AbstractParserFactory {
	protected Algorithm algorithm;
	protected DependencyParserConfig manager;
	
	public StackFactory(Algorithm algorithm) {
		setAlgorithm(algorithm);
		setManager(algorithm.getManager());
	}
	
	public ParserConfiguration makeParserConfiguration() throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Parser configuration : Stack\n");
		}
		return new StackConfig(manager.getSymbolTables());
	}
	
	public Function makeFunction(String subFunctionName) throws MaltChainedException {
		return new StackAddressFunction(subFunctionName, algorithm);
	}
	
	public Algorithm getAlgorithm() {
		return algorithm;
	}

	public void setAlgorithm(Algorithm algorithm) {
		this.algorithm = algorithm;
	}
	
	public DependencyParserConfig getManager() {
		return manager;
	}

	public void setManager(DependencyParserConfig manager) {
		this.manager = manager;
	}
}
