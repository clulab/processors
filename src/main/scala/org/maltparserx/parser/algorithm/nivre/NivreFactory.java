package org.maltparserx.parser.algorithm.nivre;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.function.Function;
import org.maltparserx.parser.AbstractParserFactory;
import org.maltparserx.parser.Algorithm;
import org.maltparserx.parser.DependencyParserConfig;
import org.maltparserx.parser.ParserConfiguration;
/**
 * @author Johan Hall
 *
 */
public abstract class NivreFactory implements AbstractParserFactory {
	protected Algorithm algorithm;
	protected DependencyParserConfig manager;
	
	public NivreFactory(Algorithm algorithm) {
		setAlgorithm(algorithm);
		setManager(algorithm.getManager());
	}
	
	public ParserConfiguration makeParserConfiguration() throws MaltChainedException {
		boolean allowRoot = (Boolean)manager.getOptionValue("nivre", "allow_root");
		boolean allowReduce = (Boolean)manager.getOptionValue("nivre", "allow_reduce");
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Parser configuration : Nivre with with allow_root="+allowRoot+" and allow_reduce="+allowReduce+"\n");
		}
		return new NivreConfig(manager.getSymbolTables(), allowRoot, allowReduce);
	}
	
	public Function makeFunction(String subFunctionName) throws MaltChainedException {
		return new NivreAddressFunction(subFunctionName, algorithm);
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
