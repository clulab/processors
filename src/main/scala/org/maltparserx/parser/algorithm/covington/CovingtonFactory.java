package org.maltparserx.parser.algorithm.covington;

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
public abstract class CovingtonFactory implements AbstractParserFactory {
	protected Algorithm algorithm;
	protected DependencyParserConfig manager;
	
	public CovingtonFactory(Algorithm algorithm) {
		setAlgorithm(algorithm);
		setManager(algorithm.getManager());
	}
	
	public ParserConfiguration makeParserConfiguration() throws MaltChainedException {
		boolean allowRoot = (Boolean)manager.getOptionValue("covington", "allow_root");
		boolean allowShift = (Boolean)manager.getOptionValue("covington", "allow_shift");
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Parser configuration : Covington with allow_root="+allowRoot+" and allow_shift="+allowShift+"\n");
		}
		CovingtonConfig config = new CovingtonConfig(manager.getSymbolTables(), allowRoot, allowShift);
		return config;
	}
	
	public Function makeFunction(String subFunctionName) throws MaltChainedException {
		return new CovingtonAddressFunction(subFunctionName, algorithm);
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
