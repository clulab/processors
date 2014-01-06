package org.maltparserx.parser.algorithm.planar;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.function.Function;
import org.maltparserx.parser.AbstractParserFactory;
import org.maltparserx.parser.Algorithm;
import org.maltparserx.parser.DependencyParserConfig;
import org.maltparserx.parser.ParserConfiguration;
/**
 * @author Carlos Gomez Rodriguez
 *
 */
public abstract class PlanarFactory implements AbstractParserFactory {
	protected Algorithm algorithm;
	protected DependencyParserConfig manager;
	
	public PlanarFactory(Algorithm algorithm) {
		setAlgorithm(algorithm);
		setManager(algorithm.getManager());
	}
	
	public ParserConfiguration makeParserConfiguration() throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Parser configuration : Planar with no_covered_roots = " + manager.getOptionValue("planar", "no_covered_roots").toString().toUpperCase() + ", " + "acyclicity = " + manager.getOptionValue("planar", "acyclicity").toString().toUpperCase() + ", connectedness = " + manager.getOptionValue("planar", "connectedness").toString().toUpperCase() + ", planar root handling = " + manager.getOptionValue("2planar" , "planar_root_handling").toString().toUpperCase() + "\n");
		}
		return new PlanarConfig(manager.getSymbolTables() , manager.getOptionValue("planar", "no_covered_roots").toString() , manager.getOptionValue("planar", "acyclicity").toString() , manager.getOptionValue("planar", "connectedness").toString(), manager.getOptionValue("multiplanar" , "planar_root_handling").toString());
	}
	
	public Function makeFunction(String subFunctionName) throws MaltChainedException {
		return new PlanarAddressFunction(subFunctionName, algorithm);
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
