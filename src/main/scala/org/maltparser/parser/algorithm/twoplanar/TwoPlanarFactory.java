package org.maltparser.parser.algorithm.twoplanar;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.Function;
import org.maltparser.parser.AbstractParserFactory;
import org.maltparser.parser.Algorithm;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.ParserConfiguration;
/**
 * @author Carlos Gomez Rodriguez
 *
 */
public abstract class TwoPlanarFactory implements AbstractParserFactory {
	protected Algorithm algorithm;
	protected DependencyParserConfig manager;
	
	public TwoPlanarFactory(Algorithm algorithm) {
		setAlgorithm(algorithm);
		setManager(algorithm.getManager());
	}
	
	public ParserConfiguration makeParserConfiguration() throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Parser configuration : Two-Planar with no_covered_roots = " + manager.getOptionValue("planar", "no_covered_roots").toString().toUpperCase() + ", " + "acyclicity = " + manager.getOptionValue("planar", "acyclicity").toString().toUpperCase() + ", planar root handling = " + manager.getOptionValue("2planar" , "planar_root_handling").toString().toUpperCase() + ", reduce on switch = " + manager.getOptionValue("2planar" , "reduceonswitch").toString().toUpperCase() + "\n");
		}
		return new TwoPlanarConfig(manager.getSymbolTables() , manager.getOptionValue("planar", "no_covered_roots").toString() , manager.getOptionValue("planar", "acyclicity").toString() , manager.getOptionValue("2planar" , "reduceonswitch").toString()  , manager.getOptionValue("multiplanar" , "planar_root_handling").toString() );
	}
	
	public Function makeFunction(String subFunctionName) throws MaltChainedException {
		return new TwoPlanarAddressFunction(subFunctionName, algorithm);
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
