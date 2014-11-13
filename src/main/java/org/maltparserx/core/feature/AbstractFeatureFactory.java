package org.maltparserx.core.feature;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.function.Function;

public interface AbstractFeatureFactory {
	public Function makeFunction(String subFunctionName) throws MaltChainedException;
}
