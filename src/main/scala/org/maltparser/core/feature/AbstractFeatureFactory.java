package org.maltparser.core.feature;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.function.Function;

public interface AbstractFeatureFactory {
	public Function makeFunction(String subFunctionName) throws MaltChainedException;
}
