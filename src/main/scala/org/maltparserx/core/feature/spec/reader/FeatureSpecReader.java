package org.maltparserx.core.feature.spec.reader;

import java.net.URL;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.spec.SpecificationModels;
/**
*
*
* @author Johan Hall
*/
public interface FeatureSpecReader {
	public void load(URL specModelURL, SpecificationModels featureSpecModels) throws MaltChainedException;
}
