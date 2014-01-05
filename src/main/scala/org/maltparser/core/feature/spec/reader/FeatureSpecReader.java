package org.maltparser.core.feature.spec.reader;

import java.net.URL;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.spec.SpecificationModels;
/**
*
*
* @author Johan Hall
*/
public interface FeatureSpecReader {
	public void load(URL specModelURL, SpecificationModels featureSpecModels) throws MaltChainedException;
}
