package org.maltparser.core.feature;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.maltparser.core.config.ConfigurationDir;
import org.maltparser.core.config.ConfigurationRegistry;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.spec.SpecificationModel;
import org.maltparser.core.feature.spec.SpecificationModels;
import org.maltparser.core.feature.system.FeatureEngine;

/**
*
*
* @author Johan Hall
*/
public class FeatureModelManager {
	protected SpecificationModels specModels;
	protected FeatureEngine featureEngine;
	protected ConfigurationDir configDirectory;

	
	public FeatureModelManager(FeatureEngine engine, ConfigurationDir configDirectory) throws MaltChainedException {
		specModels = new SpecificationModels();
		setConfigDirectory(configDirectory);
		setFeatureEngine(engine);
	}
	
	private URL findURL(String specModelFileName) throws MaltChainedException {
		URL url = null;
		File specFile = configDirectory.getFile(specModelFileName);
		if (specFile.exists()) {
			try {
				url = new URL("file:///"+specFile.getAbsolutePath());
			} catch (MalformedURLException e) {
				throw new MaltChainedException("Malformed URL: "+specFile, e);
			}
		} else {
			url = configDirectory.getConfigFileEntryURL(specModelFileName);
		}
		return url;
	}
	
	public void loadSpecification(String specModelFileName) throws MaltChainedException {
		specModels.load(findURL(specModelFileName));
	}

	
	public void loadParSpecification(String specModelFileName, String markingStrategy, String coveredRoot) throws MaltChainedException {
		specModels.loadParReader(findURL(specModelFileName), markingStrategy, coveredRoot);
	}
	
	public FeatureModel getFeatureModel(String specModelURL, int specModelUrlIndex, ConfigurationRegistry registry) throws MaltChainedException {
		return new FeatureModel(specModels.getSpecificationModel(findURL(specModelURL), specModelUrlIndex), registry, featureEngine);
	}
	
	public FeatureModel getFeatureModel(String specModelURL, ConfigurationRegistry registry) throws MaltChainedException {
		return new FeatureModel(specModels.getSpecificationModel(findURL(specModelURL), 0), registry, featureEngine);
	}
	
	public FeatureModel getFeatureModel(SpecificationModel specModel, ConfigurationRegistry registry) throws MaltChainedException {
		return new FeatureModel(specModel, registry, featureEngine);
	}
	
	public SpecificationModels getSpecModels() {
		return specModels;
	}

	protected void setSpecModels(SpecificationModels specModel) {
		this.specModels = specModel;
	}
	
	public FeatureEngine getFeatureEngine() {
		return featureEngine;
	}

	public void setFeatureEngine(FeatureEngine featureEngine) {
		this.featureEngine = featureEngine;
	}

	public ConfigurationDir getConfigDirectory() {
		return configDirectory;
	}

	public void setConfigDirectory(ConfigurationDir configDirectory) {
		this.configDirectory = configDirectory;
	}

	public String toString() {
		return specModels.toString();
	}
}
