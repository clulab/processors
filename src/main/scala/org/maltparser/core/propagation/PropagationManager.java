package org.maltparser.core.propagation;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.maltparser.core.config.ConfigurationDir;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.propagation.spec.PropagationSpecs;
import org.maltparser.core.propagation.spec.PropagationSpecsReader;
import org.maltparser.core.syntaxgraph.edge.Edge;

public class PropagationManager {
	private ConfigurationDir configDirectory;
	private PropagationSpecs propagationSpecs;
	private Propagations propagations;
	
	public PropagationManager(ConfigurationDir configDirectory) {
		setConfigDirectory(configDirectory);
		propagationSpecs = new PropagationSpecs();
		
	}

	private URL findURL(String propagationSpecFileName) throws MaltChainedException {
		URL url = null;
		File specFile = configDirectory.getFile(propagationSpecFileName);
		if (specFile.exists()) {
			try {
				url = new URL("file:///"+specFile.getAbsolutePath());
			} catch (MalformedURLException e) {
				throw new PropagationException("Malformed URL: "+specFile, e);
			}
		} else {
			url = configDirectory.getConfigFileEntryURL(propagationSpecFileName);
		}
		return url;
	}
	
	public void loadSpecification(String propagationSpecFileName) throws MaltChainedException {
		PropagationSpecsReader reader = new PropagationSpecsReader();
		reader.load(findURL(propagationSpecFileName), propagationSpecs);
		propagations = new Propagations(propagationSpecs, configDirectory.getInputDataFormatInstance());
	}
	
	public void propagate(Edge e) throws MaltChainedException {
		if (propagations != null && e != null) {
			propagations.propagate(e);
		}
	}
	
	public PropagationSpecs getPropagationSpecs() {
		return propagationSpecs;
	}

	public ConfigurationDir getConfigDirectory() {
		return configDirectory;
	}

	public void setConfigDirectory(ConfigurationDir configDirectory) {
		this.configDirectory = configDirectory;
	}

	public Propagations getPropagations() {
		return propagations;
	}
	
	
}
