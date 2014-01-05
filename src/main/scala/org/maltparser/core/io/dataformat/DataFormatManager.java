package org.maltparser.core.io.dataformat;

import java.net.URL;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.HashMap;
import org.maltparser.core.helper.URLFinder;
import org.maltparser.core.io.dataformat.DataFormatSpecification.Dependency;

public class DataFormatManager {
	private DataFormatSpecification inputDataFormatSpec;
	private DataFormatSpecification outputDataFormatSpec;
	private final HashMap<String, DataFormatSpecification> fileNameDataFormatSpecs;
	private final HashMap<String, DataFormatSpecification> nameDataFormatSpecs;
	
	public DataFormatManager(URL inputFormatUrl, URL outputFormatUrl) throws MaltChainedException {
		fileNameDataFormatSpecs = new HashMap<String, DataFormatSpecification>();
		nameDataFormatSpecs = new HashMap<String, DataFormatSpecification>();
		inputDataFormatSpec = loadDataFormat(inputFormatUrl);
		outputDataFormatSpec = loadDataFormat(outputFormatUrl);
	}

	public DataFormatSpecification loadDataFormat(URL dataFormatUrl) throws MaltChainedException {
		if (dataFormatUrl == null) {
			return null;
		}
		DataFormatSpecification dataFormat = fileNameDataFormatSpecs.get(dataFormatUrl.toString());
		if (dataFormat == null) {
			dataFormat = new DataFormatSpecification();
			dataFormat.parseDataFormatXMLfile(dataFormatUrl);
			fileNameDataFormatSpecs.put(dataFormatUrl.toString(), dataFormat);
			nameDataFormatSpecs.put(dataFormat.getDataFormatName(), dataFormat);
			final URLFinder f = new URLFinder();
			
			for (Dependency dep : dataFormat.getDependencies()) {
				loadDataFormat(f.findURLinJars(dep.getUrlString()));
			}
		}
		return dataFormat;
	}
	
	public DataFormatSpecification getInputDataFormatSpec() {
		return inputDataFormatSpec;
	}

	public DataFormatSpecification getOutputDataFormatSpec() {
		return outputDataFormatSpec;
	}
	
	public void setInputDataFormatSpec(DataFormatSpecification inputDataFormatSpec) {
		this.inputDataFormatSpec = inputDataFormatSpec;
	}

	public void setOutputDataFormatSpec(DataFormatSpecification outputDataFormatSpec) {
		this.outputDataFormatSpec = outputDataFormatSpec;
	}

	public DataFormatSpecification getDataFormatSpec(String name) {
		return nameDataFormatSpecs.get(name);
	}
}
