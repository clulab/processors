package org.maltparser.core.syntaxgraph.reader;

import java.io.File;

import org.maltparser.core.config.ConfigurationDir;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowChartInstance;
import org.maltparser.core.flow.item.ChartItem;
import org.maltparser.core.flow.spec.ChartItemSpecification;
import org.maltparser.core.helper.URLFinder;
import org.maltparser.core.io.dataformat.DataFormatException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.io.dataformat.DataFormatManager;
import org.maltparser.core.options.OptionManager;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.syntaxgraph.TokenStructure;

public class ReadChartItem extends ChartItem {
	private String idName;
	private String inputFormatName;
	private String inputFileName;
	private String inputCharSet;
	private String readerOptions;
	private int iterations;
	private Class<? extends SyntaxGraphReader> graphReaderClass;
	
	private String nullValueStrategy;
	
	private SyntaxGraphReader reader;
	private String targetName;
	private String optiongroupName;
	private DataFormatInstance inputDataFormatInstance;
	private TokenStructure cachedGraph = null;
	
	public ReadChartItem() { super(); }

	public void initialize(FlowChartInstance flowChartinstance, ChartItemSpecification chartItemSpecification) throws MaltChainedException {
		super.initialize(flowChartinstance, chartItemSpecification);
		
		for (String key : chartItemSpecification.getChartItemAttributes().keySet()) {
			if (key.equals("id")) {
				idName = chartItemSpecification.getChartItemAttributes().get(key);
			} else if (key.equals("target")) {
				targetName = chartItemSpecification.getChartItemAttributes().get(key);
			} else if (key.equals("optiongroup")) {
				optiongroupName = chartItemSpecification.getChartItemAttributes().get(key);
			}
		}
		
		if (idName == null) {
			idName = getChartElement("read").getAttributes().get("id").getDefaultValue();
		} else if (targetName == null) {
			targetName = getChartElement("read").getAttributes().get("target").getDefaultValue();
		} else if (optiongroupName == null) {
			optiongroupName = getChartElement("read").getAttributes().get("optiongroup").getDefaultValue();
		}
		
		setInputFormatName(OptionManager.instance().getOptionValue(getOptionContainerIndex(), optiongroupName, "format").toString());
		setInputFileName(OptionManager.instance().getOptionValue(getOptionContainerIndex(), optiongroupName, "infile").toString());
		setInputCharSet(OptionManager.instance().getOptionValue(getOptionContainerIndex(), optiongroupName, "charset").toString());
		setReaderOptions(OptionManager.instance().getOptionValue(getOptionContainerIndex(), optiongroupName, "reader_options").toString());
		if (OptionManager.instance().getOptionValue(getOptionContainerIndex(), optiongroupName, "iterations") != null) {
			setIterations((Integer)OptionManager.instance().getOptionValue(getOptionContainerIndex(), optiongroupName, "iterations"));
		} else {
			setIterations(1);
		}
		setSyntaxGraphReaderClass((Class<?>)OptionManager.instance().getOptionValue(getOptionContainerIndex(), optiongroupName, "reader"));

		setNullValueStrategy(OptionManager.instance().getOptionValue(getOptionContainerIndex(), "singlemalt", "null_value").toString());
		
		initInput(getNullValueStrategy());
		initReader(getSyntaxGraphReaderClass(), getInputFileName(), getInputCharSet(), getReaderOptions(), iterations);
	}
	
	public int preprocess(int signal) throws MaltChainedException {
		return signal;
	}
	
	public int process(int signal) throws MaltChainedException {
		if (cachedGraph == null) {
			cachedGraph = (TokenStructure)flowChartinstance.getFlowChartRegistry(org.maltparser.core.syntaxgraph.TokenStructure.class, targetName);
		}
		int prevIterationCounter = reader.getIterationCounter();
		boolean moreInput = reader.readSentence(cachedGraph);
		if (!moreInput) {
			return ChartItem.TERMINATE;
		} else if (prevIterationCounter < reader.getIterationCounter()) {
			return ChartItem.NEWITERATION;
		}
		return ChartItem.CONTINUE;
//		return continueNextSentence && moreInput;
	}
	
	public int postprocess(int signal) throws MaltChainedException {
		return signal;
	}
	
	public void terminate() throws MaltChainedException {
		if (reader != null) {
			reader.close();
			reader = null;
		}
		cachedGraph = null;
		inputDataFormatInstance = null;
	}
	
	public String getInputFormatName() {
		if (inputFormatName == null) {
			return "/appdata/dataformat/conllx.xml";
		}
		return inputFormatName;
	}

	public void setInputFormatName(String inputFormatName) {
		this.inputFormatName = inputFormatName;
	}

	public String getInputFileName() {
		if (inputFileName == null) {
			return "/dev/stdin";
		}
		return inputFileName;
	}

	public void setInputFileName(String inputFileName) {
		this.inputFileName = inputFileName;
	}

	public String getInputCharSet() {
		if (inputCharSet == null) {
			return "UTF-8";
		}
		return inputCharSet;
	}

	public void setInputCharSet(String inputCharSet) {
		this.inputCharSet = inputCharSet;
	}

	public String getReaderOptions() {
		if (readerOptions == null) {
			return "";
		}
		return readerOptions;
	}

	public void setReaderOptions(String readerOptions) {
		this.readerOptions = readerOptions;
	}

	
	public int getIterations() {
		return iterations;
	}

	public void setIterations(int iterations) {
		this.iterations = iterations;
	}

	public Class<? extends SyntaxGraphReader> getSyntaxGraphReaderClass() {
		return graphReaderClass;
	}

	public void setSyntaxGraphReaderClass(Class<?> graphReaderClass) throws MaltChainedException {
		try {
			if (graphReaderClass != null) {
				this.graphReaderClass = graphReaderClass.asSubclass(org.maltparser.core.syntaxgraph.reader.SyntaxGraphReader.class);
			}
		} catch (ClassCastException e) {
			throw new DataFormatException("The class '"+graphReaderClass.getName()+"' is not a subclass of '"+org.maltparser.core.syntaxgraph.reader.SyntaxGraphReader.class.getName()+"'. ", e);
		}
	}
	
	public String getNullValueStrategy() {
		if (nullValueStrategy == null) {
			return "one";
		}
		return nullValueStrategy;
	}

	public void setNullValueStrategy(String nullValueStrategy) {
		this.nullValueStrategy = nullValueStrategy;
	}

	public String getTargetName() {
		return targetName;
	}

	public void setTargetName(String targetName) {
		this.targetName = targetName;
	}

	public SyntaxGraphReader getReader() {
		return reader;
	}

	public DataFormatInstance getInputDataFormatInstance() {
		return inputDataFormatInstance;
	}

	public void initInput(String nullValueStategy) throws MaltChainedException {
		ConfigurationDir configDir = (ConfigurationDir)flowChartinstance.getFlowChartRegistry(org.maltparser.core.config.ConfigurationDir.class, idName);
		DataFormatManager dataFormatManager = configDir.getDataFormatManager();
		SymbolTableHandler symbolTables = configDir.getSymbolTables();
		inputDataFormatInstance = dataFormatManager.getInputDataFormatSpec().createDataFormatInstance(symbolTables, nullValueStategy);
		configDir.addDataFormatInstance(dataFormatManager.getInputDataFormatSpec().getDataFormatName(), inputDataFormatInstance);

	}
	
	public void initReader(Class<? extends SyntaxGraphReader> syntaxGraphReader, String inputFile, String inputCharSet, String readerOptions, int iterations) throws MaltChainedException {
		try {
			final URLFinder f = new URLFinder();
			reader = syntaxGraphReader.newInstance();
			if (inputFile == null || inputFile.length() == 0 || inputFile.equals("/dev/stdin")) {
				reader.open(System.in, inputCharSet);
			} else if (new File(inputFile).exists()) {
				reader.setNIterations(iterations);
				reader.open(inputFile, inputCharSet);
			} else {
				reader.setNIterations(iterations);
				reader.open(f.findURL(inputFile), inputCharSet);
			}
			reader.setDataFormatInstance(inputDataFormatInstance); 
			reader.setOptions(readerOptions);
		} catch (InstantiationException e) {
			throw new DataFormatException("The data reader '"+syntaxGraphReader.getName()+"' cannot be initialized. ", e);
		} catch (IllegalAccessException e) {
			throw new DataFormatException("The data reader '"+syntaxGraphReader.getName()+"' cannot be initialized. ", e);
		} 	
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return obj.toString().equals(this.toString());
	}
	
	public int hashCode() {
		return 217 + (null == toString() ? 0 : toString().hashCode());
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("    read ");
		sb.append("id:");sb.append(idName);
		sb.append(' ');
		sb.append("target:");
		sb.append(targetName);
		sb.append(' ');
		sb.append("optiongroup:");
		sb.append(optiongroupName);
		return sb.toString();
	}
}
