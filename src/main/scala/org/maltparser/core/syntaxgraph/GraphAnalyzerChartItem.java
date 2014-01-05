package org.maltparser.core.syntaxgraph;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;

import org.maltparser.core.config.ConfigurationDir;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowChartInstance;
import org.maltparser.core.flow.item.ChartItem;
import org.maltparser.core.flow.spec.ChartItemSpecification;
import org.maltparser.core.io.dataformat.DataFormatException;
import org.maltparser.core.options.OptionManager;
import org.maltparser.core.syntaxgraph.node.DependencyNode;

public class GraphAnalyzerChartItem extends ChartItem {
	private String idName;
	private String sourceName;
	private String task;
	private ConfigurationDir configDir;
	private DependencyStructure cachedSource = null;
	private BufferedWriter writer;
	private boolean closeStream = true;
	private int graphCounter = 1;
	
	public void initialize(FlowChartInstance flowChartinstance, ChartItemSpecification chartItemSpecification) throws MaltChainedException {
		super.initialize(flowChartinstance, chartItemSpecification);
		for (String key : chartItemSpecification.getChartItemAttributes().keySet()) {
			if (key.equals("id")) {
				idName = chartItemSpecification.getChartItemAttributes().get(key);
			} else if (key.equals("source")) {
				sourceName = chartItemSpecification.getChartItemAttributes().get(key);
			}
		}
		if (idName == null) {
			idName = getChartElement("analyzer").getAttributes().get("id").getDefaultValue();
		} else if (sourceName == null) {
			sourceName = getChartElement("analyzer").getAttributes().get("source").getDefaultValue();
		}
		task = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "analyzer", "task").toString();
		configDir = (ConfigurationDir)flowChartinstance.getFlowChartRegistry(org.maltparser.core.config.ConfigurationDir.class, idName);
		open(task+".dat",OptionManager.instance().getOptionValue(getOptionContainerIndex(), "input", "charset").toString());
	}

	public int preprocess(int signal) throws MaltChainedException {
		return signal;
	}
	
	public int process(int signal) throws MaltChainedException {
		if (task.equals("projectivity")) {
			if (cachedSource == null) {
				cachedSource = (DependencyStructure)flowChartinstance.getFlowChartRegistry(org.maltparser.core.syntaxgraph.DependencyStructure.class, sourceName);
			}
			try {
				writer.append("graph # ");
				writer.append(Integer.toString(graphCounter));
				writer.append('\n');
				for (int index : cachedSource.getTokenIndices()) {
					DependencyNode node = cachedSource.getDependencyNode(index);
					
					writer.append(Integer.toString(node.getIndex()));
					writer.append('\t');
					writer.append(Integer.toString(node.getHead().getIndex()));
					writer.append('\t');
					writer.append('#');
					writer.append('\t');
					if (node.isProjective()) {
						writer.append("@P");
					} else {
						writer.append("@N");
					}
					writer.append('\n');
				}
				writer.append('\n');
			} catch (IOException e) {
				throw new MaltChainedException("", e);
			}
			graphCounter++;
		}
		return signal;
	}
	
	public int postprocess(int signal) throws MaltChainedException {
		return signal;	
	}

	
	public void terminate() throws MaltChainedException {
		cachedSource = null;
		close();
	}
	
	private void open(String fileName, String charsetName) throws MaltChainedException {
		try {
			open(new OutputStreamWriter(new FileOutputStream(fileName),charsetName));
		} catch (FileNotFoundException e) {
			throw new DataFormatException("The output file '"+fileName+"' cannot be found.", e);
		} catch (UnsupportedEncodingException e) {
			throw new DataFormatException("The character encoding set '"+charsetName+"' isn't supported.", e);
		}	
	}
	
	private void open(OutputStreamWriter osw) throws MaltChainedException {
		setWriter(new BufferedWriter(osw));
	}

	private void setWriter(BufferedWriter writer) throws MaltChainedException  {
		close();
		this.writer = writer;
	}
	
	private void close() throws MaltChainedException {
		try {
			if (writer != null) {
				writer.flush();
				if (closeStream) {
					writer.close();
				}
				writer = null;
			}
		}   catch (IOException e) {
			throw new DataFormatException("Could not close the output file. ", e);
		} 

	}
}
