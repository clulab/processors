package org.maltparser.core.syntaxgraph;

import org.maltparser.core.config.ConfigurationDir;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowChartInstance;
import org.maltparser.core.flow.FlowException;
import org.maltparser.core.flow.item.ChartItem;
import org.maltparser.core.flow.spec.ChartItemSpecification;
import org.maltparser.core.helper.HashSet;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.io.dataformat.DataFormatManager;
import org.maltparser.core.io.dataformat.DataFormatSpecification.DataStructure;
import org.maltparser.core.io.dataformat.DataFormatSpecification.Dependency;
import org.maltparser.core.options.OptionManager;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.syntaxgraph.ds2ps.LosslessMapping;
/**
*
*
* @author Johan Hall
*/
public class SyntaxGraphChartItem extends ChartItem {
	private String idName;
	private String structureName;
	private String taskName;
	private TokenStructure graph;
	
	public SyntaxGraphChartItem() { super(); }
	
	public void initialize(FlowChartInstance flowChartinstance, ChartItemSpecification chartItemSpecification) throws MaltChainedException {
		super.initialize(flowChartinstance, chartItemSpecification);
		
		for (String key : chartItemSpecification.getChartItemAttributes().keySet()) {
			if (key.equals("id")) {
				idName = chartItemSpecification.getChartItemAttributes().get(key);
			} else if (key.equals("structure")) {
				structureName = chartItemSpecification.getChartItemAttributes().get(key);
			} else if (key.equals("task")) {
				taskName = chartItemSpecification.getChartItemAttributes().get(key);
			}
		}
		if (idName == null) {
			idName = getChartElement("graph").getAttributes().get("id").getDefaultValue();
		} else if (structureName == null) {
			structureName = getChartElement("graph").getAttributes().get("structure").getDefaultValue();
		} else if (taskName == null) {
			taskName = getChartElement("graph").getAttributes().get("task").getDefaultValue();
		}
	}
	
	public int preprocess(int signal) throws MaltChainedException {
		if (taskName.equals("create")) {
			boolean phrase = false;
			boolean dependency = false;
			ConfigurationDir configDir = (ConfigurationDir)flowChartinstance.getFlowChartRegistry(org.maltparser.core.config.ConfigurationDir.class, idName);
			DataFormatInstance dataFormatInstance = null;
			DataFormatManager dataFormatManager = configDir.getDataFormatManager();
			SymbolTableHandler symbolTables = configDir.getSymbolTables();

			

			for (String key : configDir.getDataFormatInstanceKeys()) {
				DataFormatInstance dfi = configDir.getDataFormatInstance(key);
				if (dfi.getDataFormarSpec().getDataStructure() == DataStructure.PHRASE) {
					phrase = true;
				}
				if (dfi.getDataFormarSpec().getDataStructure() == DataStructure.DEPENDENCY) {
					dependency = true;
					dataFormatInstance = dfi;
				}
			}

			if (dependency == false && OptionManager.instance().getOptionValue(getOptionContainerIndex(), "config", "flowchart").toString().equals("learn")) {
				dependency = true;
				HashSet<Dependency> deps = dataFormatManager.getInputDataFormatSpec().getDependencies();
				String nullValueStategy = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "singlemalt", "null_value").toString();
				for (Dependency dep : deps) {
					dataFormatInstance = dataFormatManager.getDataFormatSpec(dep.getDependentOn()).createDataFormatInstance(symbolTables, nullValueStategy);
					configDir.addDataFormatInstance(dataFormatManager.getOutputDataFormatSpec().getDataFormatName(), dataFormatInstance);
				}
			}

			if (dependency == true && phrase == false) {
				graph = new DependencyGraph(symbolTables);
				flowChartinstance.addFlowChartRegistry(org.maltparser.core.syntaxgraph.DependencyStructure.class, structureName, graph);
			} else if (dependency == true && phrase == true) {
				graph = new MappablePhraseStructureGraph(symbolTables);
				final DataFormatInstance inFormat = configDir.getDataFormatInstance(dataFormatManager.getInputDataFormatSpec().getDataFormatName()); 
				final DataFormatInstance outFormat = configDir.getDataFormatInstance(dataFormatManager.getOutputDataFormatSpec().getDataFormatName());

				if (inFormat != null && outFormat != null) {
					LosslessMapping mapping = null;
					if (inFormat.getDataFormarSpec().getDataStructure() == DataStructure.DEPENDENCY) {
						mapping = new LosslessMapping(inFormat, outFormat);
					} else {
						mapping = new LosslessMapping(outFormat, inFormat);
					}
					if (inFormat.getDataFormarSpec().getDataStructure() == DataStructure.PHRASE) {
						mapping.setHeadRules(OptionManager.instance().getOptionValue(getOptionContainerIndex(), "graph", "head_rules").toString());
					}
					((MappablePhraseStructureGraph)graph).setMapping(mapping);
				} else {
					throw new FlowException("Couldn't determine the input and output data format. ");
				}
				flowChartinstance.addFlowChartRegistry(org.maltparser.core.syntaxgraph.DependencyStructure.class, structureName, graph);
				flowChartinstance.addFlowChartRegistry(org.maltparser.core.syntaxgraph.PhraseStructure.class, structureName, graph);
			} else if (dependency == false && phrase == true) {
				graph = new PhraseStructureGraph(symbolTables);
				flowChartinstance.addFlowChartRegistry(org.maltparser.core.syntaxgraph.PhraseStructure.class, structureName, graph);
			} else {
				graph = new Sentence(symbolTables);
			}
			
			if (dataFormatInstance != null) {
				((DependencyStructure)graph).setDefaultRootEdgeLabels(
						OptionManager.instance().getOptionValue(getOptionContainerIndex(), "graph", "root_label").toString(), 
						dataFormatInstance.getDependencyEdgeLabelSymbolTables());
			}
			flowChartinstance.addFlowChartRegistry(org.maltparser.core.syntaxgraph.TokenStructure.class, structureName, graph);
		}
		return signal;
	}
	
	public int process(int signal) throws MaltChainedException {
		return signal;
	}
	
	public int postprocess(int signal) throws MaltChainedException {
		return signal;
	}
	
	public void terminate() throws MaltChainedException {
		if (graph != null) {
			graph.clear();
			graph = null;
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
		StringBuilder sb = new StringBuilder();
		sb.append("    graph ");
		sb.append("id:");sb.append(idName);
		sb.append(' ');
		sb.append("task:");
		sb.append(taskName);
		sb.append(' ');
		sb.append("structure:");
		sb.append(structureName);
		return sb.toString();
	}
}
