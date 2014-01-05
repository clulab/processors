package org.maltparser.parser;

import java.util.Set;
import java.util.regex.Pattern;

import org.maltparser.core.config.ConfigurationDir;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowChartInstance;
import org.maltparser.core.flow.item.ChartItem;
import org.maltparser.core.flow.spec.ChartItemSpecification;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.io.dataformat.DataFormatManager;
import org.maltparser.core.io.dataformat.DataFormatSpecification.DataStructure;
import org.maltparser.core.io.dataformat.DataFormatSpecification.Dependency;
import org.maltparser.core.options.OptionManager;
import org.maltparser.core.syntaxgraph.DependencyStructure;

/**
 * @author Johan Hall
 *
 */
public class SingleMaltChartItem extends ChartItem {
	private SingleMalt singleMalt;
	private String idName;
	private String targetName;
	private String sourceName;
	private String modeName;
	private String taskName;
	private DependencyStructure cachedSourceGraph = null;
	private DependencyStructure cachedTargetGraph = null;

	
	
	public void initialize(FlowChartInstance flowChartinstance, ChartItemSpecification chartItemSpecification) throws MaltChainedException {
		super.initialize(flowChartinstance, chartItemSpecification);
		
		for (String key : chartItemSpecification.getChartItemAttributes().keySet()) {
			if (key.equals("target")) {
				targetName = chartItemSpecification.getChartItemAttributes().get(key);
			} else if (key.equals("source")) {
				sourceName = chartItemSpecification.getChartItemAttributes().get(key);
			}  else if (key.equals("mode")) {
				modeName = chartItemSpecification.getChartItemAttributes().get(key);
			}  else if (key.equals("task")) {
				taskName = chartItemSpecification.getChartItemAttributes().get(key);
			} else if (key.equals("id")) {
				idName = chartItemSpecification.getChartItemAttributes().get(key);
			}
		}
		if (targetName == null) {
			targetName = getChartElement("singlemalt").getAttributes().get("target").getDefaultValue();
		} else if (sourceName == null) {
			sourceName = getChartElement("singlemalt").getAttributes().get("source").getDefaultValue();
		} else if (modeName == null) {
			modeName = getChartElement("singlemalt").getAttributes().get("mode").getDefaultValue();
		} else if (taskName == null) {
			taskName = getChartElement("singlemalt").getAttributes().get("task").getDefaultValue();
		} else if (idName == null) {
			idName = getChartElement("singlemalt").getAttributes().get("id").getDefaultValue();
		}
		
		singleMalt = (SingleMalt)flowChartinstance.getFlowChartRegistry(org.maltparser.parser.SingleMalt.class, idName);
		if (singleMalt == null) {
			singleMalt = new SingleMalt();
			flowChartinstance.addFlowChartRegistry(org.maltparser.parser.SingleMalt.class, idName, singleMalt);
			flowChartinstance.addFlowChartRegistry(org.maltparser.core.config.Configuration.class, idName, singleMalt);

		}
	}
	
	
	public int preprocess(int signal) throws MaltChainedException {
		if (taskName.equals("init")) {
			if (modeName.equals("learn") || modeName.equals("parse")) {
				OptionManager.instance().overloadOptionValue(getOptionContainerIndex(), "singlemalt", "mode", modeName);
				ConfigurationDir configDir = (ConfigurationDir)flowChartinstance.getFlowChartRegistry(org.maltparser.core.config.ConfigurationDir.class, idName);
				DataFormatManager dataFormatManager = configDir.getDataFormatManager();
				
				if (modeName.equals("learn")) {
					DataFormatInstance dataFormatInstance = null;
					if (dataFormatManager.getInputDataFormatSpec().getDataStructure() == DataStructure.PHRASE) {
						Set<Dependency> deps = dataFormatManager.getInputDataFormatSpec().getDependencies();
						String nullValueStrategy = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "singlemalt", "null_value").toString();

						for (Dependency dep : dataFormatManager.getInputDataFormatSpec().getDependencies()) {
							dataFormatInstance = dataFormatManager.getDataFormatSpec(dep.getDependentOn()).createDataFormatInstance(configDir.getSymbolTables(), nullValueStrategy);
							configDir.addDataFormatInstance(dataFormatManager.getOutputDataFormatSpec().getDataFormatName(), dataFormatInstance);
						}
						
						String decisionSettings = OptionManager.instance().getOptionValue(getOptionContainerIndex(),"guide", "decision_settings").toString().trim();
						StringBuilder newDecisionSettings = new StringBuilder();
						if (!Pattern.matches(".*A\\.HEADREL.*", decisionSettings)) {
							newDecisionSettings.append("+A.HEADREL");
						}
						if (!Pattern.matches(".*A\\.PHRASE.*", decisionSettings)) {
							newDecisionSettings.append("+A.PHRASE");
						}
						if (!Pattern.matches(".*A\\.ATTACH.*", decisionSettings)) {
							newDecisionSettings.append("+A.ATTACH");
						}
						if (newDecisionSettings.length() > 0) {
							OptionManager.instance().overloadOptionValue(getOptionContainerIndex(), "guide", "decision_settings", decisionSettings+newDecisionSettings.toString());
						}
					} else {
						dataFormatInstance = configDir.getDataFormatInstance(dataFormatManager.getInputDataFormatSpec().getDataFormatName());
					}
					singleMalt.initialize(getOptionContainerIndex(), dataFormatInstance, configDir, SingleMalt.LEARN);
				} else if (modeName.equals("parse")) {
					singleMalt.initialize(getOptionContainerIndex(), 
							configDir.getDataFormatInstance(dataFormatManager.getInputDataFormatSpec().getDataFormatName()), configDir, SingleMalt.PARSE);
				} else {
					return ChartItem.TERMINATE;
				}
			} else {
				return ChartItem.TERMINATE;
			}
		}
		return signal;
	}
	
	public int process(int signal) throws MaltChainedException {
		if (taskName.equals("process")) {
			if (cachedSourceGraph == null) {
				cachedSourceGraph = (DependencyStructure)flowChartinstance.getFlowChartRegistry(org.maltparser.core.syntaxgraph.DependencyStructure.class, sourceName);
			}
			if (cachedTargetGraph == null) {
				cachedTargetGraph = (DependencyStructure)flowChartinstance.getFlowChartRegistry(org.maltparser.core.syntaxgraph.DependencyStructure.class, targetName);
			}
			if (modeName.equals("learn")) {
				singleMalt.oracleParse(cachedSourceGraph, cachedTargetGraph);
			} else if (modeName.equals("parse")) {
				singleMalt.parse(cachedSourceGraph);
//				if (cachedSourceGraph instanceof MappablePhraseStructureGraph) {
//					System.out.println("MappablePhraseStructureGraph");
//					((MappablePhraseStructureGraph)cachedSourceGraph).getMapping().connectUnattachedSpines((MappablePhraseStructureGraph)cachedSourceGraph);
//				}	
			}
		}
		return signal;
	}
	
	public int postprocess(int signal) throws MaltChainedException {
		if (taskName.equals("train") && singleMalt.getGuide() != null) {
			singleMalt.getGuide().noMoreInstances();
		} else if (taskName.equals("train") && singleMalt.getGuide() == null) {
			singleMalt.train();
		}
		return signal;
	}

	public void terminate() throws MaltChainedException {
		if (flowChartinstance.getFlowChartRegistry(org.maltparser.parser.SingleMalt.class, idName) != null) {
			singleMalt.terminate(null);
			flowChartinstance.removeFlowChartRegistry(org.maltparser.parser.SingleMalt.class, idName);
			flowChartinstance.removeFlowChartRegistry(org.maltparser.core.config.Configuration.class, idName);
			singleMalt = null;
		} else {
			singleMalt = null;
		}
		cachedSourceGraph = null;
		cachedTargetGraph = null;
	}
	
	public SingleMalt getSingleMalt() {
		return singleMalt;
	}
	
	public void setSingleMalt(SingleMalt singleMalt) {
		this.singleMalt = singleMalt;
	}

	public String getTargetName() {
		return targetName;
	}

	public void setTargetName(String targetName) {
		this.targetName = targetName;
	}

	public String getSourceName() {
		return sourceName;
	}

	public void setSourceName(String sourceName) {
		this.sourceName = sourceName;
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
		sb.append("    singlemalt ");
		sb.append("id:");sb.append(idName);
		sb.append(' ');
		sb.append("mode:");sb.append(modeName);
		sb.append(' ');
		sb.append("task:");sb.append(taskName);
		sb.append(' ');
		sb.append("source:");sb.append(sourceName);
		sb.append(' ');
		sb.append("target:");sb.append(targetName);
		return sb.toString();
	}
}