package org.maltparser.transform.pseudo;


import org.maltparser.core.config.ConfigurationDir;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowChartInstance;
import org.maltparser.core.flow.item.ChartItem;
import org.maltparser.core.flow.spec.ChartItemSpecification;
import org.maltparser.core.helper.SystemLogger;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.options.OptionManager;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.core.syntaxgraph.TokenStructure;
/**
*
*
* @author Johan Hall
*/
public class PseudoProjChartItem extends ChartItem {
	private String idName;
	private String targetName;
	private String sourceName;
	private String taskName;
	
	private String marking_strategy;
	private String covered_root;
	private String lifting_order;
	
	private PseudoProjectivity pproj; 
	private boolean pprojActive = false;
	private TokenStructure cachedGraph = null;
	
	public PseudoProjChartItem() {}
	
	public void initialize(FlowChartInstance flowChartinstance, ChartItemSpecification chartItemSpecification) throws MaltChainedException {
		super.initialize(flowChartinstance, chartItemSpecification);
		
		for (String key : chartItemSpecification.getChartItemAttributes().keySet()) {
			if (key.equals("target")) {
				targetName = chartItemSpecification.getChartItemAttributes().get(key);
			} else if (key.equals("source")) {
				sourceName = chartItemSpecification.getChartItemAttributes().get(key);
			} else if (key.equals("id")) {
				idName = chartItemSpecification.getChartItemAttributes().get(key);
			}  else if (key.equals("task")) {
				taskName = chartItemSpecification.getChartItemAttributes().get(key);
			}
		}
		
		if (targetName == null) {
			targetName = getChartElement("pseudoproj").getAttributes().get("target").getDefaultValue();
		} else if (sourceName == null) {
			sourceName = getChartElement("pseudoproj").getAttributes().get("source").getDefaultValue();
		} else if (idName == null) {
			idName = getChartElement("pseudoproj").getAttributes().get("id").getDefaultValue();
		} else if (taskName == null) {
			taskName = getChartElement("pseudoproj").getAttributes().get("task").getDefaultValue();
		}
	
		PseudoProjectivity tmppproj = (PseudoProjectivity)flowChartinstance.getFlowChartRegistry(org.maltparser.transform.pseudo.PseudoProjectivity.class, idName);
		if (tmppproj == null) {
			pproj = new PseudoProjectivity();
			flowChartinstance.addFlowChartRegistry(org.maltparser.transform.pseudo.PseudoProjectivity.class, idName, pproj);
		} else {
			pproj = tmppproj;
		}
	}
	
	public int preprocess(int signal) throws MaltChainedException {
		if (taskName.equals("init")) {
			ConfigurationDir configDir = (ConfigurationDir)flowChartinstance.getFlowChartRegistry(org.maltparser.core.config.ConfigurationDir.class, idName);
//			SymbolTableHandler symbolTables = configDir.getSymbolTables();
			DataFormatInstance dataFormatInstance = configDir.getInputDataFormatInstance();
			marking_strategy = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "pproj", "marking_strategy").toString().trim();
			covered_root = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "pproj", "covered_root").toString().trim();
			lifting_order = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "pproj", "lifting_order").toString().trim();
			
			if (!marking_strategy.equalsIgnoreCase("none") || !covered_root.equalsIgnoreCase("none")) { 
				pproj.initialize(marking_strategy, covered_root, lifting_order, SystemLogger.logger(), dataFormatInstance);
			}
			if (!marking_strategy.equalsIgnoreCase("none") || !covered_root.equalsIgnoreCase("none")) { 
				pprojActive = true;
			}
		}
		return signal;
	}
	
	public int process(int signal) throws MaltChainedException {
		if (cachedGraph == null) {
			marking_strategy = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "pproj", "marking_strategy").toString().trim();
			covered_root = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "pproj", "covered_root").toString().trim();
			lifting_order = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "pproj", "lifting_order").toString().trim();

			cachedGraph = (TokenStructure)flowChartinstance.getFlowChartRegistry(org.maltparser.core.syntaxgraph.TokenStructure.class, sourceName);
			if (!marking_strategy.equalsIgnoreCase("none") || !covered_root.equalsIgnoreCase("none")) { 
				pprojActive = true;
			}
		}
		
		if (pprojActive && cachedGraph instanceof DependencyStructure) {
			if (taskName.equals("proj")) {
					pproj.projectivize((DependencyStructure)cachedGraph);
			} else if (taskName.equals("merge")) {
					pproj.mergeArclabels((DependencyStructure)cachedGraph);
			} else if (taskName.equals("deproj")) {
					pproj.deprojectivize((DependencyStructure)cachedGraph);
			} else if (taskName.equals("split")) {
					pproj.splitArclabels((DependencyStructure)cachedGraph);
			}
		}
		return signal;
	}
	
	public int postprocess(int signal) throws MaltChainedException {
		return signal;
	}

	
	public void terminate() throws MaltChainedException {
		pproj = null; 
		pprojActive = false;
		cachedGraph = null;
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
		sb.append("    pseudoproj ");
		sb.append("id:");sb.append(idName);
		sb.append(' ');
		sb.append("task:");sb.append(taskName);
		sb.append(' ');
		sb.append("source:");sb.append(sourceName);
		sb.append(' ');
		sb.append("target:");sb.append(targetName);
		return sb.toString();
	}
}
