package org.maltparser.core.flow;

import java.util.HashMap;
import java.util.LinkedHashSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.item.ChartItem;
import org.maltparser.core.flow.spec.ChartItemSpecification;
import org.maltparser.core.flow.spec.ChartSpecification;

/**
*
*
* @author Johan Hall
*/
public class FlowChartInstance {
	private FlowChartManager flowChartManager;
	private int optionContainerIndex;
	private String name;
	private ChartSpecification chartSpecification;
	private final LinkedHashSet<ChartItem> preProcessChartItems;
	private final LinkedHashSet<ChartItem> processChartItems;
	private final LinkedHashSet<ChartItem> postProcessChartItems;
	
//	private SymbolTableHandler symbolTables;
//	private DataFormatManager dataFormatManager;
//	private final HashMap<String,DataFormatInstance> dataFormatInstances;
	private final HashMap<String,Object> flowChartRegistry;
	private final HashMap<String,Object> engineRegistry;
	private final StringBuilder flowChartRegistryKey;

	
	public FlowChartInstance(int optionContainerIndex, ChartSpecification chartSpecification, FlowChartManager flowChartManager) throws MaltChainedException {
		setFlowChartManager(flowChartManager);
		setOptionContainerIndex(optionContainerIndex);
		setChartSpecification(chartSpecification);

		flowChartRegistry = new HashMap<String,Object>();
		engineRegistry = new HashMap<String,Object>();
		flowChartRegistryKey = new StringBuilder();
//		dataFormatInstances = new HashMap<String, DataFormatInstance>(3);
//		
//		String inputFormatName = OptionManager.instance().getOptionValue(0, "input", "format").toString();
//		String outputFormatName = OptionManager.instance().getOptionValue(0, "output", "format").toString();
//		SystemLogger.logger().info(inputFormatName + "\n");
//		SystemLogger.logger().info(outputFormatName + "\n");
////		featureModelFileName = configDir.copyToConfig(Util.findURLinJars(featureModelFileName));
//		dataFormatManager = new DataFormatManager(inputFormatName, outputFormatName);
//		symbolTables = new TrieSymbolTableHandler();

		preProcessChartItems = new LinkedHashSet<ChartItem>();
		for (ChartItemSpecification chartItemSpecification : chartSpecification.getPreProcessChartItemSpecifications()) {
			preProcessChartItems.add(initChartItem(chartItemSpecification));
		}
		processChartItems = new LinkedHashSet<ChartItem>();
		for (ChartItemSpecification chartItemSpecification : chartSpecification.getProcessChartItemSpecifications()) {
			processChartItems.add(initChartItem(chartItemSpecification));
		}
		
		postProcessChartItems = new LinkedHashSet<ChartItem>();
		for (ChartItemSpecification chartItemSpecification : chartSpecification.getPostProcessChartItemSpecifications()) {
			postProcessChartItems.add(initChartItem(chartItemSpecification));
		}
		

	}
	
	protected ChartItem initChartItem(ChartItemSpecification chartItemSpecification) throws MaltChainedException {
		ChartItem chartItem = null;
		try {
			chartItem = chartItemSpecification.getChartItemClass().newInstance();
			chartItem.initialize(this, chartItemSpecification);
		} catch (InstantiationException e) {
			throw new FlowException("The chart item '"+chartItemSpecification.getChartItemName()+"' could not be created. ", e);
		} catch (IllegalAccessException e) {
			throw new FlowException("The chart item '"+chartItemSpecification.getChartItemName()+"' could not be created. ", e);
		}
		return chartItem;
	}
	
//	public SymbolTableHandler getSymbolTables() {
//		return symbolTables;
//	}
//
//	public void setSymbolTables(SymbolTableHandler symbolTables) {
//		this.symbolTables = symbolTables;
//	}
//
//	public DataFormatManager getDataFormatManager() {
//		return dataFormatManager;
//	}
//
//	public void setDataFormatManager(DataFormatManager dataFormatManager) {
//		this.dataFormatManager = dataFormatManager;
//	}
	
	private void setFlowChartRegistryKey(Class<?> entryClass, String identifier) {
		flowChartRegistryKey.setLength(0);
		flowChartRegistryKey.append(identifier.toString());
		flowChartRegistryKey.append(entryClass.toString());
	}
	
	public void addFlowChartRegistry(Class<?> entryClass, String identifier, Object entry) {
		setFlowChartRegistryKey(entryClass, identifier);
		flowChartRegistry.put(flowChartRegistryKey.toString(), entry);
	}
	
	public void removeFlowChartRegistry(Class<?> entryClass, String identifier) {
		setFlowChartRegistryKey(entryClass, identifier);
		flowChartRegistry.remove(flowChartRegistryKey.toString());
	}
	
	public Object getFlowChartRegistry(Class<?> entryClass, String identifier) {
		setFlowChartRegistryKey(entryClass, identifier);
		return flowChartRegistry.get(flowChartRegistryKey.toString());
	}

	public void setEngineRegistry(String key, Object value) {
		engineRegistry.put(key, value);
	}
	
	public Object getEngineRegistry(String key) {
		return engineRegistry.get(key);
	}
	
//	public HashMap<String, DataFormatInstance> getDataFormatInstances() {
//		return dataFormatInstances;
//	}
	
	public FlowChartManager getFlowChartManager() {
		return flowChartManager;
	}

	protected void setFlowChartManager(FlowChartManager flowChartManager) {
		this.flowChartManager = flowChartManager;
	}

	public int getOptionContainerIndex() {
		return optionContainerIndex;
	}

	protected void setOptionContainerIndex(int optionContainerIndex) {
		this.optionContainerIndex = optionContainerIndex;
	}

	public ChartSpecification getChartSpecification() {
		return chartSpecification;
	}

	protected void setChartSpecification(ChartSpecification chartSpecification) {
		this.chartSpecification = chartSpecification;
	}

	public LinkedHashSet<ChartItem> getPreProcessChartItems() {
		return preProcessChartItems;
	}

	public LinkedHashSet<ChartItem> getProcessChartItems() {
		return processChartItems;
	}
	
	public LinkedHashSet<ChartItem> getPostProcessChartItems() {
		return postProcessChartItems;
	}

	public boolean hasPreProcessChartItems() {
		return !(preProcessChartItems.size() == 0);
	}
	
	public boolean hasProcessChartItems() {
		return !(processChartItems.size() == 0);
	}
	
	public boolean hasPostProcessChartItems() {
		return !(postProcessChartItems.size() == 0);
	}
	
	public int preprocess() throws MaltChainedException {
		LinkedHashSet<ChartItem> chartItems = getPreProcessChartItems();
		if (chartItems.size() == 0) {
			return ChartItem.TERMINATE;
		}
		int signal = ChartItem.CONTINUE;
		for (ChartItem chartItem : chartItems) {
			signal = chartItem.preprocess(signal);
			if (signal == ChartItem.TERMINATE) {
				return signal;
			}
		}
		return signal;
	}
	
	public int process()  throws MaltChainedException {
		LinkedHashSet<ChartItem> chartItems = getProcessChartItems();
		if (chartItems.size() == 0) {
			return ChartItem.TERMINATE;
		}
		int signal = ChartItem.CONTINUE;
		for (ChartItem chartItem : chartItems) {
			signal = chartItem.process(signal);
//			if (!more) {
//				return false;
//			}
		}
		return signal;
	}
	
	public int postprocess() throws MaltChainedException {
		LinkedHashSet<ChartItem> chartItems = getPostProcessChartItems();
		if (chartItems.size() == 0) {
			return ChartItem.TERMINATE;
		}
		int signal = ChartItem.CONTINUE;
		for (ChartItem chartItem : chartItems) {
			signal = chartItem.postprocess(signal);
			if (signal == ChartItem.TERMINATE) {
				return signal;
			}
		}
		return signal;
	}
	
	public void terminate() throws MaltChainedException {
		LinkedHashSet<ChartItem> chartItems = getPreProcessChartItems();
		for (ChartItem chartItem : chartItems) {
			chartItem.terminate();
		}
		chartItems = getProcessChartItems();
		for (ChartItem chartItem : chartItems) {
			chartItem.terminate();
		}
		chartItems = getPostProcessChartItems();
		for (ChartItem chartItem : chartItems) {
			chartItem.terminate();
		}
		flowChartRegistry.clear();
		engineRegistry.clear();
		flowChartRegistryKey.setLength(0);
//		symbolTables = null;
		
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}

	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + optionContainerIndex;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((chartSpecification == null) ? 0 : chartSpecification.hashCode());
//		result = prime * result + ((dataFormatInstances == null) ? 0 : dataFormatInstances.hashCode());
//		result = prime * result + ((dataFormatManager == null) ? 0 : dataFormatManager.hashCode());
		result = prime * result + ((flowChartRegistry == null) ? 0 : flowChartRegistry.hashCode());
		result = prime * result + ((postProcessChartItems == null) ? 0 : postProcessChartItems.hashCode());
		result = prime * result + ((preProcessChartItems == null) ? 0 : preProcessChartItems.hashCode());
		result = prime * result + ((processChartItems == null) ? 0 : processChartItems.hashCode());
//		result = prime * result + ((symbolTables == null) ? 0 : symbolTables.hashCode());
		return result;
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		FlowChartInstance other = (FlowChartInstance) obj;
		if (optionContainerIndex != other.optionContainerIndex)
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (chartSpecification == null) {
			if (other.chartSpecification != null)
				return false;
		} else if (!chartSpecification.equals(other.chartSpecification))
			return false;
//		if (dataFormatInstances == null) {
//			if (other.dataFormatInstances != null)
//				return false;
//		} else if (!dataFormatInstances.equals(other.dataFormatInstances))
//			return false;
//		if (dataFormatManager == null) {
//			if (other.dataFormatManager != null)
//				return false;
//		} else if (!dataFormatManager.equals(other.dataFormatManager))
//			return false;
		if (flowChartRegistry == null) {
			if (other.flowChartRegistry != null)
				return false;
		} else if (!flowChartRegistry.equals(other.flowChartRegistry))
			return false;
		if (postProcessChartItems == null) {
			if (other.postProcessChartItems != null)
				return false;
		} else if (!postProcessChartItems.equals(other.postProcessChartItems))
			return false;
		if (preProcessChartItems == null) {
			if (other.preProcessChartItems != null)
				return false;
		} else if (!preProcessChartItems.equals(other.preProcessChartItems))
			return false;
		if (processChartItems == null) {
			if (other.processChartItems != null)
				return false;
		} else if (!processChartItems.equals(other.processChartItems))
			return false;
//		if (symbolTables == null) {
//			if (other.symbolTables != null)
//				return false;
//		} else if (!symbolTables.equals(other.symbolTables))
//			return false;
		return true;
	}

	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(name);sb.append('\n');
		if (preProcessChartItems.size() > 0) {
			sb.append("  preprocess:");sb.append('\n');
			for (ChartItem key : preProcessChartItems) {
				sb.append(key);
				sb.append('\n');
			}
		}
		if (processChartItems.size() > 0) {
			sb.append("  process:");sb.append('\n');
			for (ChartItem key : processChartItems) {
				sb.append(key);
				sb.append('\n');
			}
		}
		if (postProcessChartItems.size() > 0) {
			sb.append("  postprocess:");sb.append('\n');
			for (ChartItem key : postProcessChartItems) {
				sb.append(key);
				sb.append('\n');
			}
		}

		return sb.toString();
	}
}
