package org.maltparserx;

import java.util.SortedMap;
import java.util.TreeMap;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.flow.FlowChartInstance;
import org.maltparserx.core.flow.FlowChartManager;
import org.maltparserx.core.flow.item.ChartItem;
import org.maltparserx.core.helper.SystemLogger;
import org.maltparserx.core.helper.Util;
import org.maltparserx.core.options.OptionManager;
import org.maltparserx.core.plugin.PluginLoader;


public class Engine  {
	private final long startTime;
	private final FlowChartManager flowChartManager;
	private final SortedMap<Integer,FlowChartInstance> flowChartInstances;
	
	public Engine() throws MaltChainedException {
		startTime = System.currentTimeMillis();
		flowChartManager = new FlowChartManager();
		flowChartManager.getFlowChartSystem().load(getClass().getResource("/appdata/flow/flowchartsystem.xml"));
		flowChartManager.getFlowChartSystem().load(PluginLoader.instance());
		flowChartManager.load(getClass().getResource("/appdata/flow/flowcharts.xml"));
		flowChartManager.load(PluginLoader.instance());
		flowChartInstances = new TreeMap<Integer,FlowChartInstance>();
	}

	public FlowChartInstance initialize(int optionContainerIndex) throws MaltChainedException {
		String flowChartName = null;
		if (OptionManager.instance().getOptionValueNoDefault(optionContainerIndex, "config", "flowchart") != null) {
			flowChartName = OptionManager.instance().getOptionValue(optionContainerIndex, "config", "flowchart").toString();
		}
		if (flowChartName == null) {
			if (OptionManager.instance().getOptionValueNoDefault(optionContainerIndex, "singlemalt", "mode") != null) {
				// This fix maps --singlemalt-mode option to --config-flowchart option because of historical reasons (version 1.0-1.1)
				flowChartName = OptionManager.instance().getOptionValue(optionContainerIndex, "singlemalt", "mode").toString();
				OptionManager.instance().overloadOptionValue(optionContainerIndex, "config", "flowchart", flowChartName);
			} else {
				flowChartName = OptionManager.instance().getOptionValue(optionContainerIndex, "config", "flowchart").toString();
			}
		}
		FlowChartInstance flowChartInstance = flowChartManager.initialize(optionContainerIndex, flowChartName);
		flowChartInstances.put(optionContainerIndex, flowChartInstance);
		return flowChartInstance;
	}
	
	public void process(int optionContainerIndex) throws MaltChainedException {
		FlowChartInstance flowChartInstance = flowChartInstances.get(optionContainerIndex);
		if (flowChartInstance.hasPreProcessChartItems()) {
			flowChartInstance.preprocess();
		}
		if (flowChartInstance.hasProcessChartItems()) {
			int signal = ChartItem.CONTINUE;
			int tic = 0;
			int sentenceCounter = 0;
			int nIteration = 1;
			flowChartInstance.setEngineRegistry("iterations", nIteration);
			System.gc();
			while (signal != ChartItem.TERMINATE) {
				signal = flowChartInstance.process();
				if (signal == ChartItem.CONTINUE) {
					sentenceCounter++;
				} else if (signal == ChartItem.NEWITERATION) {
					SystemLogger.logger().info("\n=== END ITERATION "+nIteration+" ===\n");
					nIteration++;
					flowChartInstance.setEngineRegistry("iterations", nIteration);
				}
				if (sentenceCounter < 101 && sentenceCounter == 1 || sentenceCounter == 10 || sentenceCounter == 100) {
					Util.startTicer(SystemLogger.logger(), startTime, 10, sentenceCounter);
				} 
				if (sentenceCounter%100 == 0) {
					tic = Util.simpleTicer(SystemLogger.logger(), startTime, 10, tic, sentenceCounter);
				}
			}
			Util.endTicer(SystemLogger.logger(), startTime, 10, tic, sentenceCounter);
		}
		if (flowChartInstance.hasPostProcessChartItems()) {
			flowChartInstance.postprocess();
		}
	}
	
	public void terminate(int optionContainerIndex) throws MaltChainedException {
		flowChartInstances.get(optionContainerIndex).terminate();
	}
}
