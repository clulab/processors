package org.maltparser.core.flow.spec;

import java.util.LinkedHashSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowChartManager;
import org.maltparser.core.flow.FlowException;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
/**
*
*
* @author Johan Hall
*/
public class ChartSpecification {
	private String name;
	private LinkedHashSet<ChartItemSpecification> preProcessChartItemSpecifications;
	private LinkedHashSet<ChartItemSpecification> processChartItemSpecifications;
	private LinkedHashSet<ChartItemSpecification> postProcessChartItemSpecifications;
	
	public ChartSpecification() {
		preProcessChartItemSpecifications = new LinkedHashSet<ChartItemSpecification>(7);
		processChartItemSpecifications = new LinkedHashSet<ChartItemSpecification>(7);
		postProcessChartItemSpecifications = new LinkedHashSet<ChartItemSpecification>(7);
	}
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	
	public LinkedHashSet<ChartItemSpecification> getPreProcessChartItemSpecifications() {
		return preProcessChartItemSpecifications;
	}

	public void addPreProcessChartItemSpecifications(ChartItemSpecification chartItemSpecification) {
		preProcessChartItemSpecifications.add(chartItemSpecification);
	}
	
	public void removePreProcessChartItemSpecifications(ChartItemSpecification chartItemSpecification) {
		preProcessChartItemSpecifications.remove(chartItemSpecification);
	}
	
	public LinkedHashSet<ChartItemSpecification> getProcessChartItemSpecifications() {
		return processChartItemSpecifications;
	}
	
	public void addProcessChartItemSpecifications(ChartItemSpecification chartItemSpecification) {
		processChartItemSpecifications.add(chartItemSpecification);
	}
	
	public void removeProcessChartItemSpecifications(ChartItemSpecification chartItemSpecification) {
		processChartItemSpecifications.remove(chartItemSpecification);
	}
	
	public LinkedHashSet<ChartItemSpecification> getPostProcessChartItemSpecifications() {
		return postProcessChartItemSpecifications;
	}
	
	public void addPostProcessChartItemSpecifications(ChartItemSpecification chartItemSpecification) {
		postProcessChartItemSpecifications.add(chartItemSpecification);
	}
	
	public void removePostProcessChartItemSpecifications(ChartItemSpecification chartItemSpecification) {
		postProcessChartItemSpecifications.remove(chartItemSpecification);
	}

	public void read(Element chartElem, FlowChartManager flowCharts) throws MaltChainedException {
		setName(chartElem.getAttribute("name"));
		NodeList flowChartProcessList = chartElem.getElementsByTagName("preprocess");
		if (flowChartProcessList.getLength() == 1) {
			readChartItems((Element)flowChartProcessList.item(0), flowCharts, preProcessChartItemSpecifications);
		} else if (flowChartProcessList.getLength() > 1) {
			throw new FlowException("The flow chart '"+getName()+"' has more than one preprocess elements. ");
		}
		
		flowChartProcessList = chartElem.getElementsByTagName("process");
		if (flowChartProcessList.getLength() == 1) {
			readChartItems((Element)flowChartProcessList.item(0), flowCharts, processChartItemSpecifications);
		} else if (flowChartProcessList.getLength() > 1) {
			throw new FlowException("The flow chart '"+getName()+"' has more than one process elements. ");
		}
		
		flowChartProcessList = chartElem.getElementsByTagName("postprocess");
		if (flowChartProcessList.getLength() == 1) {
			readChartItems((Element)flowChartProcessList.item(0), flowCharts, postProcessChartItemSpecifications);
		} else if (flowChartProcessList.getLength() > 1) {
			throw new FlowException("The flow chart '"+getName()+"' has more than one postprocess elements. ");
		}
	}
	
	private void readChartItems(Element chartElem, FlowChartManager flowCharts, LinkedHashSet<ChartItemSpecification> chartItemSpecifications) throws MaltChainedException {
		NodeList flowChartItemList = chartElem.getElementsByTagName("chartitem");
		for (int i = 0; i < flowChartItemList.getLength(); i++) {
			ChartItemSpecification chartItemSpecification = new ChartItemSpecification();
			chartItemSpecification.read((Element)flowChartItemList.item(i), flowCharts);
			chartItemSpecifications.add(chartItemSpecification);
		}
	}
	
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((postProcessChartItemSpecifications == null) ? 0 : postProcessChartItemSpecifications.hashCode());
		result = prime * result + ((preProcessChartItemSpecifications == null) ? 0 : preProcessChartItemSpecifications.hashCode());
		result = prime * result + ((processChartItemSpecifications == null) ? 0 : processChartItemSpecifications.hashCode());
		return result;
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ChartSpecification other = (ChartSpecification) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (postProcessChartItemSpecifications == null) {
			if (other.postProcessChartItemSpecifications != null)
				return false;
		} else if (!postProcessChartItemSpecifications
				.equals(other.postProcessChartItemSpecifications))
			return false;
		if (preProcessChartItemSpecifications == null) {
			if (other.preProcessChartItemSpecifications != null)
				return false;
		} else if (!preProcessChartItemSpecifications
				.equals(other.preProcessChartItemSpecifications))
			return false;
		if (processChartItemSpecifications == null) {
			if (other.processChartItemSpecifications != null)
				return false;
		} else if (!processChartItemSpecifications
				.equals(other.processChartItemSpecifications))
			return false;
		return true;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(name);sb.append('\n');
		if (preProcessChartItemSpecifications.size() > 0) {
			sb.append("  preprocess:");sb.append('\n');
			for (ChartItemSpecification key : preProcessChartItemSpecifications) {
				sb.append(key);sb.append('\n');
			}
		}
		if (processChartItemSpecifications.size() > 0) {
			sb.append("  process:");sb.append('\n');
			for (ChartItemSpecification key : processChartItemSpecifications) {
				sb.append(key);sb.append('\n');
			}
		}
		if (postProcessChartItemSpecifications.size() > 0) {
			sb.append("  postprocess:");sb.append('\n');
			for (ChartItemSpecification key : postProcessChartItemSpecifications) {
				sb.append(key);sb.append('\n');
			}
		}
		return sb.toString();
	}
}
