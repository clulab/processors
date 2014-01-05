package org.maltparser.core.flow.spec;

import java.util.HashMap;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowChartManager;
import org.maltparser.core.flow.item.ChartItem;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
/**
*
*
* @author Johan Hall
*/
public class ChartItemSpecification {
	private String chartItemName;
	private Class<? extends ChartItem> chartItemClass;
	private HashMap<String,String> attributes;
	
	public ChartItemSpecification() {
		this(null,null);
	}

	public ChartItemSpecification(String chartItemName, Class<? extends ChartItem> chartItemClass) {
		setChartItemName(chartItemName);
		setChartItemClass(chartItemClass);
		attributes = new HashMap<String,String>(3);
	}
	
	public String getChartItemName() {
		return chartItemName;
	}

	public void setChartItemName(String chartItemName) {
		this.chartItemName = chartItemName;
	}

	public Class<? extends ChartItem> getChartItemClass() {
		return chartItemClass;
	}

	public void setChartItemClass(Class<? extends ChartItem> chartItemClass) {
		this.chartItemClass = chartItemClass;
	}
	
	public HashMap<String, String> getChartItemAttributes() {
		return attributes;
	}

	public String getChartItemAttribute(String key) {
		return attributes.get(key);
	}
	
	public void addChartItemAttribute(String key, String value) {
		attributes.put(key, value);
	}
	
	public void removeChartItemAttribute(String key) {
		attributes.remove(key);
	}
	
	public void read(Element chartItemSpec, FlowChartManager flowCharts) throws MaltChainedException {
		chartItemName = chartItemSpec.getAttribute("item");
		chartItemClass = flowCharts.getFlowChartSystem().getChartElement(chartItemName).getChartItemClass();
		
		NamedNodeMap attrs = chartItemSpec.getAttributes();  
		for(int i = 0 ; i < attrs.getLength() ; i++) {
			Attr attribute = (Attr)attrs.item(i);
			addChartItemAttribute(attribute.getName(),attribute.getValue());
		}
	}

	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((chartItemName == null) ? 0 : chartItemName.hashCode());
		result = prime * result + ((attributes == null) ? 0 : attributes.hashCode());
		result = prime * result + ((chartItemClass == null) ? 0 : chartItemClass.hashCode());
		return result;
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ChartItemSpecification other = (ChartItemSpecification) obj;
		if (chartItemName == null) {
			if (other.chartItemName != null)
				return false;
		} else if (!chartItemName.equals(other.chartItemName))
			return false;
		
		if (attributes == null) {
			if (other.attributes != null)
				return false;
		} else if (!attributes.equals(other.attributes))
			return false;
		
		if (chartItemClass == null) {
			if (other.chartItemClass != null)
				return false;
		} else if (!chartItemClass.equals(other.chartItemClass))
			return false;

		return true;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(chartItemName);sb.append(' ');
		for (String key : attributes.keySet()) {
			sb.append(key);sb.append('=');sb.append(attributes.get(key));sb.append(' ');
		}
		return sb.toString();
	}
	
}
