package org.maltparser.core.flow.system.elem;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.system.FlowChartSystem;
import org.w3c.dom.Element;
/**
*
*
* @author Johan Hall
*/
public class ChartAttribute {
	private String name;
	private String defaultValue;
	
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	public void read(Element attrElem, FlowChartSystem flowChartSystem) throws MaltChainedException {
		setName(attrElem.getAttribute("name"));
		setDefaultValue(attrElem.getAttribute("default"));
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(name);
		sb.append(' ');
		sb.append(defaultValue);
		return sb.toString();
	}
}
