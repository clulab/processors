package org.maltparser.core.flow.system.elem;

import java.util.LinkedHashMap;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowException;
import org.maltparser.core.flow.item.ChartItem;
import org.maltparser.core.flow.system.FlowChartSystem;
import org.maltparser.core.plugin.PluginLoader;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
/**
*
*
* @author Johan Hall
*/
public class ChartElement {
	private String item;
	private Class<? extends ChartItem> chartItemClass;	
	private LinkedHashMap<String,ChartAttribute> attributes;
	
	public ChartElement() {
		attributes = new LinkedHashMap<String,ChartAttribute>();
	}

	public String getItem() {
		return item;
	}

	public void setItem(String item) {
		this.item = item;
	}
	
	public void addAttribute(String name, ChartAttribute attribute) {
		attributes.put(name, attribute);
	}
	
	public ChartAttribute getAttribute(String name) {
		return attributes.get(name);
	}
	
	public Class<? extends ChartItem> getChartItemClass() {
		return chartItemClass;
	}

	
	public LinkedHashMap<String, ChartAttribute> getAttributes() {
		return attributes;
	}

	public void read(Element chartElem, FlowChartSystem flowChartSystem) throws MaltChainedException {
		setItem(chartElem.getAttribute("item"));
		String chartItemClassName = chartElem.getAttribute("class");	
		Class<?> clazz = null;
		try {
			if (PluginLoader.instance() != null) {
				clazz = PluginLoader.instance().getClass(chartItemClassName);
			}
			if (clazz == null) {
				clazz = Class.forName(chartItemClassName);
			}
			this.chartItemClass = clazz.asSubclass(org.maltparser.core.flow.item.ChartItem.class);
		} catch (ClassCastException e) {
			throw new FlowException("The class '"+clazz.getName()+"' is not a subclass of '"+org.maltparser.core.flow.item.ChartItem.class.getName()+"'. ", e);
		} catch (ClassNotFoundException e) {
			throw new FlowException("The class "+chartItemClassName+"  could not be found. ", e);
		}
		NodeList attrElements = chartElem.getElementsByTagName("attribute");
		for (int i = 0; i < attrElements.getLength(); i++) {
			ChartAttribute attribute = new ChartAttribute();
			attribute.read((Element)attrElements.item(i),flowChartSystem);
			attributes.put(((Element)attrElements.item(i)).getAttribute("name"), attribute);
		}
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("    ");
		sb.append(item);
		sb.append(' ');
		sb.append(chartItemClass.getName());
		sb.append('\n');
		for (String key : attributes.keySet()) {
			sb.append("       ");
			sb.append(attributes.get(key));
			sb.append('\n');
		}
		return sb.toString();
	}
}
