package org.maltparser.core.flow.system;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureException;
import org.maltparser.core.flow.FlowException;
import org.maltparser.core.flow.system.elem.ChartElement;
import org.maltparser.core.helper.URLFinder;
import org.maltparser.core.plugin.Plugin;
import org.maltparser.core.plugin.PluginLoader;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
/**
*
*
* @author Johan Hall
*/
public class FlowChartSystem {
	private HashMap<String,ChartElement> chartElements;
	
	public FlowChartSystem() {
		chartElements = new HashMap<String,ChartElement>();
	}
	
	public void load(String urlstring) throws MaltChainedException {
		final URLFinder f = new URLFinder();
		load(f.findURL(urlstring));
	}
	
	public void load(PluginLoader plugins) throws MaltChainedException {
		 for (Plugin plugin : plugins) {
			URL url = null;
			try {
				url = new URL("jar:"+plugin.getUrl() + "!/appdata/plugin.xml");
			} catch (MalformedURLException e) {
				throw new FeatureException("Malformed URL: 'jar:"+plugin.getUrl() + "!plugin.xml'", e);
			}
			try { 
				InputStream is = url.openStream();
				is.close();
			} catch (IOException e) {
				continue;
			}

			load(url);
		}
	}
	
	public void load(URL specModelURL) throws MaltChainedException {
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Element root = null;

            root = db.parse(specModelURL.openStream()).getDocumentElement();

            if (root == null) {
            	throw new FlowException("The flow chart system file '"+specModelURL.getFile()+"' cannot be found. ");
            }
            
            readChartElements(root);
        } catch (IOException e) {
        	throw new FlowException("The flow chart system file '"+specModelURL.getFile()+"' cannot be found. ", e);
        } catch (ParserConfigurationException e) {
        	throw new FlowException("Problem parsing the file "+specModelURL.getFile()+". ", e);
        } catch (SAXException e) {
        	throw new FlowException("Problem parsing the file "+specModelURL.getFile()+". ", e);
        }
	}
	
	public void readChartElements(Element root) throws MaltChainedException {
		NodeList chartElem = root.getElementsByTagName("chartelement");
		for (int i = 0; i < chartElem.getLength(); i++) {
			ChartElement chartElement = new ChartElement();
			chartElement.read((Element)chartElem.item(i), this);
			chartElements.put(((Element)chartElem.item(i)).getAttribute("item"),chartElement);
		}
	}
	
	public ChartElement getChartElement(String name) {
		return chartElements.get(name);
	}
	
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("CHART ELEMENTS:\n");
		for (String key : chartElements.keySet()) {
			sb.append(chartElements.get(key));
			sb.append('\n');
		}
		return sb.toString();
	}
}
