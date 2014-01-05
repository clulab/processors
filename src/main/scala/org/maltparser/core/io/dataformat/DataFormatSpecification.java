package org.maltparser.core.io.dataformat;

import java.net.URL;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.HashSet;
import org.maltparser.core.helper.URLFinder;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 *  
 *
 * @author Johan Hall
 * @since 1.0
**/
public class DataFormatSpecification {	
	public enum DataStructure {
		DEPENDENCY,  // Dependency structure
		PHRASE, // Phrase structure
	};
//	private int entryPositionCounter;
	private String dataFormatName;
	private DataStructure dataStructure;
	private final Map<String, DataFormatEntry> entries;
	private final HashSet<Dependency> dependencies;
//	private final HashSet<SyntaxGraphReader> supportedReaders;
//	private final HashSet<SyntaxGraphWriter> supportedWriters;
	
	public DataFormatSpecification() {
		entries = new LinkedHashMap<String, DataFormatEntry>();
//		entryPositionCounter = 0;
		dependencies = new HashSet<Dependency>();
//		supportedReaders = new HashSet<SyntaxGraphReader>();
//		supportedWriters = new HashSet<SyntaxGraphWriter>();
	}
	
	public DataFormatInstance createDataFormatInstance(SymbolTableHandler symbolTables, String nullValueStrategy) throws MaltChainedException {
		return new DataFormatInstance(entries, symbolTables, nullValueStrategy, this); //rootLabel, this);

	}
	
	public void parseDataFormatXMLfile(String fileName) throws MaltChainedException {
		final URLFinder f = new URLFinder();
		URL url = f.findURL(fileName);
		if (url == null) {
			throw new DataFormatException("The data format specifcation file '"+fileName+"'cannot be found. ");
		}
		parseDataFormatXMLfile(url);
	}
	
	public HashSet<Dependency> getDependencies() {
		return dependencies;
	}
	
	public void parseDataFormatXMLfile(URL url) throws MaltChainedException {
		if (url == null) {
			throw new DataFormatException("The data format specifcation file cannot be found. ");
		}

        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();

    		Element root = db.parse(url.openStream()).getDocumentElement();
    		if (root.getNodeName().equals("dataformat")) { 
    			dataFormatName = root.getAttribute("name");
    			if (root.getAttribute("datastructure").length() > 0) {
    				dataStructure = DataStructure.valueOf(root.getAttribute("datastructure").toUpperCase());
    			} else {
    				dataStructure = DataStructure.DEPENDENCY;
    			}
    		} else {
    			throw new DataFormatException("Data format specification file must contain one 'dataformat' element. ");
    		}
    		NodeList cols = root.getElementsByTagName("column");
            Element col = null;
            for (int i = 0, n = cols.getLength(); i < n; i++) {
            	col = (Element)cols.item(i);
            	DataFormatEntry entry = new DataFormatEntry(col.getAttribute("name"), col.getAttribute("category"),col.getAttribute("type"), col.getAttribute("default"));
            	entries.put(entry.getDataFormatEntryName(), entry);
            }
            NodeList deps = root.getElementsByTagName("dependencies");
            if (deps.getLength() > 0) {
            	NodeList dep = ((Element)deps.item(0)).getElementsByTagName("dependency");
            	for (int i = 0, n = dep.getLength(); i < n; i++) {
            		Element e = (Element)dep.item(i);
            		dependencies.add(new Dependency(e.getAttribute("name"), e.getAttribute("url"), e.getAttribute("map"), e.getAttribute("urlmap")));
            	}
            }
        } catch (java.io.IOException e) {
        	throw new DataFormatException("Cannot find the file "+url.toString()+". ", e);
        } catch (ParserConfigurationException e) {
        	throw new DataFormatException("Problem parsing the file "+url.toString()+". ", e);
        } catch (SAXException e) {
        	throw new DataFormatException("Problem parsing the file "+url.toString()+". ", e);
        }
	}
	
	public void addEntry(String dataFormatEntryName, String category, String type, String defaultOutput) {
		DataFormatEntry entry = new DataFormatEntry(dataFormatEntryName, category, type, defaultOutput);
		entries.put(entry.getDataFormatEntryName(), entry);
	}
	
	public DataFormatEntry getEntry(String dataFormatEntryName) {
		return entries.get(dataFormatEntryName);
	}

	public String getDataFormatName() {
		return dataFormatName;
	}

	public DataStructure getDataStructure() {
		return dataStructure;
	}

	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("Data format specification: ");
		sb.append(dataFormatName);
		sb.append('\n');
		for (DataFormatEntry dfe : entries.values()) {
			sb.append(dfe);
			sb.append('\n');
		}
		return sb.toString();
	}
	
	public class Dependency {
		protected String dependentOn;
		protected String urlString;
		protected String map;
		protected String mapUrl;
		
		public Dependency(String dependentOn, String urlString, String map, String mapUrl) {
			setDependentOn(dependentOn);
			setUrlString(urlString);
			setMap(map);
			setMapUrl(mapUrl);
		}
		
		public String getDependentOn() {
			return dependentOn;
		}
		protected void setDependentOn(String dependentOn) {
			this.dependentOn = dependentOn;
		}
		
		public String getUrlString() {
			return urlString;
		}

		public void setUrlString(String urlString) {
			this.urlString = urlString;
		}

		public String getMap() {
			return map;
		}
		protected void setMap(String map) {
			this.map = map;
		}

		public String getMapUrl() {
			return mapUrl;
		}

		public void setMapUrl(String mapUrl) {
			this.mapUrl = mapUrl;
		}

		@Override
		public String toString() {
			return "Dependency [dependentOn=" + dependentOn + ", map=" + map
					+ ", mapUrl=" + mapUrl + ", urlString=" + urlString + "]";
		}
	}
}
