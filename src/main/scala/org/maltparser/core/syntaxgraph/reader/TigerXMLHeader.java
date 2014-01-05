package org.maltparser.core.syntaxgraph.reader;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.SortedMap;
import java.util.TreeMap;

import org.maltparser.core.helper.Util;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.SymbolTableHandler;
/**
*
*
* @author Johan Hall
*/
public class TigerXMLHeader {
	public enum Domain {
		T, // feature for terminal nodes 
		NT, // feature for nonterminal nodes 
		FREC, //feature for both
		EL, // edge label (same as "edgelabel" in TigerXML schema)
		SEL // secondary edge Label (same as "secedgelabel" in TigerXML schema)
	};

	private String corpusID;
	private String corpusVersion;
	private String external;
	private String metaName;
	private String metaAuthor;
	private String metaDescription;
	private String metaInDate;
	private String metaFormat;
	private String metaHistory;
	private SymbolTableHandler symbolTableHandler;
	private FeatureEdgeLabel edgeLabels;
	private FeatureEdgeLabel secEdgeLabels;
	private LinkedHashMap<String,FeatureEdgeLabel> features;
	
	public TigerXMLHeader(SymbolTableHandler symbolTableHandler) { 
		setSymbolTableHandler(symbolTableHandler);
		features = new LinkedHashMap<String,FeatureEdgeLabel>();
	}

	public boolean isTigerXMLWritable() {
		return true;
		//return features.size() > 0;
	}
	
	public void addFeature(String featureName, String domainName) {
		if (!features.containsKey(featureName)) {
			features.put(featureName, new FeatureEdgeLabel(featureName, domainName));
		} 
	}
	
	public void addFeatureValue(String featureName, String name) {
		addFeatureValue(featureName, name, "\t");
	}
	
	public void addFeatureValue(String featureName, String name, String desc) {
		if (features.containsKey(featureName)) {
			if (desc == null || desc.length() == 0) {
				features.get(featureName).addValue(name, "\t");
			} else {
				features.get(featureName).addValue(name, desc);
			}
		} 
	}
	
	public void addEdgeLabelValue(String name) {
		addEdgeLabelValue(name, "\t");
	}
	
	public void addEdgeLabelValue(String name, String desc) {
		if (edgeLabels == null) {
			edgeLabels = new FeatureEdgeLabel("edgelabel", Domain.EL);
		}
		if (desc == null || desc.length() == 0) {
			edgeLabels.addValue(name, "\t");
		} else {
			edgeLabels.addValue(name, desc);
		}
	}
	
	public void addSecEdgeLabelValue(String name) {
		addSecEdgeLabelValue(name, "\t");
	}
	
	public void addSecEdgeLabelValue(String name, String desc) {
		if (secEdgeLabels == null) {
			secEdgeLabels = new FeatureEdgeLabel("secedgelabel", Domain.SEL);
		}
		if (desc == null || desc.length() == 0) {
			secEdgeLabels.addValue(name, "\t");
		} else {
			secEdgeLabels.addValue(name, desc);
		}
	}
	
	public String getCorpusID() {
		return corpusID;
	}

	public void setCorpusID(String corpusID) {
		this.corpusID = corpusID;
	}

	public String getCorpusVersion() {
		return corpusVersion;
	}

	public void setCorpusVersion(String corpusVersion) {
		this.corpusVersion = corpusVersion;
	}

	public void setExternal(String external) {
		this.external = external;
	}
	
	public String getExternal() {
		return external;
	}
	
	public void setMeta(String metaElement, String value) {
		if (metaElement.equals("name")) 		{ setMetaName(value); }
		if (metaElement.equals("author")) 		{ setMetaAuthor(value); }
		if (metaElement.equals("description")) 	{ setMetaDescription(value); }
		if (metaElement.equals("date")) 		{ setMetaInDate(value); }
		if (metaElement.equals("format")) 		{ setMetaFormat(value); }
		if (metaElement.equals("history")) 		{ setMetaHistory(value); }
	}

	public String getMetaName() {
		return metaName;
	}

	public void setMetaName(String metaName) {
		this.metaName = metaName;
	}

	public String getMetaAuthor() {
		return metaAuthor;
	}
	
	public void setMetaAuthor(String metaAuthor) {
		this.metaAuthor = metaAuthor;
	}

	public String getMetaDescription() {
		return metaDescription;
	}
	
	public void setMetaDescription(String metaDescription) {
		this.metaDescription = metaDescription;
	}
	
	public String getMetaInDate() {
		return metaInDate;
	}

	public String getMetaCurrentDate() {
		return getMetaCurrentDate("yyyy-MM-dd HH:mm:ss"); 
	}
	
	public String getMetaCurrentDate(String format) {
		return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date());
	}
	
	public void setMetaInDate(String metaInDate) {
		this.metaInDate = metaInDate;
	}

	public String getMetaFormat() {
		return metaFormat;
	}

	public void setMetaFormat(String metaFormat) {
		this.metaFormat = metaFormat;
	}

	public String getMetaHistory() {
		return metaHistory;
	}

	public void setMetaHistory(String metaHistory) {
		this.metaHistory = metaHistory;
	}
	
	public SymbolTableHandler getSymbolTableHandler() {
		return symbolTableHandler;
	}

	protected void setSymbolTableHandler(SymbolTableHandler symbolTableHandler) {
		this.symbolTableHandler = symbolTableHandler;
	}

	public String toTigerXML() {
		final StringBuilder sb = new StringBuilder();
		
		if (getCorpusVersion() == null) {
			sb.append("<corpus id=\"");
			sb.append(((getCorpusID() == null)?"GeneratedByMaltParser":getCorpusID()));
			sb.append("\">\n");
		} else {
			sb.append("<corpus id=\"");
			sb.append(((getCorpusID() == null)?"GeneratedByMaltParser":getCorpusID()));
			sb.append("\" version=\"");
			sb.append(getCorpusVersion());
			sb.append("\">\n");
		}
		sb.append("  <head>\n");
		sb.append("    <meta>\n");
		sb.append("      <name>");
		sb.append(((getMetaName() == null)?"GeneratedByMaltParser":Util.xmlEscape(getMetaName())));
		sb.append("</name>\n");
		sb.append("      <author>MaltParser</author>\n");
		sb.append("      <date>");
		sb.append(getMetaCurrentDate());
		sb.append("</date>\n");
		
		sb.append("      <description>");
		sb.append(Util.xmlEscape("Unfortunately, you have to add the annotations header data yourself. Maybe in later releases this will be fixed. "));
		sb.append("</description>\n");
		
//		if (getMetaDescription() != null) {
//			sb.append("      <description>");
//			sb.append(Util.xmlEscape(getMetaDescription()));
//			sb.append("</description>\n");
//		}
//		if (getMetaFormat() != null) {
//			sb.append("      <format>");
//			sb.append(Util.xmlEscape(getMetaFormat()));
//			sb.append("</format>\n");
//		}
//		if (getMetaHistory() != null) {
//			sb.append("      <history>");
//			sb.append(Util.xmlEscape(getMetaHistory()));
//			sb.append("</history>\n");
//		}
		sb.append("    </meta>\n");
		sb.append("    <annotation/>\n");
//		sb.append("    <annotation>\n");
//		for (String name : features.keySet()) {
//			sb.append(features.get(name).toTigerXML());
//		}
//		if (edgeLabels != null) {
//			sb.append(edgeLabels.toTigerXML());
//		}
//		if (secEdgeLabels != null) {
//			sb.append(secEdgeLabels.toTigerXML());
//		}
//		sb.append("    </annotation>\n");
		sb.append("  </head>\n");
		sb.append("  <body>\n");
		return sb.toString();
	}
	
	public String toString() {
		return toTigerXML();
	}
	
	protected class FeatureEdgeLabel {
		private String name;
		private Domain domain;
		// values: key mapped to \t (tab) indicates that the description part is missing
		private SortedMap<String, String> values; 
		private SymbolTable table;
		
		public FeatureEdgeLabel(String name, String domainName) { 
			setName(name);
			setDomain(domainName);
		}

		public FeatureEdgeLabel(String name, Domain domain) { 
			setName(name);
			setDomain(domain);
		}
		
		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
		
		public void setDomain(String domainName) {
			domain = Domain.valueOf(domainName);
		}
		
		public void setDomain(Domain domain) {
			this.domain = domain;
		}
		
		public String getDomainName() {
			return domain.toString();
		}
		
		public Domain getDomain() {
			return domain;
		}
		
		public SymbolTable getTable() {
			return table;
		}

		public void setTable(SymbolTable table) {
			this.table = table;
		}

		public void addValue(String name) {
			addValue(name, "\t");
		}
		
		public void addValue(String name, String desc) {
			if (values == null) {
				values = new TreeMap<String,String>();
			}
			values.put(name, desc);
		}
		
		public String toTigerXML() {
			final StringBuilder sb = new StringBuilder();
			if (domain == Domain.T || domain == Domain.FREC || domain == Domain.NT) {
				sb.append("      <feature domain=\"");
				sb.append(getDomainName());
				sb.append("\" name=\"");
				sb.append(getName());
				sb.append((values == null)?"\" />\n":"\">\n");
			}
			if (domain == Domain.EL) {
				sb.append((values != null)?"      <edgelabel>\n":"      <edgelabel />\n");
			}
			if (domain == Domain.SEL) {
				sb.append((values != null)?"      <secedgelabel>\n":"      <secedgelabel />\n");
			}
			if (values != null) {
				for (String name : values.keySet()) {
					sb.append("        <value name=\"");
					sb.append(name);
					if (values.get(name).equals("\t")) {
						sb.append("\" />\n");
					} else {
						sb.append("\">");
						sb.append(Util.xmlEscape(values.get(name)));
						sb.append("</value>\n");
					}
				}
			}
			if (domain == Domain.T || domain == Domain.FREC || domain == Domain.NT) {
				if (values != null) {
					sb.append("      </feature>\n");
				}
			}
			if (domain == Domain.EL && values != null) {
				sb.append("      </edgelabel>\n");
			}
			if (domain == Domain.SEL && values != null) {
				sb.append("      </secedgelabel>\n");
			}
			return sb.toString();
		}
		
		public String toString() {
			return toTigerXML();
		}
	}
}	

  
 