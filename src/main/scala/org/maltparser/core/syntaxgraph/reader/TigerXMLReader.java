package org.maltparser.core.syntaxgraph.reader;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.SortedMap;
import java.util.regex.PatternSyntaxException;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.DataFormatException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.MappablePhraseStructureGraph;
import org.maltparser.core.syntaxgraph.PhraseStructure;
import org.maltparser.core.syntaxgraph.SyntaxGraphException;
import org.maltparser.core.syntaxgraph.TokenStructure;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.NonTerminalNode;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;

/**
*
*
* @author Johan Hall
*/
public class TigerXMLReader implements SyntaxGraphReader {
//	private TigerXMLHeader header;
	private XMLStreamReader reader;
	private int sentenceCount;
	private DataFormatInstance dataFormatInstance;
	private StringBuffer ntid;
	private final StringBuilder graphRootID;
//	private StringBuilder elementContent; 
//	private StringBuilder valueName;
//	private StringBuilder currentFeatureName;
//	private Domain domain;
//	private boolean collectChar = false;
	private String optionString;
	private String fileName = null;
	private URL url = null;
	private String charsetName;
	private int nIterations;
	private int cIterations;
	private int START_ID_OF_NONTERMINALS = 500;
	private boolean closeStream = true;
	
	public TigerXMLReader() {
		this.ntid = new StringBuffer();
//		elementContent = new StringBuilder();
//		valueName = new StringBuilder();
//		currentFeatureName = new StringBuilder(); 
		graphRootID = new StringBuilder(); 
		nIterations = 1;
		cIterations = 1;
	}
	
	private void reopen() throws MaltChainedException {
		close();
		if (fileName != null) {
			open(fileName, charsetName);
		} else if (url != null) {
			open(url, charsetName);
		} else {
			throw new DataFormatException("The input stream cannot be reopen. ");
		}
	}
	
	public void open(String fileName, String charsetName) throws MaltChainedException {
		setFileName(fileName);
		setCharsetName(charsetName);
		try {
			open(new FileInputStream(fileName), charsetName);
		}catch (FileNotFoundException e) {
			throw new DataFormatException("The input file '"+fileName+"' cannot be found. ", e);
		}
	}
	public void open(URL url, String charsetName) throws MaltChainedException {
		setUrl(url);
		setCharsetName(charsetName);
		try {
			open(url.openStream(), charsetName);
		} catch (IOException e) {
			throw new DataFormatException("The URL '"+url.toString()+"' cannot be opened. ", e);
		}
	}
	
	public void open(InputStream is, String charsetName) throws MaltChainedException {
		try {
			if (is == System.in) {
				closeStream = false;
			}
			open(new InputStreamReader(is, charsetName));
		} catch (UnsupportedEncodingException e) {
			throw new DataFormatException("The character encoding set '"+charsetName+"' isn't supported. ", e);
		}
	}
	
	private void open(InputStreamReader isr) throws MaltChainedException {
		try {
			XMLInputFactory factory = XMLInputFactory.newInstance();
			setReader(factory.createXMLStreamReader(new BufferedReader(isr)));
		} catch (XMLStreamException e) {
			throw new DataFormatException("XML input file could be opened. ", e);
		} 
		setSentenceCount(0);
	}
	
	public void readProlog() throws MaltChainedException {
		
	}
	
	public boolean readSentence(TokenStructure syntaxGraph) throws MaltChainedException  {
		if (syntaxGraph == null || !(syntaxGraph instanceof PhraseStructure)) {
			return false;
		}
		syntaxGraph.clear();
		final PhraseStructure phraseStructure = (PhraseStructure)syntaxGraph;
		PhraseStructureNode parent = null;
		PhraseStructureNode child = null;
//		if (header == null) {
//			header = new TigerXMLHeader(syntaxGraph.getSymbolTables());
//		}

		try {
			while (true) {
				int event = reader.next();
				if (event == XMLStreamConstants.START_ELEMENT) {
					if (reader.getLocalName().length() == 0) {
						continue;
					}
					if (reader.getLocalName().charAt(0) == 'e') {
						// e -> edge, edgelabel
						if (reader.getLocalName().length() == 4) { //edge
							int childid = -1;
							int indexSep = reader.getAttributeValue(null, "idref").indexOf('_');
							
							try {
								if (indexSep != -1) {
									childid = Integer.parseInt(reader.getAttributeValue(null, "idref").substring(indexSep+1));
								} else {
									childid = Integer.parseInt(reader.getAttributeValue(null, "idref"));
								}
								if (childid == -1) {
									throw new SyntaxGraphException("The tiger reader couldn't recognize the idref attribute '"+reader.getAttributeValue(null, "idref")+"' of the edge element. ");
								}
							} catch (NumberFormatException e) {
								throw new SyntaxGraphException("The tiger reader couldn't recognize the idref attribute '"+reader.getAttributeValue(null, "idref")+"' of the edge element. ");
							}

							if (childid < START_ID_OF_NONTERMINALS) {
								child = phraseStructure.getTokenNode(childid);
							} else {

								child = phraseStructure.getNonTerminalNode(childid-START_ID_OF_NONTERMINALS+1);
							}

							Edge e = phraseStructure.addPhraseStructureEdge(parent, child);
							SortedMap<String, SymbolTable> inputTables = dataFormatInstance.getPhraseStructureEdgeLabelSymbolTables();
							for (String name : inputTables.keySet()) {
								e.addLabel(inputTables.get(name), reader.getAttributeValue(null, name.toLowerCase()));
							}
						} else if (reader.getLocalName().equals("edgelabel")) { // edgelabel
//							domain = Domain.EL;
						}
					} else if (reader.getLocalName().charAt(0) == 'n') {
						// n -> nt, nonterminals, name
						if (reader.getLocalName().length() == 2) { // nt
							final String id = reader.getAttributeValue(null, "id");
							if (graphRootID.length() == id.length() && graphRootID.toString().equals(id)) {
								parent = phraseStructure.getPhraseStructureRoot();
							} else {
								int index = id.indexOf('_');
								if (index != -1) {
									parent = phraseStructure.addNonTerminalNode(Integer.parseInt(id.substring(index+1))-START_ID_OF_NONTERMINALS+1);
								}
							}
							SortedMap<String, SymbolTable> inputTables = dataFormatInstance.getPhraseStructureNodeLabelSymbolTables();
							for (String name : inputTables.keySet()) {
								parent.addLabel(inputTables.get(name), reader.getAttributeValue(null, name.toLowerCase()));
							}
						} else if (reader.getLocalName().equals("name")) { // name
//							elementContent.setLength(0);
//							collectChar = true;
						}
					} else if (reader.getLocalName().charAt(0) == 't') {
						// t -> t, terminals
						if (reader.getLocalName().length() == 1) { // t
							SortedMap<String, SymbolTable> inputTables = dataFormatInstance.getInputSymbolTables();
							child = syntaxGraph.addTokenNode();
							for (String name : inputTables.keySet()) {
								child.addLabel(inputTables.get(name), reader.getAttributeValue(null, name.toLowerCase()));
							}
						}
					} else if (reader.getLocalName().charAt(0) == 's') {
						// s -> subcorpus, secedge, s, secedgelabel
						if (reader.getLocalName().length() == 1) { // s
							String id = reader.getAttributeValue(null, "id");
							boolean indexable = false;
							int index = -1;
							if (id != null && id.length() > 0) {
								for (int i = 0, n = id.length(); i < n; i++) {
									if (Character.isDigit(id.charAt(i))) {
										if (index == -1) { 
											index = i;
										}
										indexable = true;
									}
								}
							}
							if (indexable) {
								phraseStructure.setSentenceID(Integer.parseInt(id.substring(index)));
							} else {
								phraseStructure.setSentenceID(sentenceCount+1);
							}
						}
					} else if (reader.getLocalName().charAt(0) == 'v') {
						// v -> variable, value
//						if (reader.getLocalName().equals("value")) {
//							valueName.setLength(0);
//							valueName.append(reader.getAttributeValue(null, "name"));
//							elementContent.setLength(0);
//							collectChar = true;
//						}
					} else {
//						 a -> annotation, author
//						 b -> body
//						 c -> corpus
//						 d -> date, description,
//						 f -> feature, format
//						 g -> graph
//						 h -> head, history
//						 m -> matches, match
						if (reader.getLocalName().equals("graph")) {
							graphRootID.setLength(0);
							graphRootID.append(reader.getAttributeValue(null, "root"));
						} else  if (reader.getLocalName().equals("corpus")) {
//							header.setCorpusID(reader.getAttributeValue(null, "id"));
//							header.setCorpusID(reader.getAttributeValue(null, "version"));
						} else if (reader.getLocalName().equals("feature")) {
//							if (header != null) {
//								currentFeatureName.setLength(0);
//								currentFeatureName.append(reader.getAttributeValue(null, "name"));
//								header.addFeature(reader.getAttributeValue(null, "name"), reader.getAttributeValue(null, "domain"));
//							}
//							domain = Domain.valueOf(reader.getAttributeValue(null, "domain"));
						} else if (reader.getLocalName().equals("secedgelabel")) {
//							domain = Domain.SEL;
						} else if (reader.getLocalName().equals("author")) {
//							elementContent.setLength(0);
//							collectChar = true;
						} else if (reader.getLocalName().equals("date")) {
//							elementContent.setLength(0);
//							collectChar = true;
						} else if (reader.getLocalName().equals("description")) {
//							elementContent.setLength(0);
//							collectChar = true;
						} else if (reader.getLocalName().equals("format")) {
//							elementContent.setLength(0);
//							collectChar = true;
						} else if (reader.getLocalName().equals("history")) {
//							elementContent.setLength(0);
//							collectChar = true;
						} 
					}
				} else if (event == XMLStreamConstants.END_ELEMENT) {
					if (reader.getLocalName().length() == 0) {
						continue;
					}
					if (reader.getLocalName().charAt(0) == 'e') {
						// e -> edge, edgelabel
					} else if (reader.getLocalName().charAt(0) == 'n') {
						// n -> nt, nonterminals, name
						if (reader.getLocalName().equals("nt")) {
							ntid.setLength(0);
						}
						else if (reader.getLocalName().equals("nonterminals")) {
							if (phraseStructure.nTokenNode() == 1 && phraseStructure.nNonTerminals() == 0 &&((NonTerminalNode)phraseStructure.getPhraseStructureRoot()).nChildren() == 0) {
								Edge e = phraseStructure.addPhraseStructureEdge(phraseStructure.getPhraseStructureRoot(), phraseStructure.getTokenNode(1));
								SortedMap<String, SymbolTable> inputTables = dataFormatInstance.getPhraseStructureEdgeLabelSymbolTables();
								for (String name : inputTables.keySet()) {
									e.addLabel(inputTables.get(name), "--");
								}
							}
						}
//						else if (reader.getLocalName().equals("name")) {
//							if (header != null) {
//								header.setMetaName(elementContent.toString());
//							}
//							collectChar = false;
//						}
					} else if (reader.getLocalName().charAt(0) == 't') {
						// t -> t, terminals
					} else if (reader.getLocalName().charAt(0) == 's') {
						// s -> subcorpus, secedge, s, secedgelabel
						if (reader.getLocalName().equals("s")) {
							if (syntaxGraph.hasTokens()) {
								sentenceCount++;
							}
							if (syntaxGraph instanceof MappablePhraseStructureGraph) {
								((MappablePhraseStructureGraph)syntaxGraph).getMapping().updateDependenyGraph(((MappablePhraseStructureGraph)syntaxGraph), ((PhraseStructure)syntaxGraph).getPhraseStructureRoot());
							}
							return true;
						}
					} else if (reader.getLocalName().charAt(0) == 'v') {
						// v -> variable, value
//						if (reader.getLocalName().equals("value")) {
//							if (header != null) {
//								if (domain == Domain.T || domain == Domain.NT || domain == Domain.FREC) {
//									header.addFeatureValue(currentFeatureName.toString(), valueName.toString(), elementContent.toString());
//								} else if (domain == Domain.EL) {
//									header.addEdgeLabelValue(valueName.toString(), elementContent.toString());
//								} else if (domain == Domain.SEL) {
//									header.addSecEdgeLabelValue(valueName.toString(), elementContent.toString());
//								}
//							}
//							collectChar = false;
//						}
					} else {
//						 a -> annotation, author
//						 b -> body
//						 c -> corpus
//						 d -> date, description,
//						 f -> feature, format
//						 g -> graph
//						 h -> head, history
//						 m -> matches, match
						if (reader.getLocalName().equals("body")) {
							//sentence = dataStructures.getSentence();
							//phraseTree = dataStructures.getInPhraseTree();
							//sentence.clear();
							//phraseTree.clear();
							//dataStructures.setLastProcessObject(true);
						}  else if (reader.getLocalName().equals("author")) {
//							if (header != null) {
//								header.setMetaAuthor(elementContent.toString());
//							}
//							collectChar = false;
						} else if (reader.getLocalName().equals("date")) {
//							if (header != null) {
//								header.setMetaInDate(elementContent.toString());
//							}
//							collectChar = false;
						} else if (reader.getLocalName().equals("description")) {
//							if (header != null) {
//								header.setMetaDescription(elementContent.toString());
//							}
//							collectChar = false;
						} else if (reader.getLocalName().equals("format")) {
//							if (header != null) {
//								header.setMetaFormat(elementContent.toString());
//							}
//							collectChar = false;
						} else if (reader.getLocalName().equals("history")) {
//							if (header != null) {
//								header.setMetaHistory(elementContent.toString());
//							}
//							collectChar = false;
						} /* else if (reader.getLocalName().equals("annotation")) {
							if (header != null) {
								System.out.println(header.toTigerXML());
							}
							collectChar = false;
						} */
					}				
				} else if (event == XMLStreamConstants.END_DOCUMENT) {
					if (syntaxGraph.hasTokens()) {
						sentenceCount++;
					}
					if (cIterations < nIterations) {
						cIterations++;
						reopen();
						return true;
					}
					return false;
				} else if (event == XMLStreamConstants.CHARACTERS) {
//					if (collectChar) {
//						char[] ch = reader.getTextCharacters();
//						final int size = reader.getTextStart()+reader.getTextLength();
//						for (int i = reader.getTextStart(); i < size; i++) {
//							elementContent.append(ch[i]);
//						}
//					}
				}
			}
		} catch (XMLStreamException e) {
			throw new DataFormatException("", e);
		}
	}
	
	public int getSentenceCount() {
		return sentenceCount;
	}

	public void setSentenceCount(int sentenceCount) {
		this.sentenceCount = sentenceCount;
	}
	
	public XMLStreamReader getReader() {
		return reader;
	}

	public void setReader(XMLStreamReader reader) {
		this.reader = reader;
	}
	
	public void readEpilog() throws MaltChainedException {
		
	}
	
	public void close() throws MaltChainedException {
		try {
			if (reader != null) {
				if (closeStream) {
					reader.close();
				}
				reader = null;
			}
		} catch (XMLStreamException e) {
			throw new DataFormatException("The XML input file could be closed. ", e);
		}
	}

	public DataFormatInstance getDataFormatInstance() {
		return dataFormatInstance;
	}
	
	public void setDataFormatInstance(DataFormatInstance inputDataFormatInstance) {
		this.dataFormatInstance = inputDataFormatInstance;
	}
	
	public String getOptions() {
		return optionString;
	}
	
	public void setOptions(String optionString) throws MaltChainedException {
		this.optionString = optionString;
		String[] argv;
		try {
			argv = optionString.split("[_\\p{Blank}]");
		} catch (PatternSyntaxException e) {
			throw new DataFormatException("Could not split the TigerXML reader option '"+optionString+"'. ", e);
		}
		for (int i=0; i < argv.length-1; i++) {
			if(argv[i].charAt(0) != '-') {
				throw new DataFormatException("The argument flag should start with the following character '-', not with "+argv[i].charAt(0));
			}
			if(++i>=argv.length) {
				throw new DataFormatException("The last argument does not have any value. ");
			}
			switch(argv[i-1].charAt(1)) {
			case 's': 
				try {
					START_ID_OF_NONTERMINALS = Integer.parseInt(argv[i]);
				} catch (NumberFormatException e){
					throw new MaltChainedException("The TigerXML Reader option -s must be an integer value. ");
				}
				break;
			default:
				throw new DataFormatException("Unknown TigerXMLReader parameter: '"+argv[i-1]+"' with value '"+argv[i]+"'. ");		
			}
		}
	}
	
	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public URL getUrl() {
		return url;
	}

	public void setUrl(URL url) {
		this.url = url;
	}

	public String getCharsetName() {
		return charsetName;
	}

	public void setCharsetName(String charsetName) {
		this.charsetName = charsetName;
	}

	public int getNIterations() {
		return nIterations;
	}

	public void setNIterations(int iterations) {
		nIterations = iterations;
	}

	public int getIterationCounter() {
		return cIterations;
	}
//	public TigerXMLHeader getHeader() {
//		return header;
//	}
//	
//	public void setHeader(TigerXMLHeader header) {
//		this.header = header;
//	}
}
