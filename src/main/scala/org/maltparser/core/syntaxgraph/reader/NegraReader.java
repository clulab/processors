package org.maltparser.core.syntaxgraph.reader;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.PatternSyntaxException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.syntaxgraph.MappablePhraseStructureGraph;
import org.maltparser.core.syntaxgraph.PhraseStructure;
import org.maltparser.core.syntaxgraph.TokenStructure;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;

/**
*
*
* @author Johan Hall
*/
public class NegraReader implements SyntaxGraphReader {
	private enum NegraTables {
		ORIGIN, EDITOR, WORDTAG, MORPHTAG, NODETAG, EDGETAG, SECEDGETAG, SENTENCE, UNDEF
	};
	private BufferedReader reader;
	private DataFormatInstance dataFormatInstance;
	private int sentenceCount;
	private String optionString;
	private int formatVersion;
	private NegraTables currentHeaderTable;
	private int currentTerminalSize;
	private int currentNonTerminalSize;
	private SortedMap<Integer,PhraseStructureNode> nonterminals; 
	private StringBuilder edgelabelSymbol;
	private StringBuilder edgelabelTableName;
	private int START_ID_OF_NONTERMINALS = 500;
	private String fileName = null;
	private URL url = null;
	private String charsetName;
	private int nIterations;
	private int cIterations;
	private boolean closeStream = true;
	
	public NegraReader() {
		currentHeaderTable = NegraTables.UNDEF;
		edgelabelSymbol = new StringBuilder();
		edgelabelTableName = new StringBuilder();
		nonterminals = new TreeMap<Integer,PhraseStructureNode>();
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
		} catch (FileNotFoundException e) {
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
		setReader(new BufferedReader(isr));
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
		currentHeaderTable = NegraTables.UNDEF;
		String line = null;
		syntaxGraph.clear();
		nonterminals.clear();
		try {
			while (true) {
				line = reader.readLine();
				if (line == null) {
					if (syntaxGraph.hasTokens()) {
						sentenceCount++;
						if (syntaxGraph instanceof MappablePhraseStructureGraph) {
							((MappablePhraseStructureGraph)syntaxGraph).getMapping().updateDependenyGraph(((MappablePhraseStructureGraph)syntaxGraph), ((PhraseStructure)syntaxGraph).getPhraseStructureRoot());
						}
					}
					if (cIterations < nIterations) {
						cIterations++;
						reopen();
						return true;
					}
					return false;
				} else if (line.startsWith("#EOS")) {
					currentTerminalSize = 0;
					currentNonTerminalSize = 0;
					currentHeaderTable = NegraTables.UNDEF;
					if (syntaxGraph instanceof MappablePhraseStructureGraph) {
						((MappablePhraseStructureGraph)syntaxGraph).getMapping().updateDependenyGraph(((MappablePhraseStructureGraph)syntaxGraph), ((PhraseStructure)syntaxGraph).getPhraseStructureRoot());
					}
					return true;
				} else if (line.startsWith("#BOS")) {
					currentHeaderTable = NegraTables.SENTENCE;
					int s = -1, e = -1;
					for (int i = 5, n = line.length(); i < n; i++) {
						if (Character.isDigit(line.charAt(i)) && s == -1) {
							s = i;
						}
						if (line.charAt(i) == ' ') {
							e = i;
							break;
						}
					}
					if (s != e && s != -1 && e != -1) {
						phraseStructure.setSentenceID(Integer.parseInt(line.substring(s,e)));
					}
					sentenceCount++;
				} else if (currentHeaderTable == NegraTables.SENTENCE) {
					if (line.length() >= 2 && line.charAt(0) == '#' && Character.isDigit(line.charAt(1))) { // Non-terminal
						Iterator<ColumnDescription> columns = dataFormatInstance.iterator();
						ColumnDescription column = null;
						currentNonTerminalSize++;
						char[] lineChars = line.toCharArray();
						int start = 0;
						int secedgecounter = 0;
						for (int i = 0, n = lineChars.length; i < n; i++) {
							if (lineChars[i] == '\t' && start == i) {
								start++;
							} else if (lineChars[i] == '\t' || i == n - 1) {
								if (columns.hasNext()) {
									column = columns.next();
								}
								if (column.getPosition() == 0) {
									int index = Integer.parseInt((i == n - 1)?line.substring(start+1):line.substring(start+1, i));
									child = nonterminals.get(index);
									if (child == null) {
										if (index != 0) {
											child = ((PhraseStructure)syntaxGraph).addNonTerminalNode(index-START_ID_OF_NONTERMINALS+1);
										}
										nonterminals.put(index,child);
									}
								} else if (column.getPosition() == 2 && child != null) {
									syntaxGraph.addLabel(child, "CAT", (i == n - 1)?line.substring(start):line.substring(start, i));
								} else if (column.getCategory() == ColumnDescription.PHRASE_STRUCTURE_EDGE_LABEL) { 
									edgelabelSymbol.setLength(0);
									edgelabelSymbol.append((i == n - 1)?line.substring(start):line.substring(start, i));
									edgelabelTableName.setLength(0);
									edgelabelTableName.append(column.getName());
								} else if (column.getCategory() == ColumnDescription.PHRASE_STRUCTURE_NODE_LABEL && child != null) {
									int index = Integer.parseInt((i == n - 1)?line.substring(start):line.substring(start, i));
									parent = nonterminals.get(index);
									if (parent == null) {
										if (index == 0) {
											parent = phraseStructure.getPhraseStructureRoot();	
										} else {
											parent = phraseStructure.addNonTerminalNode(index-START_ID_OF_NONTERMINALS+1);
										}
										nonterminals.put(index,parent);
									}
									Edge e = phraseStructure.addPhraseStructureEdge(parent, child);
									syntaxGraph.addLabel(e, edgelabelTableName.toString(), edgelabelSymbol.toString());
								} else if (column.getCategory() == ColumnDescription.SECONDARY_EDGE_LABEL && child != null) {
									if (secedgecounter % 2 == 0) {
										edgelabelSymbol.setLength(0);
										edgelabelSymbol.append((i == n - 1)?line.substring(start):line.substring(start, i));
										secedgecounter++;
									} else {
										int index = Integer.parseInt((i == n - 1)?line.substring(start):line.substring(start, i));
										if (index == 0) {
											parent = phraseStructure.getPhraseStructureRoot();
										} else if (index < START_ID_OF_NONTERMINALS) {
											parent = phraseStructure.getTokenNode(index);
										} else {
											parent = nonterminals.get(index);
											if (parent == null) {
												parent = phraseStructure.addNonTerminalNode(index-START_ID_OF_NONTERMINALS+1);
												nonterminals.put(index,parent);
											}
										}
										Edge e = phraseStructure.addSecondaryEdge(parent, child);
										e.addLabel(column.getSymbolTable(), edgelabelSymbol.toString());
										secedgecounter++;
									}
								}
								start = i + 1;
							}
						}
					} else { // Terminal
						Iterator<ColumnDescription> columns = dataFormatInstance.iterator();
						ColumnDescription column = null;
						
						currentTerminalSize++;
						child = syntaxGraph.addTokenNode(currentTerminalSize);
						char[] lineChars = line.toCharArray();
						int start = 0;
						int secedgecounter = 0;
						for (int i = 0, n = lineChars.length; i < n; i++) {
							if (lineChars[i] == '\t' && start == i) {
								start++;
							} else if (lineChars[i] == '\t' || i == n - 1) {
								if (columns.hasNext()) {
									column = columns.next();
								}
								if (column.getCategory() == ColumnDescription.INPUT && child != null) {
									syntaxGraph.addLabel(child, column.getName(), (i == n - 1)?line.substring(start):line.substring(start, i));
								} else if (column.getCategory() == ColumnDescription.PHRASE_STRUCTURE_EDGE_LABEL && child != null) { // && column.getName().equals("EDGELABEL")) {
									edgelabelSymbol.setLength(0);
									edgelabelSymbol.append((i == n - 1)?line.substring(start):line.substring(start, i));
									edgelabelTableName.setLength(0);
									edgelabelTableName.append(column.getName());
								} else if (column.getCategory() == ColumnDescription.PHRASE_STRUCTURE_NODE_LABEL && child != null) {
									int index = Integer.parseInt((i == n - 1)?line.substring(start):line.substring(start, i));
									parent = nonterminals.get(index);
									if (parent == null) {
										if (index == 0) {
											parent = phraseStructure.getPhraseStructureRoot();	
										} else {
											parent = phraseStructure.addNonTerminalNode(index-START_ID_OF_NONTERMINALS+1);
										}
										nonterminals.put(index,parent);
									}

									Edge e = phraseStructure.addPhraseStructureEdge(parent, child);
									syntaxGraph.addLabel(e, edgelabelTableName.toString(), edgelabelSymbol.toString());
								} else if (column.getCategory() == ColumnDescription.SECONDARY_EDGE_LABEL && child != null) {
									if (secedgecounter % 2 == 0) {
										edgelabelSymbol.setLength(0);
										edgelabelSymbol.append((i == n - 1)?line.substring(start):line.substring(start, i));
										secedgecounter++;
									} else {
										int index = Integer.parseInt((i == n - 1)?line.substring(start):line.substring(start, i));
										if (index == 0) {
											parent = phraseStructure.getPhraseStructureRoot();
										} else if (index < START_ID_OF_NONTERMINALS) {
											parent = phraseStructure.getTokenNode(index);
										} else {
											parent = nonterminals.get(index);
											if (parent == null) {
												parent = phraseStructure.addNonTerminalNode(index-START_ID_OF_NONTERMINALS+1);
												nonterminals.put(index,parent);
											}
										}
										Edge e = phraseStructure.addSecondaryEdge(parent, child);
										e.addLabel(column.getSymbolTable(), edgelabelSymbol.toString());
										secedgecounter++;
									}
								}
								start = i + 1;
							}
						}
					}
				} else if (line.startsWith("%%")) { // comment skip
				
				} else if (line.startsWith("#FORMAT")) {
//				int index = line.indexOf(' ');
//				if (index > -1) {
//					try {
//						formatVersion = Integer.parseInt(line.substring(index+1));
//					} catch (NumberFormatException e) {
//						
//					}
//				}
				} else if (line.startsWith("#BOT")) {
//				int index = line.indexOf(' ');
//				if (index > -1) {
//					if (line.substring(index+1).equals("ORIGIN")) {
//						currentHeaderTable = NegraTables.ORIGIN;
//					} else if (line.substring(index+1).equals("EDITOR")) {
//						currentHeaderTable = NegraTables.EDITOR;
//					} else if (line.substring(index+1).equals("WORDTAG")) {
//						currentHeaderTable = NegraTables.WORDTAG;
//					} else if (line.substring(index+1).equals("MORPHTAG")) {
//						currentHeaderTable = NegraTables.MORPHTAG;
//					} else if (line.substring(index+1).equals("NODETAG")) {
//						currentHeaderTable = NegraTables.NODETAG;
//					} else if (line.substring(index+1).equals("EDGETAG")) {
//						currentHeaderTable = NegraTables.EDGETAG;
//					} else if (line.substring(index+1).equals("SECEDGETAG")) {
//						currentHeaderTable = NegraTables.SECEDGETAG;
//					} else {
//						currentHeaderTable = NegraTables.UNDEF;
//					}
//				}
				} else if (line.startsWith("#EOT")) {
					currentHeaderTable = NegraTables.UNDEF;
				}
			}
		}  catch (IOException e) {
			throw new DataFormatException("Error when reading from the input file. ", e);
		}
	}
	
	public void readEpilog() throws MaltChainedException {
		
	}
	
	public BufferedReader getReader() {
		return reader;
	}

	public void setReader(BufferedReader reader) {
		this.reader = reader;
	}

	public int getSentenceCount() {
		return sentenceCount;
	}

	public void setSentenceCount(int sentenceCount) {
		this.sentenceCount = sentenceCount;
	}
	
	public int getFormatVersion() {
		return formatVersion;
	}

	public void setFormatVersion(int formatVersion) {
		this.formatVersion = formatVersion;
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
			throw new DataFormatException("Could not split the penn writer option '"+optionString+"'. ", e);
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
				throw new DataFormatException("Unknown NegraReader parameter: '"+argv[i-1]+"' with value '"+argv[i]+"'. ");		
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
	
	public void close() throws MaltChainedException {
		try {
			if (reader != null) {
				if (closeStream) {
					reader.close();
				}
				reader = null;
			}
		}   catch (IOException e) {
			throw new DataFormatException("Error when closing the input file.", e);
		} 
	}
}
