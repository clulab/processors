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

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.syntaxgraph.MappablePhraseStructureGraph;
import org.maltparser.core.syntaxgraph.PhraseStructure;
import org.maltparser.core.syntaxgraph.TokenStructure;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;
import org.maltparser.core.syntaxgraph.node.TokenNode;
/**
*
*
* @author Johan Hall
*/
public class BracketReader implements SyntaxGraphReader {
	private BufferedReader reader;
	private DataFormatInstance dataFormatInstance;
	private int sentenceCount;
	private StringBuilder input;
	private int terminalCounter; 
	private int nonTerminalCounter;
	private String optionString;
	private SortedMap<String,ColumnDescription> inputColumns;
	private SortedMap<String,ColumnDescription> edgeLabelColumns;
	private SortedMap<String,ColumnDescription> phraseLabelColumns;
	
	private String fileName = null;
	private URL url = null;
	private String charsetName;
	private int nIterations;
	private int cIterations;
	private boolean closeStream = true;
	
	private char STARTING_BRACKET = '(';
	private char CLOSING_BRACKET = ')';
	private char INPUT_SEPARATOR = ' ';
	private char EDGELABEL_SEPARATOR = '-';
	private char SENTENCE_SEPARATOR = '\n';
	private char BLANK = ' ';
	private char CARRIAGE_RETURN = '\r';
	private char TAB = '\t';
	
	public BracketReader() { 
		input = new StringBuilder();
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
		setReader(new BufferedReader(isr));
		setSentenceCount(0);
	}
	
	public void readProlog() throws MaltChainedException {
		
	}
	
	public boolean readSentence(TokenStructure syntaxGraph) throws MaltChainedException {
		if (syntaxGraph == null || dataFormatInstance == null) {
			return false;
		}
		syntaxGraph.clear();
		int brackets = 0;
		try {
			int l = reader.read();
			char c;
			input.setLength(0);
		
			while (true) {
				if (l == -1) {
					input.setLength(0);
					return false;
				}
				
				c = (char)l; 
				l = reader.read();

				if (c == SENTENCE_SEPARATOR || c == CARRIAGE_RETURN || c == TAB || c == -1) {

				} else if (c == STARTING_BRACKET) {
					input.append(c);
					brackets++;
				} else if (c == CLOSING_BRACKET) {
					input.append(c);
					brackets--;
				} else if (c == INPUT_SEPARATOR) {
					if (l != STARTING_BRACKET && l != CLOSING_BRACKET && l != INPUT_SEPARATOR && l != SENTENCE_SEPARATOR && l != CARRIAGE_RETURN && l != TAB && l != -1) {
						input.append(c);
					}
				// Start BracketProgLangReader
				} else if (c == '\\') {
					c = (char) l;
					l = reader.read();
					if (c != ' ' && c != '(' && c != ')' && c != '\\' && c != 'n' && c != 'r' && c != 't' && c != '\"' && c != '\'') {
//						System.out.println("Error");
						System.exit(1);
					} else {
						input.append("\\" + c);
					}
				// End BracketProgLangReader
				} else if (brackets != 0){
					input.append(c);
				}
				if (brackets == 0 && input.length() != 0) {
					sentenceCount++;
					terminalCounter = 1; 
					nonTerminalCounter = 1;
					if (syntaxGraph instanceof PhraseStructure) {
						bracketing((PhraseStructure)syntaxGraph, 0, input.length(), null);
						if (syntaxGraph instanceof MappablePhraseStructureGraph) {
							((MappablePhraseStructureGraph)syntaxGraph).getMapping().updateDependenyGraph(((MappablePhraseStructureGraph)syntaxGraph), ((PhraseStructure)syntaxGraph).getPhraseStructureRoot());
						}
					}
					return true;
				}
				
				if (c == -1) {
					if (brackets != 0) {
						close();
						throw new MaltChainedException("Error when reading from the input file. ");
					}
					if (cIterations < nIterations) {
						cIterations++;
						reopen();
						return true;
					}
					return false;
				}
			}
		}  catch (IOException e) {
			close();
			throw new MaltChainedException("Error when reading from the input file. ", e);
		} 
		
	}
		
	private void bracketing(PhraseStructure phraseStructure, int start, int end, PhraseStructureNode parent) throws MaltChainedException {
		int bracketsdepth = 0;
		int startpos = start-1;
		for (int i = start, n = end; i < n; i++) {
			if (input.charAt(i) == STARTING_BRACKET
					// Start BracketProgLangReader
					&& (i == 0 || input.charAt(i - 1) != '\\')
					// end BracketProgLangReader
			
			) {
				if (bracketsdepth == 0) {
					startpos = i;
				}
				bracketsdepth++;
			} else if (input.charAt(i) == CLOSING_BRACKET
					// Start BracketProgLangReader
					&& (i == 0 || input.charAt(i - 1) != '\\')
					// end BracketProgLangReader
			) {
				bracketsdepth--;
				if (bracketsdepth == 0) {
					extract(phraseStructure, startpos+1, i, parent);
				}	
			}
		}
	}

	private void extract(PhraseStructure phraseStructure, int begin, int end,  PhraseStructureNode parent) throws MaltChainedException {
		int index = -1;
		for (int i = begin; i < end; i++) {
			if (input.charAt(i) == STARTING_BRACKET
					// Start BracketProgLangReader
					&& (i == begin || input.charAt(i - 1) != '\\')
					// end BracketProgLangReader		
			) {
				index = i;
				break;
			}
		}
		if (index == -1) {
			TokenNode t = phraseStructure.addTokenNode(terminalCounter);
			if (t == null) {
				close();
				throw new MaltChainedException("Bracket Reader error: could not create a terminal node. ");
			}

			terminalCounter++;
			Edge e = null;

			if (parent != null) {
				e = phraseStructure.addPhraseStructureEdge(parent, (PhraseStructureNode)t);
			} else {
				close();
				throw new MaltChainedException("Bracket Reader error: could not find the parent node. ");
			}

			int start = begin;

			Iterator<String> inputColumnsIterator = inputColumns.keySet().iterator();
			Iterator<String> edgeLabelsColumnsIterator = edgeLabelColumns.keySet().iterator();
			boolean noneNode = false;
			boolean edgeLabels = false;
			for (int i = begin; i < end; i++) {
				if (input.charAt(i) == EDGELABEL_SEPARATOR || (input.charAt(i) == INPUT_SEPARATOR 
						// Start BracketProgLangReader
						&& (i == begin || input.charAt(i - 1) != '\\')
						// end BracketProgLangReader	
					) || i == end - 1) {
					if (i == begin && input.charAt(i) == EDGELABEL_SEPARATOR) {
						noneNode = true;
					} else if (start == begin) {
						if ((noneNode && input.charAt(i) != EDGELABEL_SEPARATOR) || !noneNode) {
							if (inputColumnsIterator.hasNext()) { 
								t.addLabel(inputColumns.get(inputColumnsIterator.next()).getSymbolTable(), 
										
										// Start BracketProgLangReader
										decodeString(
										// end BracketProgLangReader
										(i == end - 1)?input.substring(start,end):input.substring(start, i)
										// Start BracketProgLangReader
										)
										// end BracketProgLangReader		
										);
							}
							start = i + 1;
							if (input.charAt(i) == EDGELABEL_SEPARATOR) {
								edgeLabels = true;
							}
						}
					} else if (edgeLabels && e != null) {
						if (edgeLabelsColumnsIterator.hasNext()) { 
							e.addLabel(edgeLabelColumns.get(edgeLabelsColumnsIterator.next()).getSymbolTable(), (i == end - 1)?input.substring(start,end):input.substring(start, i));
						}
						start = i + 1;
						if (input.charAt(i) == INPUT_SEPARATOR
								// Start BracketProgLangReader
								&& (i == begin || input.charAt(i - 1) != '\\')
								// end BracketProgLangReader		
						) {
							edgeLabels = false;
						}
					} else if (input.charAt(i) == EDGELABEL_SEPARATOR && i != end - 1 && (input.charAt(i+1) != INPUT_SEPARATOR
							// Start BracketProgLangReader
							&& (i == begin || input.charAt(i - 1) != '\\')
							// end BracketProgLangReader
							)
					) {	
					} else {
						if (inputColumnsIterator.hasNext()) { 
							t.addLabel(inputColumns.get(inputColumnsIterator.next()).getSymbolTable(), (i == end - 1)?input.substring(start,end):input.substring(start, i));
						}
						start = i + 1;
					}
				}
			}
		} else {
			PhraseStructureNode nt;
			Edge e = null;
			if (parent == null) {
				nt = phraseStructure.getPhraseStructureRoot();
			} else {
				nt = phraseStructure.addNonTerminalNode(nonTerminalCounter);
				if (nt == null) {
					close();
					throw new MaltChainedException("Bracket Reader error: could not create a nonterminal node. ");
				} 
				nonTerminalCounter++;

				e = phraseStructure.addPhraseStructureEdge(parent, nt);
			}
			Iterator<String> phraseLabelColumnsIterator = phraseLabelColumns.keySet().iterator();
			Iterator<String> edgeLabelsColumnsIterator = edgeLabelColumns.keySet().iterator();
			int newbegin = begin;
			int start = begin;
			
			for (int i = begin; i < index; i++) {
				if (input.charAt(i) == EDGELABEL_SEPARATOR || i == index - 1) {
					if (start == newbegin) {
						if (phraseLabelColumnsIterator.hasNext()) { 
							nt.addLabel(phraseLabelColumns.get(phraseLabelColumnsIterator.next()).getSymbolTable(), (i == index - 1)?input.substring(start,index):input.substring(start, i));
						}
						start = i + 1;
					} else if (e != null) {
						if (edgeLabelsColumnsIterator.hasNext()) { 
							e.addLabel(edgeLabelColumns.get(edgeLabelsColumnsIterator.next()).getSymbolTable(), (i == index - 1)?input.substring(start,index):input.substring(start, i));
						}
						start = i + 1;
					}
				} else if (input.charAt(i) == BLANK) {
					start++;
					newbegin++;
				}
			}

			bracketing(phraseStructure, index, end, nt);
		}
	}
	
	private String decodeString(String string) {
		return string.replace("\\(", "(").replace("\\)", ")").replace("\\ ", " ");
	}
	
	public void readEpilog() throws MaltChainedException {
		
	}
	
	public BufferedReader getReader() {
		return reader;
	}

	public void setReader(BufferedReader reader) {
		this.reader = reader;
	}
	
	public int getSentenceCount() throws MaltChainedException {
		return sentenceCount;
	}
	
	public void setSentenceCount(int sentenceCount) {
		this.sentenceCount = sentenceCount;
	}
	
	public DataFormatInstance getDataFormatInstance() {
		return dataFormatInstance;
	}
	
	public void setDataFormatInstance(DataFormatInstance inputDataFormatInstance) {
		this.dataFormatInstance = inputDataFormatInstance;
		inputColumns = dataFormatInstance.getInputColumnDescriptions();
		edgeLabelColumns = dataFormatInstance.getPhraseStructureEdgeLabelColumnDescriptions();
		phraseLabelColumns = dataFormatInstance.getPhraseStructureNodeLabelColumnDescriptions();
	}
	
	public String getOptions() {
		return optionString;
	}
	
	public void setOptions(String optionString) throws MaltChainedException {
		this.optionString = optionString;
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
