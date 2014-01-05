package org.maltparser.core.syntaxgraph.writer;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.PatternSyntaxException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.syntaxgraph.PhraseStructure;
import org.maltparser.core.syntaxgraph.TokenStructure;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.NonTerminalNode;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;
/**
*
*
* @author Johan Hall
*/
public class NegraWriter implements SyntaxGraphWriter {
	private BufferedWriter writer; 
	private DataFormatInstance dataFormatInstance;
	private String optionString;
	private int sentenceCount;
	private LinkedHashMap<Integer, Integer> nonTerminalIndexMap;
	private int START_ID_OF_NONTERMINALS = 500;
	private boolean closeStream = true;
	
	public NegraWriter() { 
		nonTerminalIndexMap = new LinkedHashMap<Integer, Integer>();
	}
	
	public void open(String fileName, String charsetName) throws MaltChainedException {
		try {
			open(new OutputStreamWriter(new FileOutputStream(fileName),charsetName));
		} catch (FileNotFoundException e) {
			throw new DataFormatException("The output file '"+fileName+"' cannot be found.", e);
		} catch (UnsupportedEncodingException e) {
			throw new DataFormatException("The character encoding set '"+charsetName+"' isn't supported.", e);
		}	
	}
	
	public void open(OutputStream os, String charsetName) throws MaltChainedException {
		try {
			if (os == System.out || os == System.err) {
				closeStream = false;
			}
			open(new OutputStreamWriter(os, charsetName));
		} catch (UnsupportedEncodingException e) {
			throw new DataFormatException("The character encoding set '"+charsetName+"' isn't supported.", e);
		}
	}
	
	private void open(OutputStreamWriter osw) throws MaltChainedException {
		setWriter(new BufferedWriter(osw));
		setSentenceCount(0);
	}
	
	public void writeProlog() throws MaltChainedException { }
	
	public void writeSentence(TokenStructure syntaxGraph) throws MaltChainedException {
		if (syntaxGraph == null || dataFormatInstance == null || !(syntaxGraph instanceof PhraseStructure) || !syntaxGraph.hasTokens()) {
			return;
		}
		PhraseStructure phraseStructure = (PhraseStructure)syntaxGraph;
		sentenceCount++;
		try {
			writer.write("#BOS ");
			if (phraseStructure.getSentenceID() != 0) {
				writer.write(Integer.toString(phraseStructure.getSentenceID()));
			} else {
				writer.write(Integer.toString(sentenceCount));
			}
			writer.write('\n');

			if (phraseStructure.hasNonTerminals()) {
				calculateIndices(phraseStructure);
				writeTerminals(phraseStructure);
				writeNonTerminals(phraseStructure);
			} else {
				writeTerminals(phraseStructure);
			}
			writer.write("#EOS ");
			if (phraseStructure.getSentenceID() != 0) {
				writer.write(Integer.toString(phraseStructure.getSentenceID()));
			} else {
				writer.write(Integer.toString(sentenceCount));
			}
			writer.write('\n');
		} catch (IOException e) {
			throw new DataFormatException("Could not write to the output file. ", e);
		}
	}
	public void writeEpilog() throws MaltChainedException { }
	

	private void calculateIndices(PhraseStructure phraseStructure) throws MaltChainedException {
		final SortedMap<Integer,Integer> heights = new TreeMap<Integer,Integer>();
		for (int index : phraseStructure.getNonTerminalIndices()) {
			heights.put(index, ((NonTerminalNode)phraseStructure.getNonTerminalNode(index)).getHeight());
		}
		
		boolean done = false;
		int h = 1;
		int ntid = START_ID_OF_NONTERMINALS;
		nonTerminalIndexMap.clear();
		while (!done) {
			done = true;
			for (int index : phraseStructure.getNonTerminalIndices()) {
				if (heights.get(index) == h) {
					NonTerminalNode nt = (NonTerminalNode)phraseStructure.getNonTerminalNode(index);
					nonTerminalIndexMap.put(nt.getIndex(), ntid++);
//					nonTerminalIndexMap.put(nt.getIndex(), nt.getIndex()+START_ID_OF_NONTERMINALS-1);
					done = false;
				}
			}
			h++;
		}
		
//		boolean done = false;
//		int h = 1;
////		int ntid = START_ID_OF_NONTERMINALS;
////		nonTerminalIndexMap.clear();
//		while (!done) {
//			done = true;
//			for (int index : phraseStructure.getNonTerminalIndices()) {
//				if (heights.get(index) == h) {
//					NonTerminalNode nt = (NonTerminalNode)phraseStructure.getNonTerminalNode(index);
////					nonTerminalIndexMap.put(nt.getIndex(), ntid++);
//					nonTerminalIndexMap.put(nt.getIndex(), nt.getIndex()+START_ID_OF_NONTERMINALS-1);
//					done = false;
//				}
//			}
//			h++;
//		}
	}
	
	private void writeTerminals(PhraseStructure phraseStructure) throws MaltChainedException {
		try {
			for (int index : phraseStructure.getTokenIndices()) {
				final PhraseStructureNode terminal = phraseStructure.getTokenNode(index);
				final Iterator<ColumnDescription> columns = dataFormatInstance.iterator();
				ColumnDescription column = null;
				int ti = 1;
				while (columns.hasNext()) {
					column = columns.next();
					if (column.getCategory() == ColumnDescription.INPUT) {
						writer.write(terminal.getLabelSymbol(column.getSymbolTable()));
						int nTabs = 1;
						if (ti == 1 || ti == 2) {
							nTabs = 3 - (terminal.getLabelSymbol(column.getSymbolTable()).length() / 8);
						} else if (ti == 3) {
							nTabs = 1;
						} else if (ti == 4) {
							nTabs = 2 - (terminal.getLabelSymbol(column.getSymbolTable()).length() / 8);
						}
						if (nTabs < 1) {
							nTabs = 1;
						}
						for (int j = 0; j < nTabs; j++) {
							writer.write('\t');
						}
						ti++;
					} else if (column.getCategory() == ColumnDescription.PHRASE_STRUCTURE_EDGE_LABEL) {
						if (terminal.getParent() != null && terminal.hasParentEdgeLabel(column.getSymbolTable())) {
							writer.write(terminal.getParentEdgeLabelSymbol(column.getSymbolTable()));
							writer.write('\t');
						} else {
							writer.write("--\t");
						}
					} else if (column.getCategory() == ColumnDescription.PHRASE_STRUCTURE_NODE_LABEL) { 
						if (terminal.getParent() == null || terminal.getParent() == phraseStructure.getPhraseStructureRoot()) {
							writer.write('0');
						} else {
							writer.write(Integer.toString(nonTerminalIndexMap.get(terminal.getParent().getIndex())));
//							writer.write(Integer.toString(terminal.getParent().getIndex()+START_ID_OF_NONTERMINALS-1));
						}
					}
				}
				for (Edge e : terminal.getIncomingSecondaryEdges()) {
					if (e.hasLabel(column.getSymbolTable())) {
						writer.write('\t');
						writer.write(e.getLabelSymbol(column.getSymbolTable()));
						writer.write('\t');
						if (e.getSource() instanceof NonTerminalNode) {
							writer.write(Integer.toString(nonTerminalIndexMap.get(e.getSource().getIndex())));
//							writer.write(Integer.toString(e.getSource().getIndex()+START_ID_OF_NONTERMINALS-1));
						} else {
							writer.write(Integer.toString(e.getSource().getIndex()));
						}
					}
				}
				writer.write("\n");
			}

		} catch (IOException e) {
			throw new DataFormatException("The Negra writer is not able to write. ", e);
		}
	}
	
	private void writeNonTerminals(PhraseStructure phraseStructure) throws MaltChainedException {
		for (int index : nonTerminalIndexMap.keySet()) {
//		for (int index : phraseStructure.getNonTerminalIndices()) {
			NonTerminalNode nonTerminal = (NonTerminalNode)phraseStructure.getNonTerminalNode(index);
	
			if (nonTerminal == null || nonTerminal.isRoot()) {
				return;
			}
			try {
				writer.write('#');
//				writer.write(Integer.toString(index+START_ID_OF_NONTERMINALS-1));
				writer.write(Integer.toString(nonTerminalIndexMap.get(index)));
				writer.write("\t\t\t--\t\t\t");
				if (nonTerminal.hasLabel(dataFormatInstance.getColumnDescriptionByName("CAT").getSymbolTable())) {
					writer.write(nonTerminal.getLabelSymbol(dataFormatInstance.getColumnDescriptionByName("CAT").getSymbolTable()));
				} else {
					writer.write("--");
				}
				writer.write("\t--\t\t");
				if (nonTerminal.hasParentEdgeLabel(dataFormatInstance.getColumnDescriptionByName("LABEL").getSymbolTable())) {
					writer.write(nonTerminal.getParentEdgeLabelSymbol(dataFormatInstance.getColumnDescriptionByName("LABEL").getSymbolTable()));
				} else {
					writer.write("--");
				}
				writer.write('\t');
				if (nonTerminal.getParent() == null || nonTerminal.getParent().isRoot()) {
					writer.write('0');
				} else {
//					writer.write(Integer.toString(nonTerminal.getParent().getIndex()+START_ID_OF_NONTERMINALS-1));
					writer.write(Integer.toString(nonTerminalIndexMap.get(nonTerminal.getParent().getIndex())));
				}
				for (Edge e : nonTerminal.getIncomingSecondaryEdges()) {
					if (e.hasLabel(dataFormatInstance.getColumnDescriptionByName("SECEDGELABEL").getSymbolTable())) {
						writer.write('\t');
						writer.write(e.getLabelSymbol(dataFormatInstance.getColumnDescriptionByName("SECEDGELABEL").getSymbolTable()));
						writer.write('\t');
						if (e.getSource() instanceof NonTerminalNode) {
//							writer.write(Integer.toString(e.getSource().getIndex()+START_ID_OF_NONTERMINALS-1));
							writer.write(Integer.toString(nonTerminalIndexMap.get(e.getSource().getIndex())));
						} else {
							writer.write(Integer.toString(e.getSource().getIndex()));
						}
					}
				}
				writer.write("\n");
			} catch (IOException e) {
				throw new DataFormatException("The Negra writer is not able to write the non-terminals. ", e);
			}
		}
	}
	
	public BufferedWriter getWriter() {
		return writer;
	}

	public void setWriter(BufferedWriter writer) {
		this.writer = writer;
	}
	
	public int getSentenceCount() {
		return sentenceCount;
	}

	public void setSentenceCount(int sentenceCount) {
		this.sentenceCount = sentenceCount;
	}
	
	public DataFormatInstance getDataFormatInstance() {
		return dataFormatInstance;
	}

	public void setDataFormatInstance(DataFormatInstance dataFormatInstance) {
		this.dataFormatInstance = dataFormatInstance;
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
				throw new DataFormatException("Unknown svm parameter: '"+argv[i-1]+"' with value '"+argv[i]+"'. ");		
			}
		}	
	}
	
	public void close() throws MaltChainedException {
		try {
			if (writer != null) {
				writer.flush();
				if (closeStream) {
					writer.close();
				}
				writer = null;
			}
		}   catch (IOException e) {
			throw new DataFormatException("Could not close the output file. ", e);
		} 
	}
}
