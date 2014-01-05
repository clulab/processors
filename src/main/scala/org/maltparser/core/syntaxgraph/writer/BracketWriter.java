package org.maltparser.core.syntaxgraph.writer;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.SortedMap;
import java.util.regex.PatternSyntaxException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.PhraseStructure;
import org.maltparser.core.syntaxgraph.TokenStructure;
import org.maltparser.core.syntaxgraph.node.NonTerminalNode;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;
import org.maltparser.core.syntaxgraph.node.TokenNode;
/**
*
*
* @author Johan Hall
*/
public class BracketWriter implements SyntaxGraphWriter {
	private enum PennWriterFormat {
		DEFAULT, PRETTY
	};
	private PennWriterFormat format;
	private BufferedWriter writer;
	private DataFormatInstance dataFormatInstance;
	private SortedMap<String,ColumnDescription> inputColumns;
	private SortedMap<String,ColumnDescription> edgeLabelColumns;
	private SortedMap<String,ColumnDescription> phraseLabelColumns;
	private char STARTING_BRACKET = '(';
	private String EMPTY_EDGELABEL = "??";
	private char CLOSING_BRACKET = ')';
	private char INPUT_SEPARATOR = ' ';
	private char EDGELABEL_SEPARATOR = '-';
	private char SENTENCE_SEPARATOR = '\n';
	private String optionString;
	private boolean closeStream = true;
	
	public BracketWriter() { 
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
	}

	public void writeEpilog() throws MaltChainedException {

	}
	
	public void writeProlog() throws MaltChainedException {
	
	}
	
	public void writeSentence(TokenStructure syntaxGraph) throws MaltChainedException {
		if (syntaxGraph == null || dataFormatInstance == null) {
			return;
		}
		if (syntaxGraph instanceof PhraseStructure && syntaxGraph.hasTokens()) {
//			PhraseStructure phraseStructure = ((PhraseStructure) syntaxGraph);
			if (format == PennWriterFormat.PRETTY) {
				writeElement(((PhraseStructure) syntaxGraph).getPhraseStructureRoot(), 0);
			} else {
				writeElement(((PhraseStructure) syntaxGraph).getPhraseStructureRoot());
			}
			try {
				writer.write(SENTENCE_SEPARATOR);
				writer.flush();
			} catch (IOException e) {
				close();
				throw new DataFormatException("Could not write to the output file. ", e);
			}
		}
	}
	
	private void writeElement(PhraseStructureNode element) throws MaltChainedException {
		try {
			if (element instanceof TokenNode) {
				PhraseStructureNode t = (PhraseStructureNode)element;
				SymbolTable table = null;
				writer.write(STARTING_BRACKET);
				int i = 0;
				for (String inputColumn : inputColumns.keySet()) {
					if (i != 0) {
						writer.write(INPUT_SEPARATOR);
					}
					table = inputColumns.get(inputColumn).getSymbolTable();
					if (t.hasLabel(table)) {
						writer.write(t.getLabelSymbol(table));
					}
					if (i == 0) {
						for (String edgeLabelColumn : edgeLabelColumns.keySet()) {
							table = edgeLabelColumns.get(edgeLabelColumn).getSymbolTable();
							if (t.hasParentEdgeLabel(table) && !t.getParent().isRoot() && !t.getParentEdgeLabelSymbol(table).equals(EMPTY_EDGELABEL)) {
								writer.write(EDGELABEL_SEPARATOR);
								writer.write(t.getParentEdgeLabelSymbol(table));
							}
						}
					}
					i++;
				}
				writer.write(CLOSING_BRACKET);
			} else {
				NonTerminalNode nt = (NonTerminalNode)element;
				writer.write(STARTING_BRACKET);
				SymbolTable table = null;
				int i = 0;
				for (String phraseLabelColumn : phraseLabelColumns.keySet()) {
					if (i != 0) {
						writer.write(INPUT_SEPARATOR);
					}
					table = phraseLabelColumns.get(phraseLabelColumn).getSymbolTable();
					if (nt.hasLabel(table)) { 
						writer.write(nt.getLabelSymbol(table));
					}
					if (i == 0) {
						for (String edgeLabelColumn : edgeLabelColumns.keySet()) {
							table = edgeLabelColumns.get(edgeLabelColumn).getSymbolTable();
							if (nt.hasParentEdgeLabel(table) && !nt.getParent().isRoot() && !nt.getParentEdgeLabelSymbol(table).equals(EMPTY_EDGELABEL)) {
								writer.write(EDGELABEL_SEPARATOR);
								writer.write(nt.getParentEdgeLabelSymbol(table));
							}
						}
					}
					i++;
				}
				for (PhraseStructureNode node : ((NonTerminalNode)element).getChildren()) {
					writeElement(node);
				}
				writer.write(CLOSING_BRACKET);
			}
		} catch (IOException e) {
			throw new DataFormatException("Could not write to the output file. ", e);
		}
	}
	
	private String getIndentation(int depth) {
		StringBuilder sb = new StringBuilder("");
		for (int i = 0; i < depth; i++) {
			sb.append("\t");
		}
		return sb.toString();
	}
	
	private void writeElement(PhraseStructureNode element, int depth) throws MaltChainedException {
		try {
			if (element instanceof TokenNode) {
				PhraseStructureNode t = (PhraseStructureNode)element;
				SymbolTable table = null;
				writer.write("\n" + getIndentation(depth) + STARTING_BRACKET);
				int i = 0;
				for (String inputColumn : inputColumns.keySet()) {
					if (i != 0) {
						writer.write(INPUT_SEPARATOR);
					}
					table = inputColumns.get(inputColumn).getSymbolTable();
					if (t.hasLabel(table)) {
						writer.write(encodeString(t.getLabelSymbol(table)));
					}
					if (i == 0) {
						for (String edgeLabelColumn : edgeLabelColumns.keySet()) {
							table = edgeLabelColumns.get(edgeLabelColumn).getSymbolTable();
							if (t.hasParentEdgeLabel(table) && !t.getParent().isRoot() && !t.getParentEdgeLabelSymbol(table).equals(EMPTY_EDGELABEL)) {
								writer.write(EDGELABEL_SEPARATOR);
								writer.write(t.getParentEdgeLabelSymbol(table));
							}
						}
					}
					i++;
				}
				writer.write(CLOSING_BRACKET);
			} else {
				NonTerminalNode nt = (NonTerminalNode)element;
				writer.write("\n" + getIndentation(depth) + STARTING_BRACKET);
				SymbolTable table = null;
				int i = 0;
				for (String phraseLabelColumn : phraseLabelColumns.keySet()) {
					if (i != 0) {
						writer.write(INPUT_SEPARATOR);
					}
					table = phraseLabelColumns.get(phraseLabelColumn).getSymbolTable();
					if (nt.hasLabel(table)) { 
						writer.write(nt.getLabelSymbol(table));
					}
					if (i == 0) {
						for (String edgeLabelColumn : edgeLabelColumns.keySet()) {
							table = edgeLabelColumns.get(edgeLabelColumn).getSymbolTable();
							if (nt.hasParentEdgeLabel(table) && !nt.getParent().isRoot() && !nt.getParentEdgeLabelSymbol(table).equals(EMPTY_EDGELABEL)) {
								writer.write(EDGELABEL_SEPARATOR);
								writer.write(nt.getParentEdgeLabelSymbol(table));
							}
						}
					}
					i++;
				}
				for (PhraseStructureNode node : ((NonTerminalNode)element).getChildren()) {
					writeElement(node, depth + 1);
				}
				writer.write("\n" + getIndentation(depth) + CLOSING_BRACKET);
			}
		} catch (IOException e) {
			throw new DataFormatException("Could not write to the output file. ", e);
		}
	}
	
	public BufferedWriter getWriter() {
		return writer;
	}

	public void setWriter(BufferedWriter writer) throws MaltChainedException {
		close();
		this.writer = writer;
	}
	
	public DataFormatInstance getDataFormatInstance() {
		return dataFormatInstance;
	}

	public void setDataFormatInstance(DataFormatInstance dataFormatInstance) {
		this.dataFormatInstance = dataFormatInstance;
		inputColumns = dataFormatInstance.getInputColumnDescriptions();
		edgeLabelColumns = dataFormatInstance.getPhraseStructureEdgeLabelColumnDescriptions();
		phraseLabelColumns = dataFormatInstance.getPhraseStructureNodeLabelColumnDescriptions();
	}

	public String getOptions() {
		return optionString;
	}
	
	public void setOptions(String optionString) throws MaltChainedException {
		this.optionString = optionString;
		format = PennWriterFormat.DEFAULT;

		String[] argv;
		try {
			argv = optionString.split("[_\\p{Blank}]");
		} catch (PatternSyntaxException e) {
			throw new DataFormatException("Could not split the bracket writer option '"+optionString+"'. ", e);
		}
		for (int i=0; i < argv.length-1; i++) {
			if(argv[i].charAt(0) != '-') {
				throw new DataFormatException("The argument flag should start with the following character '-', not with "+argv[i].charAt(0));
			}
			if(++i>=argv.length) {
				throw new DataFormatException("The last argument does not have any value. ");
			}
			switch(argv[i-1].charAt(1)) {
			case 'f': 
				if (argv[i].equals("p")) {
					format = PennWriterFormat.PRETTY;
				} else if (argv[i].equals("p")) {
					format = PennWriterFormat.DEFAULT;
				}
				break;
			default:
				throw new DataFormatException("Unknown bracket writer option: '"+argv[i-1]+"' with value '"+argv[i]+"'. ");		
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
	
	private String encodeString(String string) {
		return string.replace("(", "-LRB-").replace(")", "-RRB-").replace("[", "-LSB-").replace("]", "-RSB-").replace("{", "-LCB-").replace("}", "-RCB-");
	}
}
