package org.maltparser.core.syntaxgraph.writer;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.PatternSyntaxException;

import org.maltparser.core.exception.MaltChainedException;

import org.maltparser.core.helper.Util;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.syntaxgraph.PhraseStructure;
import org.maltparser.core.syntaxgraph.TokenStructure;
import org.maltparser.core.syntaxgraph.node.NonTerminalNode;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;
import org.maltparser.core.syntaxgraph.node.TokenNode;
import org.maltparser.core.syntaxgraph.reader.TigerXMLHeader;
/**
*
*
* @author Johan Hall
*/
public class TigerXMLWriter implements SyntaxGraphWriter {
	private enum RootHandling {
		TALBANKEN, NORMAL  
	};

	private BufferedWriter writer;
	private DataFormatInstance dataFormatInstance;
	private String optionString;
	private int sentenceCount;
	private TigerXMLHeader header;
//	private boolean hasWriteTigerXMLHeader = false;
	private RootHandling rootHandling;
	private String sentencePrefix = "s";
	private StringBuilder sentenceID;
	private StringBuilder tmpID;
	private StringBuilder rootID;
	private int START_ID_OF_NONTERMINALS = 500;
	private boolean labeledTerminalID;
	private String VROOT_SYMBOL = "VROOT";
	private boolean useVROOT = false;
//	private String fileName = null;
//	private String charsetName = null;
	private boolean closeStream = true;
	
	public TigerXMLWriter() { 
		sentenceID = new StringBuilder();
		tmpID = new StringBuilder();
		rootID = new StringBuilder();
		labeledTerminalID = false;
	}
	
	public void open(String fileName, String charsetName) throws MaltChainedException {
		try {
//			this.fileName = fileName;
//			this.charsetName = charsetName;
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
	
	public void writeProlog() throws MaltChainedException { 
//		if (fileName == null || charsetName == null) {
			writeHeader();
//		}
	}
	
	public void writeSentence(TokenStructure syntaxGraph) throws MaltChainedException {
		if (syntaxGraph == null || dataFormatInstance == null) {
			return;
		}
		if (syntaxGraph.hasTokens()) {
			sentenceCount++;
			final PhraseStructure phraseStructure = (PhraseStructure)syntaxGraph;
			try {
				sentenceID.setLength(0);
				sentenceID.append(sentencePrefix);
				if (phraseStructure.getSentenceID() != 0) {
					sentenceID.append(Integer.toString(phraseStructure.getSentenceID()));
				} else {
					sentenceID.append(Integer.toString(sentenceCount));
				}
				writer.write("    <s id=\"");
				writer.write(sentenceID.toString());  
				writer.write("\">\n");
				
				setRootID(phraseStructure);
				writer.write("      <graph root=\"");
				writer.write(rootID.toString());
				writer.write("\" ");
				writer.write("discontinuous=\"");
				writer.write(Boolean.toString(!phraseStructure.isContinuous()));
				writer.write("\">\n");
				
				writeTerminals(phraseStructure);
				if (phraseStructure.nTokenNode() != 1 || rootHandling.equals(RootHandling.TALBANKEN)) {
					writeNonTerminals(phraseStructure);
				} else {
					writer.write("        <nonterminals/>\n");
				}
				writer.write("      </graph>\n");
				writer.write("    </s>\n");
			} catch (IOException e) {
				throw new DataFormatException("The TigerXML writer could not write to file. ", e);
			}
		}
	}
	
	private void setRootID(PhraseStructure phraseStructure) throws MaltChainedException {
		useVROOT = false;
		PhraseStructureNode root = phraseStructure.getPhraseStructureRoot();
		for (ColumnDescription column : dataFormatInstance.getPhraseStructureNodeLabelColumnDescriptionSet()) {
			if (root.hasLabel(column.getSymbolTable()) && root.getLabelSymbol(column.getSymbolTable()).equals(VROOT_SYMBOL)) {
				useVROOT = true;
				break;
			}
		}
		if (useVROOT) {
			rootID.setLength(0);
			rootID.append(sentenceID);
			rootID.append('_');
			rootID.append(VROOT_SYMBOL);
		} else if (phraseStructure.nTokenNode() == 1 && phraseStructure.nNonTerminals() == 0 && !root.isLabeled()) {
			rootID.setLength(0);
			rootID.append(sentenceID);
			rootID.append("_1");
		} else {
			rootID.setLength(0);
			rootID.append(sentenceID);
			rootID.append('_');
//			if (rootHandling.equals(RootHandling.NORMAL)) { 
				rootID.append(Integer.toString(START_ID_OF_NONTERMINALS+phraseStructure.nNonTerminals()));
//			} else if (rootHandling.equals(RootHandling.TALBANKEN)) {
//				rootID.append(Integer.toString(START_ID_OF_NONTERMINALS+1));
//			}
		}

	}
	
	public void writeEpilog() throws MaltChainedException { 
		writeTail();
	}
	
	public BufferedWriter getWriter() {
		return writer;
	}

	public void setWriter(BufferedWriter writer) {
		this.writer = writer;
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
	
	private void writeHeader() throws MaltChainedException {
		try {
			if (header == null) {
				header = new TigerXMLHeader(dataFormatInstance.getSymbolTables());
			}
			writer.write(header.toTigerXML());
//			hasWriteTigerXMLHeader = true;
		} catch (IOException e) {
			throw new DataFormatException("The TigerXML writer could not write to file. ", e);
		}
	}
	
	
	private void writeTerminals(PhraseStructure phraseStructure) throws MaltChainedException {
		try {
			writer.write("        <terminals>\n");
			for (int index : phraseStructure.getTokenIndices()) {
				final PhraseStructureNode t = phraseStructure.getTokenNode(index);
				writer.write("          <t ");
				if (!labeledTerminalID) {
					tmpID.setLength(0);
					tmpID.append(sentenceID);
					tmpID.append('_');
					tmpID.append(Integer.toString(t.getIndex()));
					writer.write("id=\"");writer.write(tmpID.toString());writer.write("\" ");
				}
				
				for (ColumnDescription column : dataFormatInstance.getInputColumnDescriptionSet()) {
					writer.write(column.getName().toLowerCase());
					writer.write("=\"");
					writer.write(Util.xmlEscape(t.getLabelSymbol(column.getSymbolTable())));
					writer.write("\" ");	
				}
				writer.write("/>\n");
			}
			writer.write("        </terminals>\n");
		} catch (IOException e) {
			throw new DataFormatException("The TigerXML writer is not able to write. ", e);
		}
	}
	
	public void writeNonTerminals(PhraseStructure phraseStructure) throws MaltChainedException {
		try {
			SortedMap<Integer,Integer> heights = new TreeMap<Integer,Integer>();
			for (int index : phraseStructure.getNonTerminalIndices()) {
				heights.put(index, ((NonTerminalNode)phraseStructure.getNonTerminalNode(index)).getHeight());
			}
			writer.write("        <nonterminals>\n");
			boolean done = false;
			int h = 1;
			while (!done) {
				done = true;
				for (int index : phraseStructure.getNonTerminalIndices()) {
					if (heights.get(index) == h) {
						NonTerminalNode nt = (NonTerminalNode)phraseStructure.getNonTerminalNode(index);
						tmpID.setLength(0);
						tmpID.append(sentenceID);
						tmpID.append('_');
						tmpID.append(Integer.toString(nt.getIndex()+START_ID_OF_NONTERMINALS-1));
						writeNonTerminal(nt, tmpID.toString());
						done = false;
					}
				}
				h++;
			}
			
			writeNonTerminal((NonTerminalNode)phraseStructure.getPhraseStructureRoot(),rootID.toString());
			writer.write("        </nonterminals>\n");
		} catch (IOException e) {
			throw new DataFormatException("The TigerXML writer is not able to write. ", e);
		}
	}
	
	public void writeNonTerminal(NonTerminalNode nt, String id) throws MaltChainedException {
		try {
			writer.write("          <nt");
			writer.write(" id=\"");writer.write(id);writer.write("\" ");
			for (ColumnDescription column : dataFormatInstance.getPhraseStructureNodeLabelColumnDescriptionSet()) {
				if (nt.hasLabel(column.getSymbolTable())) {
					writer.write(column.getName().toLowerCase());
					writer.write("=");
					writer.write("\"");
					writer.write(Util.xmlEscape(nt.getLabelSymbol(column.getSymbolTable())));
					writer.write("\" ");
				}
			}
			writer.write(">\n");
			
			for (int i = 0, n = nt.nChildren(); i < n; i++) {
				PhraseStructureNode child = nt.getChild(i); 
				writer.write("            <edge ");

				for (ColumnDescription column : dataFormatInstance.getPhraseStructureEdgeLabelColumnDescriptionSet()) {
					if (child.hasParentEdgeLabel(column.getSymbolTable())) {
						writer.write(column.getName().toLowerCase());
						writer.write("=\"");
						writer.write(Util.xmlEscape(child.getParentEdgeLabelSymbol(column.getSymbolTable())));
						writer.write("\" ");
					}
				}
				if (child instanceof TokenNode) {
					if (!labeledTerminalID) {
						tmpID.setLength(0);
						tmpID.append(sentenceID);
						tmpID.append('_');
						tmpID.append(Integer.toString(child.getIndex()));
						writer.write(" idref=\"");writer.write(tmpID.toString());writer.write("\"");
					} else {
						writer.write(" idref=\"");writer.write(child.getLabelSymbol(dataFormatInstance.getInputSymbolTables().get("ID")));writer.write("\"");
					}
					
				} else {
					tmpID.setLength(0);
					tmpID.append(sentenceID);
					tmpID.append('_');
					tmpID.append(Integer.toString(child.getIndex()+START_ID_OF_NONTERMINALS-1));
					writer.write(" idref=\"");writer.write(tmpID.toString());writer.write("\"");
				}
				writer.write(" />\n");
			}
			writer.write("          </nt>\n");
		} catch (IOException e) {
			throw new DataFormatException("The TigerXML writer is not able to write. ", e);
		}
	}

	
	private void writeTail() throws MaltChainedException {
		try {
			writer.write("  </body>\n");
			writer.write("</corpus>\n");
			writer.flush();
//			if (fileName != null && charsetName != null) {
//				writer.close();
//				writer = null;
//				BufferedWriter headerWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName+".header"),charsetName));
//				if (header == null) {
//					header = new TigerXMLHeader(dataFormatInstance.getSymbolTables());
//				}
//				
//				headerWriter.write(header.toTigerXML());
//				headerWriter.flush();
//				headerWriter.close();
//			}
		} catch (IOException e) {
			throw new DataFormatException("The TigerXML writer is not able to write. ", e);
		}
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
		labeledTerminalID = (dataFormatInstance.getInputColumnDescriptions().containsKey("id") || dataFormatInstance.getInputColumnDescriptions().containsKey("ID"));
	}

	public String getOptions() {
		return optionString;
	}
	
	public void setOptions(String optionString) throws MaltChainedException {
		this.optionString = optionString;
		rootHandling = RootHandling.NORMAL;

		String[] argv;
		try {
			argv = optionString.split("[_\\p{Blank}]");
		} catch (PatternSyntaxException e) {
			throw new DataFormatException("Could not split the TigerXML writer option '"+optionString+"'. ", e);
		}
		for (int i=0; i < argv.length-1; i++) {
			if(argv[i].charAt(0) != '-') {
				throw new DataFormatException("The argument flag should start with the following character '-', not with "+argv[i].charAt(0));
			}
			if(++i>=argv.length) {
				throw new DataFormatException("The last argument does not have any value. ");
			}
			switch(argv[i-1].charAt(1)) {
			case 'r': 
				if (argv[i].equals("n")) {
					rootHandling = RootHandling.NORMAL;
				} else if (argv[i].equals("tal")) {
					rootHandling = RootHandling.TALBANKEN;
				}
				break;
			case 's': 
				try {
					START_ID_OF_NONTERMINALS = Integer.parseInt(argv[i]);
				} catch (NumberFormatException e){
					throw new MaltChainedException("The TigerXML writer option -s must be an integer value. ");
				}
				break;
			case 'v': 
				VROOT_SYMBOL = argv[i];
				break;	
			default:
				throw new DataFormatException("Unknown TigerXML writer option: '"+argv[i-1]+"' with value '"+argv[i]+"'. ");		
			}
		}	
	}
}
