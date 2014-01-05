package org.maltparser.core.syntaxgraph.writer;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.io.dataformat.DataFormatException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.core.syntaxgraph.TokenStructure;
import org.maltparser.core.syntaxgraph.node.TokenNode;
/**
*
*
* @author Johan Hall
*/
public class TabWriter implements SyntaxGraphWriter {
	private BufferedWriter writer;
	private DataFormatInstance dataFormatInstance;
	private final StringBuilder output;
	private boolean closeStream = true;
//	private String ID = "ID";
//	private String IGNORE_COLUMN_SIGN = "_";
	private final char TAB = '\t';
	private final char NEWLINE = '\n';

	
	public TabWriter() { 
		output = new StringBuilder();
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
	
	public void writeProlog() throws MaltChainedException {
		
	}
	
	public void writeSentence(TokenStructure syntaxGraph) throws MaltChainedException {
		if (syntaxGraph == null || dataFormatInstance == null || !syntaxGraph.hasTokens()) {
			return;
		}
		Iterator<ColumnDescription> columns = dataFormatInstance.iterator();
		
		for (int i : syntaxGraph.getTokenIndices()) {
			try {
				ColumnDescription column = null;
				while (columns.hasNext()) {
					column = columns.next();

					if (column.getCategory() == ColumnDescription.INPUT) { // && column.getType() != ColumnDescription.IGNORE) {
						TokenNode node = syntaxGraph.getTokenNode(i); 
						if (!column.getName().equals("ID")) {
							if (node.hasLabel(column.getSymbolTable())) {
								output.append(node.getLabelSymbol(column.getSymbolTable()));
								if (output.length() != 0) {
									writer.write(output.toString());
								} else {
									writer.write('_');
								}
							} else {
								writer.write('_');
							}
						} else {
							writer.write(Integer.toString(i));
						}
					} else if (column.getCategory() == ColumnDescription.HEAD /* && column.getType() != ColumnDescription.IGNORE */&& syntaxGraph instanceof DependencyStructure) {
						if (((DependencyStructure)syntaxGraph).getDependencyNode(i).hasHead()) {
							writer.write(Integer.toString(((DependencyStructure)syntaxGraph).getDependencyNode(i).getHead().getIndex()));
						} else {
							writer.write(Integer.toString(0));
						}
						
					} else if (column.getCategory() == ColumnDescription.DEPENDENCY_EDGE_LABEL /* && column.getType() != ColumnDescription.IGNORE */ && syntaxGraph instanceof DependencyStructure) {
						if (((DependencyStructure)syntaxGraph).getDependencyNode(i).hasHead() && ((DependencyStructure)syntaxGraph).getDependencyNode(i).hasHeadEdgeLabel(column.getSymbolTable())) {
							output.append(((DependencyStructure)syntaxGraph).getDependencyNode(i).getHeadEdgeLabelSymbol(column.getSymbolTable()));
						} else {
							output.append(((DependencyStructure)syntaxGraph).getDefaultRootEdgeLabelSymbol(column.getSymbolTable()));
						}
						
						if (output.length() != 0) {
							writer.write(output.toString());
						}
					} else {
						writer.write(column.getDefaultOutput());
					}
					if (columns.hasNext()) {
						writer.write(TAB);
					}
					output.setLength(0);
				}
				writer.write(NEWLINE);
				columns = dataFormatInstance.iterator();
			} catch (IOException e) {
				close();
				throw new DataFormatException("Could not write to the output file. ", e);
			}
		}
		
		try {
			writer.write('\n');
			writer.flush();
		} catch (IOException e) {
			close();
			throw new DataFormatException("Could not write to the output file. ", e);
		}
	}
	
	public void writeEpilog() throws MaltChainedException  {
		
	}
	
	public BufferedWriter getWriter() {
		return writer;
	}

	public void setWriter(BufferedWriter writer) throws MaltChainedException  {
		close();
		this.writer = writer;
	}
	
	public DataFormatInstance getDataFormatInstance() {
		return dataFormatInstance;
	}

	public void setDataFormatInstance(DataFormatInstance dataFormatInstance) {
		this.dataFormatInstance = dataFormatInstance;
	}

	public String getOptions() {
		return null;
	}
	
	public void setOptions(String optionString) throws MaltChainedException {
		
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
