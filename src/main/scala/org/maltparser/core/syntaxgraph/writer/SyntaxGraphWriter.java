package org.maltparser.core.syntaxgraph.writer;

import java.io.OutputStream;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.syntaxgraph.TokenStructure;
/**
*
*
* @author Johan Hall
*/
public interface SyntaxGraphWriter {
	/**
	 * Opens a file for writing
	 * 
	 * @param fileName	the file name of the file
	 * @param charsetName	the name of the character encoding set 
	 * @throws MaltChainedException
	 */
	public void open(String fileName, String charsetName) throws MaltChainedException;
	/**
	 * Opens an output stream
	 * 
	 * @param os an output stream
	 * @param charsetName the name of the character encoding set 
	 * @throws MaltChainedException
	 */
	public void open(OutputStream os, String charsetName) throws MaltChainedException;
	/**
	 * Cause the syntax graph writer to write the beginning of the file (such as header information)
	 * 
	 * @throws MaltChainedException
	 */
	public void writeProlog() throws MaltChainedException;
	/**
	 * Writes a sentence (token structure, dependency structure or/and phrase structure)
	 * 
	 * @param syntaxGraph a syntax graph (token structure, dependency structure or/and phrase structure)
	 * @throws MaltChainedException
	 */
	public void writeSentence(TokenStructure syntaxGraph) throws MaltChainedException;
	/**
	 * Writes the end of the file 
	 * 
	 * @throws MaltChainedException
	 */
	public void writeEpilog() throws MaltChainedException;
	/**
	 * Returns the output data format instance
	 * 
	 * @return the output data format instance
	 */
	public DataFormatInstance getDataFormatInstance();
	/**
	 * Sets the output data format instance
	 * 
	 * @param dataFormatInstance an output data format instance
	 */
	public void setDataFormatInstance(DataFormatInstance dataFormatInstance);
	/**
	 * Returns a string representation of the writer specific options.
	 * 
	 * @return a string representation of the writer specific options.
	 */
	public String getOptions();
	/**
	 * Sets the writer specific options.
	 * 
	 * @param optionString a string representation of the writer specific options
	 * @throws MaltChainedException
	 */
	public void setOptions(String optionString) throws MaltChainedException;
	/**
	 * Closes the file or the output stream.
	 * 
	 * @throws MaltChainedException
	 */
	public void close() throws MaltChainedException;
}
