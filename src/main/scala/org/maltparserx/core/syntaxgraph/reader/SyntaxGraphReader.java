package org.maltparserx.core.syntaxgraph.reader;

import java.io.InputStream;
import java.net.URL;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.io.dataformat.DataFormatInstance;
import org.maltparserx.core.syntaxgraph.TokenStructure;

/**
*
*
* @author Johan Hall
*/
public interface SyntaxGraphReader {
	/**
	 * Opens a file for read only
	 * 
	 * @param fileName	the file name of the file
	 * @param charsetName	the name of the character encoding set 
	 * @throws MaltChainedException
	 */
	public void open(String fileName, String charsetName) throws MaltChainedException;
	/**
	 * Opens an URL for read only
	 * 
	 * @param url the URL of the resource
	 * @param charsetName the name of the character encoding set 
	 * @throws MaltChainedException
	 */
	public void open(URL url, String charsetName) throws MaltChainedException;
	/**
	 * Opens an input stream
	 * 
	 * @param is an input stream
	 * @param charsetName the name of the character encoding set 
	 * @throws MaltChainedException
	 */
	public void open(InputStream is, String charsetName) throws MaltChainedException;
	/**
	 * Cause the syntax graph reader to read the beginning of the file (such as header information)
	 * 
	 * @throws MaltChainedException
	 */
	public void readProlog() throws MaltChainedException;
	
	/**
	 * Reads a sentence (token structure, dependency structure or/and phrase structure)
	 * 
	 * @param syntaxGraph a syntax graph (token structure, dependency structure or/and phrase structure)
	 * @return true if there is more sentences to be processed, otherwise false.
	 * @throws MaltChainedException
	 */
	public boolean readSentence(TokenStructure syntaxGraph) throws MaltChainedException;
	/**
	 * Reads the end of the file, after all sentences have been processed, 
	 * 
	 * @throws MaltChainedException
	 */
	public void readEpilog() throws MaltChainedException;
	/**
	 * Returns the current number of the sentence.
	 * 
	 * @return the current number of the sentence.
	 * @throws MaltChainedException
	 */
	public int getSentenceCount() throws MaltChainedException;
	/**
	 * Returns the input data format instance
	 * 
	 * @return the input data format instance
	 */
	public DataFormatInstance getDataFormatInstance();
	/**
	 * Sets the input data format instance
	 * 
	 * @param dataFormatInstance an input data format instance
	 */
	public void setDataFormatInstance(DataFormatInstance dataFormatInstance);
	/**
	 * Returns a string representation of the reader specific options.
	 * 
	 * @return a string representation of the reader specific options.
	 */
	public String getOptions();
	/**
	 * Sets the reader specific options.
	 * 
	 * @param optionString a string representation of the reader specific options
	 * @throws MaltChainedException
	 */
	public void setOptions(String optionString) throws MaltChainedException;
	/**
	 * Closes the file or the input stream.
	 * 
	 * @throws MaltChainedException
	 */
	public void close() throws MaltChainedException;
	
	public int getNIterations();
	public void setNIterations(int iterations);
	public int getIterationCounter();
}
