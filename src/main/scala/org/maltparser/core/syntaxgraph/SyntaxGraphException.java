package org.maltparser.core.syntaxgraph;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  GraphException extends the MaltChainedException class and is thrown by classes
 *  within the graph package.
 *
 * @author Johan Hall
**/
public class SyntaxGraphException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 

	/**
	 * Creates a GraphException object with a message
	 * 
	 * @param message	the message
	 */
	public SyntaxGraphException(String message) {
		super(message);
	}
	
	/**
	 * Creates a GraphException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public SyntaxGraphException(String message, Throwable cause) {
		super(message, cause);
	}
}