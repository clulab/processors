package org.maltparser.parser;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  ParsingException extends the MaltChainedException class and is thrown by classes
 *  within the parser.algorithm package.
 *
 * @author Johan Hall
**/
public class ParsingException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a ParsingException object with a message
	 * 
	 * @param message	the message
	 */
	public ParsingException(String message) {
		super(message);
	}
	/**
	 * Creates a ParsingException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public ParsingException(String message, Throwable cause) {
		super(message, cause);
	}
}
