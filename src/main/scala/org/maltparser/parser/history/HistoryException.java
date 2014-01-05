package org.maltparser.parser.history;

import org.maltparser.core.exception.MaltChainedException;


/**
 *  HistoryException extends the MaltChainedException class and is thrown by classes
 *  within the history package.
 *
 * @author Johan Hall
 * @since 1.1
**/
public class HistoryException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a HistoryException object with a message
	 * 
	 * @param message	the message
	 */
	public HistoryException(String message) {
		super(message);
	}
	/**
	 * Creates a HistoryException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public HistoryException(String message, Throwable cause) {
		super(message, cause);
	}
}