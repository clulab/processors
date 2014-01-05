package org.maltparser.core.symbol;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  SymbolException extends the MaltChainedException class and is thrown by classes
 *  within the symbol package.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class SymbolException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a SymbolException object with a message
	 * 
	 * @param message	the message
	 */
	public SymbolException(String message) {
		super(message);
	}
	/**
	 * Creates a SymbolException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public SymbolException(String message, Throwable cause) {
		super(message, cause);
	}
}
