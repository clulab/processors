package org.maltparser.ml.cheater;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  CheaterException extends the MaltChainedException class and is thrown by classes
 *  within the cheater package.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class CheaterException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a CheaterException object with a message
	 * 
	 * @param message	the message
	 */
	public CheaterException(String message) {
		super(message);
	}
	/**
	 * Creates a CheaterException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public CheaterException(String message, Throwable cause) {
		super(message, cause);
	}
}
