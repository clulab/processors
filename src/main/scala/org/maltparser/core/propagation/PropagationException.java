package org.maltparser.core.propagation;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  PropagationException extends the MaltChainedException class and is thrown by classes
 *  within the propagation package.
 *
 * @author Johan Hall
**/
public class PropagationException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a PropagationException object with a message
	 * 
	 * @param message	the message
	 */
	public PropagationException(String message) {
		super(message);
	}
	/**
	 * Creates a PropagationException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public PropagationException(String message, Throwable cause) {
		super(message, cause);
	}
}