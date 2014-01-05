package org.maltparser.core.flow;

import org.maltparser.core.exception.MaltChainedException;
/**
 *  FlowException extends the MaltChainedException class and is thrown by classes
 *  within the flow package.
 *
 * @author Johan Hall
**/
public class FlowException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a FlowException object with a message
	 * 
	 * @param message	the message
	 */
	public FlowException(String message) {
		super(message);
	}
	/**
	 * Creates a FlowException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public FlowException(String message, Throwable cause) {
		super(message, cause);
	}
}
