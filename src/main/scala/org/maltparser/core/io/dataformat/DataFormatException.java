package org.maltparser.core.io.dataformat;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  DataFormatException extends the MaltChainedException class and is thrown by classes
 *  within the dataformat package.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class DataFormatException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	
	/**
	 * Creates a DataFormatException object with a message
	 * 
	 * @param message	the message
	 */
	public DataFormatException(String message) {
		super(message);
	}
	
	/**
	 * Creates a DataFormatException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public DataFormatException(String message, Throwable cause) {
		super(message, cause);
	}
}