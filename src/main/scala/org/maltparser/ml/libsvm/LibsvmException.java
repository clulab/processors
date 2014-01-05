package org.maltparser.ml.libsvm;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  LibsvmException extends the MaltChainedException class and is thrown by classes
 *  within the libsvm package.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class LibsvmException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a LibsvmException object with a message
	 * 
	 * @param message	the message
	 */
	public LibsvmException(String message) {
		super(message);
	}
	/**
	 * Creates a LibsvmException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public LibsvmException(String message, Throwable cause) {
		super(message, cause);
	}
}
