package org.maltparser.ml.liblinear;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  LiblinearException extends the MaltChainedException class and is thrown by classes
 *  within the libsvm package.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class LiblinearException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a LiblinearException object with a message
	 * 
	 * @param message	the message
	 */
	public LiblinearException(String message) {
		super(message);
	}
	/**
	 * Creates a LiblinearException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public LiblinearException(String message, Throwable cause) {
		super(message, cause);
	}
}
