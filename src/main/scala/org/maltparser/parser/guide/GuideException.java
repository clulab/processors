package org.maltparser.parser.guide;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  GuideException extends the MaltChainedException class and is thrown by classes
 *  within the guide package.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class GuideException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a GuideException object with a message
	 * 
	 * @param message	the message
	 */
	public GuideException(String message) {
		super(message);
	}
	/**
	 * Creates a GuideException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public GuideException(String message, Throwable cause) {
		super(message, cause);
	}
}
