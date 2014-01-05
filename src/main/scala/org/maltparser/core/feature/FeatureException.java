package org.maltparser.core.feature;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  FeatureException extends the MaltChainedException class and is thrown by classes
 *  within the feature package.
 *
 * @author Johan Hall
**/
public class FeatureException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	/**
	 * Creates a FeatureException object with a message
	 * 
	 * @param message	the message
	 */
	public FeatureException(String message) {
		super(message);
	}
	/**
	 * Creates a FeatureException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public FeatureException(String message, Throwable cause) {
		super(message, cause);
	}
}