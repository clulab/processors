package org.maltparser.core.config;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  ConfigurationException extends the MaltChainedException class and is thrown by classes
 *  within the configuration package.
 *
 * @author Johan Hall
**/
public class ConfigurationException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 

	/**
	 * Creates a ConfigurationException object with a message
	 * 
	 * @param message	the message
	 */
	public ConfigurationException(String message) {
		super(message);
	}
	
	/**
	 * Creates a ConfigurationException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public ConfigurationException(String message, Throwable cause) {
		super(message, cause);
	}
}

