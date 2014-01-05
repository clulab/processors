package org.maltparser.core.plugin;

import org.maltparser.core.exception.MaltChainedException;
/**
 *  PluginException extends the MaltChainedException class and is thrown by classes
 *  within the plugin package.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class PluginException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 

	/**
	 * Creates a PluginException object with a message
	 * 
	 * @param message	the message
	 */
	public PluginException(String message) {
		super(message);
	}
	/**
	 * Creates a PluginException object with a message
	 * 
	 * @param message	the message
	 */
	public PluginException(String message, Throwable cause) {
		super(message, cause);
	}
}