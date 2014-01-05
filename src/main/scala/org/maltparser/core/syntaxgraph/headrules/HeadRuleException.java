package org.maltparser.core.syntaxgraph.headrules;

import org.maltparser.core.exception.MaltChainedException;

/**
 *  HeadRuleException extends the MaltChainedException class and is thrown by classes
 *  within the headrule package.
 *
 * @author Johan Hall
**/
public class HeadRuleException extends MaltChainedException {
	public static final long serialVersionUID = 8045568022124816379L; 
	
	/**
	 * Creates a HeadRuleException object with a message
	 * 
	 * @param message	the message
	 */
	public HeadRuleException(String message) {
		super(message);
	}
	
	/**
	 * Creates a HeadRuleException object with a message and a cause to the exception.
	 * 
	 * @param message	the message
	 * @param cause		the cause to the exception
	 */
	public HeadRuleException(String message, Throwable cause) {
		super(message, cause);
	}
}