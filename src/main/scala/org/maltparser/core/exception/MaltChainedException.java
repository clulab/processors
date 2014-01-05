package org.maltparser.core.exception;

/**
 * MaltChainedException handles a chain of MaltParser specific exception. 
 *
 * @author Johan Hall
**/
public class MaltChainedException extends Exception {
	public static final long serialVersionUID = 8045568022124816379L;
	private Throwable cause = null;

	/**
	 * Creates a MaltChainedException instance
	 */
	public MaltChainedException() {
		super();
	}

	/**
	 * Creates a MaltChainedException instance with a message
	 * 
	 * @param message a message string
	 */
	public MaltChainedException(String message) {
		super(message);
	}

	/**
	 * Creates a MaltChainedException instance with a message and keeps track of the cause of the exception.
	 * 
	 * @param message	a message string
	 * @param cause		a cause
	 */
	public MaltChainedException(String message, Throwable cause) {
		super(message);
		this.cause = cause;
	}
	
	
	/* (non-Javadoc)
	 * @see java.lang.Throwable#getCause()
	 */
	public Throwable getCause() {
		return cause;
	}

	/**
	 * Returns a string representation of the exception chain. Only MaltParser specific exception is included.
	 * 
	 * @return a string representation of the exception chain
	 */
	public String getMessageChain() {
		final StringBuilder sb = new StringBuilder();
	    Throwable t = this;
	    
	    while (t != null) {
	      if (t.getMessage() != null && t instanceof MaltChainedException) {
	    	  sb.append(t.getMessage()+"\n");
	      }
	      t = t.getCause();
	    }  
	    return sb.toString();
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Throwable#printStackTrace()
	 */
	public void printStackTrace() {
		super.printStackTrace();
		if (cause != null) {
			cause.printStackTrace();
		}
	}
}
