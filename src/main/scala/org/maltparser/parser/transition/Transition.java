package org.maltparser.parser.transition;


/**
 * Transition contains one individual transition. For example, Nivre arc-eager algorithms have the unlabeled 
 * transition <code>SH</code>, <code>RE</code> and the labeled transition<code>RA</code>, <code>LA</code>. These
 * transition will be four individual transition.
 * 
 * @author Joakim Nivre
 * @author Johan Hall
 * @since 1.0
*/
public class Transition implements Comparable<Transition> {
	/**
	 * Transition code
	 */
	private int code;
	/**
	 * Transition symbol
	 */
	private String symbol;
	/**
	 * <code>true</code> if the transition is labeled, otherwise <code>false</code>
	 */
	private boolean labeled;
	private int cachedHash;
	/**
	 * Creates a transition 
	 * 
	 * @param code	Transition code
	 * @param symbol	Transition name
	 * @param labeled	<code>true</code> if the transition is labeled, otherwise <code>false</code>
	 */
	public Transition(int code, String symbol, boolean labeled) {
		this.code = code;
		this.symbol = symbol;
		this.labeled = labeled;
	}

	/**
	 * Returns the transition code
	 * 
	 * @return the transition code
	 */
	public int getCode() {
		return code;
	}
	
	/**
	 * Returns the transition symbol
	 * 
	 * @return	the transition symbol
	 */
	public String getSymbol() {
		return symbol;
	}
	
	/**
	 * Returns true if the transition is labeled, otherwise false
	 * 
	 * @return <code>true</code> if the transition is labeled, otherwise <code>false</code>
	 */
	public boolean isLabeled() {
		return labeled;
	}

	
	public int compareTo(Transition that) {
		final int BEFORE = -1;
	    final int EQUAL = 0;
	    final int AFTER = 1;
//	    if ( this == that ) return EQUAL;
	    if (this.code < that.code) return BEFORE;
	    if (this.code > that.code) return AFTER;
	    return EQUAL;
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Transition t = (Transition)obj;
		return (code == t.code && symbol.equals(t.symbol) && labeled == t.isLabeled());
	}
	
	public int hashCode() {
		if (cachedHash == 0) {
			int hash = 31*7 + code;
			hash = 31*hash + (null == symbol ? 0 : symbol.hashCode());
			hash = 31*hash +  (labeled ? 1 : 0);
			cachedHash = hash;
		}
		return cachedHash;
	}
	
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return symbol + " [" + code +"] " + labeled;
	}
}
