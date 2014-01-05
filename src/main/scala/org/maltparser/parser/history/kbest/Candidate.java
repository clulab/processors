package org.maltparser.parser.history.kbest;


/**
 * A candidate in the k-best list. 
 * 
 * @author Johan Hall
*/
public class Candidate  {
	/**
	 * The integer representation of the predicted action
	 */
	protected int actionCode;
	
	/**
	 * Constructs a candidate object
	 */
	public Candidate() {
		reset();
	}

	/**
	 * Returns an integer representation of the predicted action
	 * 
	 * @return an integer representation of the predicted action
	 */
	public int getActionCode() {
		return actionCode;
	}

	/**
	 * Sets the integer representation of the predicted action
	 * 
	 * @param actionCode an integer representation of the predicted action
	 */
	public void setActionCode(int actionCode) {
		this.actionCode = actionCode;
	}

	/**
	 * Resets the candidate object
	 */
	public void reset() {
		this.actionCode = -1;
	}

	@Override
	public int hashCode() {
		return 31 + actionCode;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return actionCode == ((Candidate)obj).actionCode;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return Integer.toString(actionCode);
	}
}

