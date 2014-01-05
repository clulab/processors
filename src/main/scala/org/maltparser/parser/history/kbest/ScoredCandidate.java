package org.maltparser.parser.history.kbest;

/**
*
* @author Johan Hall
* @since 1.1
**/
public class ScoredCandidate extends Candidate {
	/**
	 * The candidate score
	 */
	protected float score;
	
	/**
	 * Constructs a candidate object
	 */
	public ScoredCandidate() {
		super();
	}
	
	/**
	 * Returns the score for this candidate if it is available, otherwise Double.NaN
	 * 
	 * @return the score for this candidate if it is available, otherwise Double.NaN
	 */
	public float getScore() {
		return score;
	}

	/**
	 * Sets the score for this candidate.
	 * 
	 * @param score a score
	 */
	public void setScore(Float score) {
		this.score = score;
	}
	
	/**
	 * Resets the candidate object
	 */
	public void reset() {
		super.reset();
		this.score = Float.NaN;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final ScoredCandidate item = (ScoredCandidate)obj;
		return actionCode == item.actionCode && score == item.score;
	}
	
	public int hashCode() {
		return (31 * 7 + actionCode) * 31 + Float.floatToIntBits(score);
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		sb.append('\t');
		sb.append(score);
		return sb.toString();
	}
}
