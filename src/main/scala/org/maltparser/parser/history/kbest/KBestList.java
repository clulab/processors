package org.maltparser.parser.history.kbest;

import java.util.ArrayList;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.history.action.SingleDecision;
/**
*
* @author Johan Hall
**/
public class KBestList {
	protected final ArrayList<Candidate> kBestList;
	protected int k = -1;
	protected int topCandidateIndex;
	protected int addCandidateIndex;
	protected SingleDecision decision;
	
	/**
	 * Creates a unrestricted k-best list
	 * 
	 * @param decision a reference to the single decision that uses the k-best list
	 */
	public KBestList(SingleDecision decision) {
		this(-1, decision);
	}

	/**
	 * Creates a k-best list
	 * 
	 * @param k	the k-best list size
	 * @param decision	a reference to the single decision that uses the k-best list.
	 */
	public KBestList(Integer k, SingleDecision decision) {
		setK(k.intValue());
		this.decision = decision;
		if (this.k > 0) {
			kBestList = new ArrayList<Candidate>(this.k);
			initKBestList();
		} else {
			kBestList = new ArrayList<Candidate>();
		}
		
	}
	
	protected void initKBestList() {
		for (int i=0; i < this.k; i++) {
			kBestList.add(new Candidate());
		}
	}
	
	/**
	 * Resets the k-best list
	 */
	public void reset() {
		this.topCandidateIndex = 0;
		this.addCandidateIndex = 0;
	}
	
	/**
	 * Adds a candidate to the k-best list
	 * 
	 * @param actionCode the integer representation  of candidate action
	 * @throws MaltChainedException
	 */
	public void add(int actionCode) throws MaltChainedException {
		if (k != -1 && addCandidateIndex >= k) { return; }
		if (addCandidateIndex >= kBestList.size()) { kBestList.add(new Candidate()); }
		kBestList.get(addCandidateIndex).setActionCode(actionCode);
		if (addCandidateIndex == 0) {
//			if (decision instanceof SingleDecision) {
//				((SingleDecision)decision).addDecision(actionCode);
//			}
			decision.addDecision(actionCode);
			topCandidateIndex++;
		}
		addCandidateIndex++;
	}
	
	public void addList(int[] predictionList) throws MaltChainedException {
		int n = (k != -1 && k <= predictionList.length-1)?k:predictionList.length - 1;
		for (int i=0; i<n; i++) {
			add(predictionList[i]);
		}	
	}
	
	/**
	 * Adds a candidate to the k-best list
	 * 
	 * @param symbol the string representation of candidate action
	 * @throws MaltChainedException
	 */
	public void add(String symbol) throws MaltChainedException {
//		if (decision instanceof SingleDecision) {
//			this.add(((SingleDecision)decision).getDecisionCode(symbol));
//		}
		this.add(decision.getDecisionCode(symbol));
	}
	

	/**
	 * Updates the corresponding single decision with the next value in the k-best list.
	 * 
	 * @return true if decision has been updated, otherwise false
	 * @throws MaltChainedException
	 */
	public boolean updateActionWithNextKBest() throws MaltChainedException {
		if (addCandidateIndex != 0 && topCandidateIndex < addCandidateIndex && topCandidateIndex < kBestList.size()) {
			int actionCode = kBestList.get(topCandidateIndex).getActionCode();
			if (decision instanceof SingleDecision) {
				((SingleDecision)decision).addDecision(actionCode);
			}
			topCandidateIndex++;
			return true;
		} 
		return false;
	}
	
	public int peekNextKBest() {
		if (addCandidateIndex != 0 && topCandidateIndex < addCandidateIndex && topCandidateIndex < kBestList.size()) {
			return kBestList.get(topCandidateIndex).getActionCode();
		}
		return -1;
	}
	
	/**
	 * Returns the current size of the k-best list
	 * 
	 * @return the current size of the k-best list
	 */
	public int getCurrentSize() {
		return addCandidateIndex;
		//return kBestList.size();
	}
	
	/**
	 * Returns the maximum number of candidates in the k-best list.
	 * 
	 * @return the maximum number of candidates in the k-best list
	 */
	public int getK() {
		return k;
	}
	/**
	 * Sets the maximum number of candidates in the k-best list
	 * 
	 * @param k the maximum number of candidates 
	 */
	protected void setK(int k) {
		if (k == 0) {
			this.k = 1; // the k-best list must contain at least one candidate
		} if (k < 0) {
			this.k = -1; // this means that the k-best list is unrestricted.
		} else {
			this.k = k;
		}
	}
	
	protected int getTopCandidateIndex() {
		return topCandidateIndex;
	}

	protected int getAddCandidateIndex() {
		return addCandidateIndex;
	}

	/**
	 * Returns a single decision object
	 * 
	 * @return a single decision object
	 */
	public SingleDecision getDecision() {
		return decision;
	}	
	
	public int getKBestListSize() {
		return kBestList.size();
	}
	
	public ScoredCandidate getCandidate(int i) {
		if (i >= kBestList.size()) {
			return null;
		}
		return (ScoredCandidate)kBestList.get(i);
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("[ ");
		for (int i = 0; i < addCandidateIndex; i++) {
			sb.append(kBestList.get(i));
			sb.append(' ');
		}
		sb.append("] ");
		return sb.toString();
	}
}
