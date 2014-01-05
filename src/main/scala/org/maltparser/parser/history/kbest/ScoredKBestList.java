package org.maltparser.parser.history.kbest;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.history.action.SingleDecision;
/**
*
* @author Johan Hall
* @since 1.1
**/
public class ScoredKBestList extends KBestList{
	
	public ScoredKBestList(SingleDecision action) {
		this(-1, action);
	}

	public ScoredKBestList(Integer k, SingleDecision action) {
		super(k, action);
	}
	
	protected void initKBestList() {
		for (int i=0; i < this.k; i++) {
			kBestList.add(new ScoredCandidate());
		}
	}
	
	public void add(int actionCode, float score) throws MaltChainedException {
		if (k != -1 && addCandidateIndex >= k) { return; }
		if (addCandidateIndex >= kBestList.size()) { kBestList.add(new ScoredCandidate()); }
		if (!(kBestList.get(addCandidateIndex) instanceof ScoredCandidate)) {
			super.add(actionCode);
			return;
		}
		final ScoredCandidate scand = (ScoredCandidate)kBestList.get(addCandidateIndex);
		scand.setActionCode(actionCode);
		scand.setScore(score);
		if (addCandidateIndex == 0) {
			if (decision instanceof SingleDecision) {
				((SingleDecision)decision).addDecision(actionCode);
			}
			topCandidateIndex++;
		}
		addCandidateIndex++;
	}
	
	public void add(String symbol, float score) throws MaltChainedException {
		if (decision instanceof SingleDecision) {
			this.add(((SingleDecision)decision).getDecisionCode(symbol), score);
		}
	}
	
	public float peekNextKBestScore() {
		if (!(kBestList.get(addCandidateIndex) instanceof ScoredCandidate)) {
			return Float.NaN;
		}
		if (addCandidateIndex != 0 && topCandidateIndex < addCandidateIndex && topCandidateIndex < kBestList.size()) {
			return ((ScoredCandidate)kBestList.get(topCandidateIndex)).getScore();
		}
		return Float.NaN;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return super.toString();
	}
}
