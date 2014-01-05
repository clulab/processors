package org.maltparser.parser.history;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * 
 * @author Johan Hall
*/
public class HistoryListNode implements HistoryNode {
	private HistoryNode previousNode;
	private GuideUserAction action;
//	private double score;
	private int position;
	
	public HistoryListNode(HistoryNode previousNode, GuideUserAction action) {
		setPreviousNode(previousNode);
		setAction(action);
//		setScore(score);
		if (previousNode != null) {
			setPosition(previousNode.getPosition()+1);
		} else {
			setPosition(1);
		}
	}
	
//	public HistoryListNode(HistoryNode previousNode, GuideUserAction action, double score) {
//		setPreviousNode(previousNode);
//		setAction(action);
//		setScore(score);
//		if (previousNode != null) {
//			setPosition(previousNode.getPosition()+1);
//		} else {
//			setPosition(1);
//		}	
//	}
	
	public HistoryNode getPreviousNode() {
		return previousNode;
	}

	public GuideUserAction getAction() {
		return action;
	}

	public void setPreviousNode(HistoryNode node) {
		this.previousNode = node;
	}

	public void setAction(GuideUserAction action) {
		this.action = action;
	}
	
//	public double getScore() {
//		return score;
//	}
//
//	public void setScore(double score) {
//		this.score = score;
//	}

	private void setPosition(int p) {
		position = p;
	}
	
	public int getPosition() {
		return position;
	}
	
	public void clear() throws MaltChainedException {
		setPreviousNode(null);
		setAction(null);
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(action);
		return sb.toString();
	} 
	
}
