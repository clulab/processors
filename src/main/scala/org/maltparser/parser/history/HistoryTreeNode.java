package org.maltparser.parser.history;

import java.util.ArrayList;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.history.action.ActionDecision;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * 
 * @author Johan Hall
*/
public class HistoryTreeNode implements HistoryNode {
	private GuideUserAction action;
	private HistoryTreeNode parent;
	private int depth;
	private ArrayList<HistoryTreeNode> children;
//	private double score;
	
	public HistoryTreeNode(HistoryNode previousNode, GuideUserAction action) {
		setPreviousNode(parent);
		setAction(action);
		children = new ArrayList<HistoryTreeNode>();
	}
	
//	public HistoryTreeNode(HistoryNode previousNode, GuideUserAction action, double score) {
//		setPreviousNode(parent);
//		setAction(action);
//		setScore(score);
//		children = new ArrayList<HistoryTreeNode>();
//	}
	
	public GuideUserAction getAction() {
		return action;
	}

	public void setAction(GuideUserAction action) {
		this.action = action;
	}

	public HistoryNode getPreviousNode() {
		return parent;
	}
	
	public void setPreviousNode(HistoryNode node) {
		if (node instanceof HistoryTreeNode) {
			this.parent = (HistoryTreeNode)node;
			parent.addChild(this);
			setDepth(parent.getDepth()+1);
		}
	}
	
	public int getDepth() {
		return depth;
	}

	public void setDepth(int depth) {
		this.depth = depth;
	}

	public void addChild(HistoryTreeNode child) {
		children.add(child);
	}
	
	public void removeChild(HistoryTreeNode child) {
		children.remove(child);
	}
	
	public HistoryTreeNode getChild(ActionDecision childDecision) {
		for (HistoryTreeNode c : children) {
			if (c.getAction().equals(childDecision)) {
				return c;
			}
		}
		return null;
	}
	
//	public double getScore() {
//		return score;
//	}
//
//	public void setScore(double score) {
//		this.score = score;
//	}
	
	public int getPosition() {
		return depth;
	}
	
	public void clear() throws MaltChainedException {
		if (parent != null) {
			parent.removeChild(this);
		}
		setAction(null);
		setPreviousNode(null);
		children.clear();
	}
	
	public boolean equals(Object obj) {
		return super.equals(obj);
	}

	public int hashCode() {
		return super.hashCode();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		for (int i = 0; i <= depth; i++) {
			sb.append("  ");
		}
		sb.append(action);
		sb.append('\n');
		for (int i = 0; i < children.size(); i++) {
			sb.append(children.get(i));
		}
		return sb.toString();
	}
	
}
