package org.maltparser.parser.history;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.pool.ObjectPoolList;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * 
 * @author Johan Hall
*/
public class HistoryTree extends HistoryStructure {
	private HistoryTreeNode root;
	protected final ObjectPoolList<HistoryNode> nodePool;
	
	public HistoryTree() {
		super();
		nodePool = new ObjectPoolList<HistoryNode>() {
			protected HistoryNode create() throws MaltChainedException { return new HistoryTreeNode(null, null); }
			public void resetObject(HistoryNode o) throws MaltChainedException { o.clear(); }
		};
		root = new HistoryTreeNode(null,null);
	}
	
	public HistoryNode getNewHistoryNode(HistoryNode previousNode, GuideUserAction action) throws MaltChainedException {
		HistoryNode node = nodePool.checkOut();
		node.setAction(action);
		if (previousNode == null) {
			node.setPreviousNode(root);
		} else {
			node.setPreviousNode(previousNode);
		}
		return node;
	}
	
	public void clear() throws MaltChainedException {
		nodePool.checkInAll();
		root.clear();
	}
	
	public void toFile() throws MaltChainedException {
		
	}
	
	public void close() throws MaltChainedException {
		
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(root.toString());
		return sb.toString();
	}
}
