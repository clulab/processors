package org.maltparser.parser.history;

import java.util.ArrayList;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.pool.ObjectPoolList;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * 
 * @author Johan Hall
*/
public class HistoryList extends HistoryStructure {
	protected final ArrayList<HistoryNode> list;
	protected final ObjectPoolList<HistoryNode> nodePool;
//	protected BufferedWriter writer;
	
	public HistoryList() throws MaltChainedException {
		super();
		list = new ArrayList<HistoryNode>();
		nodePool = new ObjectPoolList<HistoryNode>() {
			protected HistoryNode create() throws MaltChainedException { return new HistoryListNode(null, null); }
			public void resetObject(HistoryNode o) throws MaltChainedException { o.clear(); }
		};
//		try {
//			writer = new BufferedWriter(new FileWriter("tseq.dat"));
//		} catch (IOException e) {
//			throw new MaltChainedException("", e);
//		}
	}
	
	public HistoryNode getNewHistoryNode(HistoryNode previousNode, GuideUserAction action) throws MaltChainedException {
		HistoryNode node = nodePool.checkOut();
		node.setAction(action);
		node.setPreviousNode(previousNode);
		list.add(node);
		return node;
	}
	
	public void clear() throws MaltChainedException {
		nodePool.checkInAll();
		list.clear();
	}
	
	public boolean equals(Object obj) {
		return super.equals(obj);
	}

	public int hashCode() {
		return super.hashCode();
	}

	public void toFile() throws MaltChainedException {
//		try {
//			for (int i = 0; i < list.size(); i++) {
//				writer.write(((ComplexDecisionAction)list.get(i).getAction()).getSingleDecision(0).getDecisionSymbol());
//				writer.write("\n");
//				writer.flush();
//			}
//			writer.write("\n");
//		} catch (IOException e) {
//			throw new MaltChainedException("", e);
//		}
	}
	
	public void close() throws MaltChainedException {
//		if (writer != null) {
//			try {
//				writer.flush();
//				writer.close();
//				writer = null;
//			} catch (IOException e) {
//				throw new MaltChainedException("", e);
//			}
//		}
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		for (int i = 0; i < list.size(); i++) {
			sb.append(list.get(i));
			if (i < list.size()-1) {
				sb.append(", ");
			}
		}
		return sb.toString();
	}
	
}
