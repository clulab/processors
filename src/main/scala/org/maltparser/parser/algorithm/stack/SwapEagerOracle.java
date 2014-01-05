package org.maltparser.parser.algorithm.stack;

import java.util.ArrayList;
import java.util.Stack;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.Oracle;
import org.maltparser.parser.ParserConfiguration;
import org.maltparser.parser.history.GuideUserHistory;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * @author Johan Hall
 *
 */
public class SwapEagerOracle extends Oracle {
	private ArrayList<Integer> swapArray;
	private boolean swapArrayActive = false;
	
	public SwapEagerOracle(DependencyParserConfig manager, GuideUserHistory history) throws MaltChainedException {
		super(manager, history);
		setGuideName("swapeager");
		swapArray = new ArrayList<Integer>();
	}
	
	public GuideUserAction predict(DependencyStructure gold, ParserConfiguration configuration) throws MaltChainedException {
		StackConfig config = (StackConfig)configuration;
		Stack<DependencyNode> stack = config.getStack();

		if (!swapArrayActive) {
			createSwapArray(gold);
			swapArrayActive = true;
		}
		GuideUserAction action = null;
		if (stack.size() < 2) {
			action = updateActionContainers(NonProjective.SHIFT, null);
		} else {
			DependencyNode left = stack.get(stack.size()-2);
			int leftIndex = left.getIndex();
			int rightIndex = stack.get(stack.size()-1).getIndex();
			if (swapArray.get(leftIndex) > swapArray.get(rightIndex)) {
				action =  updateActionContainers(NonProjective.SWAP, null);
			} else if (!left.isRoot() && gold.getTokenNode(leftIndex).getHead().getIndex() == rightIndex
					&& nodeComplete(gold, config.getDependencyGraph(), leftIndex)) {
				action = updateActionContainers(NonProjective.LEFTARC, gold.getTokenNode(leftIndex).getHeadEdge().getLabelSet());
			} else if (gold.getTokenNode(rightIndex).getHead().getIndex() == leftIndex
					&& nodeComplete(gold, config.getDependencyGraph(), rightIndex)) {
				action = updateActionContainers(NonProjective.RIGHTARC, gold.getTokenNode(rightIndex).getHeadEdge().getLabelSet());
			} else {
				action = updateActionContainers(NonProjective.SHIFT, null);
			}
		}
		return action;
	}
	
	private boolean nodeComplete(DependencyStructure gold, DependencyStructure parseDependencyGraph, int nodeIndex) {
		if (gold.getTokenNode(nodeIndex).hasLeftDependent()) {
			if (!parseDependencyGraph.getTokenNode(nodeIndex).hasLeftDependent()) {
				return false;
			} else if (gold.getTokenNode(nodeIndex).getLeftmostDependent().getIndex() != parseDependencyGraph.getTokenNode(nodeIndex).getLeftmostDependent().getIndex()) {
				return false;
			}
		}
		if (gold.getTokenNode(nodeIndex).hasRightDependent()) {
			if (!parseDependencyGraph.getTokenNode(nodeIndex).hasRightDependent()) {
				return false;
			} else if (gold.getTokenNode(nodeIndex).getRightmostDependent().getIndex() != parseDependencyGraph.getTokenNode(nodeIndex).getRightmostDependent().getIndex()) {
				return false;
			}
		}
		return true;
	}
	
//	private boolean checkRightDependent(DependencyStructure gold, DependencyStructure parseDependencyGraph, int index) throws MaltChainedException {
//		if (gold.getTokenNode(index).getRightmostDependent() == null) {
//			return true;
//		} else if (parseDependencyGraph.getTokenNode(index).getRightmostDependent() != null) {
//			if (gold.getTokenNode(index).getRightmostDependent().getIndex() == parseDependencyGraph.getTokenNode(index).getRightmostDependent().getIndex()) {
//				return true;
//			}
//		}
//		return false;
//	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {
		swapArrayActive = false;
	}
	
	public void terminate() throws MaltChainedException {
	}
	
	private void createSwapArray(DependencyStructure goldDependencyGraph) throws MaltChainedException {
		swapArray.clear();
		for (int i = 0; i <= goldDependencyGraph.getHighestDependencyNodeIndex(); i++) {
			swapArray.add(new Integer(i));
		}
		createSwapArray(goldDependencyGraph.getDependencyRoot(), 0);
	}
	
	private int createSwapArray(DependencyNode n, int order) {
		int o = order; 
		if (n != null) {
			for (int i=0; i < n.getLeftDependentCount(); i++) {
				o = createSwapArray(n.getLeftDependent(i), o);
			}
			swapArray.set(n.getIndex(), o++);
			for (int i=n.getRightDependentCount(); i >= 0; i--) {
				o = createSwapArray(n.getRightDependent(i), o);
			}
		}
		return o;
	}
}
