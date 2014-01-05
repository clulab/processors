package org.maltparser.parser.algorithm.stack;

import java.util.Stack;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.parser.ParserConfiguration;
import org.maltparser.parser.TransitionSystem;
import org.maltparser.parser.history.GuideUserHistory;
import org.maltparser.parser.history.History;
import org.maltparser.parser.history.action.ComplexDecisionAction;
import org.maltparser.parser.history.action.GuideUserAction;
import org.maltparser.parser.transition.TransitionTable;


/**
 * @author Johan Hall
 *
 */
public class NonProjective extends TransitionSystem {
	protected static final int SHIFT = 1;
	protected static final int SWAP = 2;
	protected static final int RIGHTARC = 3;
	protected static final int LEFTARC = 4;

	public NonProjective() throws MaltChainedException {
		super();
	}
	
	public void apply(GuideUserAction currentAction, ParserConfiguration configuration) throws MaltChainedException {
		final StackConfig config = (StackConfig)configuration;
		final Stack<DependencyNode> stack = config.getStack();
		final Stack<DependencyNode> input = config.getInput();
		currentAction.getAction(actionContainers);
		Edge e = null;
		DependencyNode head = null;
		DependencyNode dep = null;
		switch (transActionContainer.getActionCode()) {
		case LEFTARC:
			head = stack.pop(); 
			dep = stack.pop();
			e = config.getDependencyStructure().addDependencyEdge(head.getIndex(), dep.getIndex());
			addEdgeLabels(e);
			stack.push(head);
			break;
		case RIGHTARC:
			dep = stack.pop(); 
			e = config.getDependencyStructure().addDependencyEdge(stack.peek().getIndex(), dep.getIndex());
			addEdgeLabels(e);
			break;
		case SWAP:
			dep = stack.pop();
			input.push(stack.pop());
			stack.push(dep);
			config.lookaheadIncrement();
			break;
		default:
			if (input.isEmpty()) {
				stack.pop();
			} else {
				stack.push(input.pop()); // SHIFT
			}
			config.lookaheadDecrement();
			break;
		}
	}
	
	public boolean permissible(GuideUserAction currentAction, ParserConfiguration configuration) throws MaltChainedException {
		currentAction.getAction(actionContainers);
		int trans = transActionContainer.getActionCode();
		if ((trans == LEFTARC || trans == RIGHTARC) && !isActionContainersLabeled()) {
			return false;
		}
		final StackConfig config = (StackConfig)configuration;
		final Stack<DependencyNode> stack = config.getStack();
		if ((trans == LEFTARC || trans == RIGHTARC || trans == SWAP) && stack.size() < 2) {
			return false;
		}
		if ((trans == LEFTARC || trans == SWAP) && stack.get(stack.size()-2).isRoot()) { 
			return false;
		}
		final Stack<DependencyNode> input = config.getInput();
		if (trans == SHIFT && input.isEmpty()) { 
			return false;
		}
		if (trans == SWAP && stack.get(stack.size()-2).getIndex() > stack.get(stack.size()-1).getIndex()) {
			return false;
		}
		return true;
	}
	
	public GuideUserAction getDeterministicAction(GuideUserHistory history, ParserConfiguration config) throws MaltChainedException {
		return null;
	}
	
	protected void addAvailableTransitionToTable(TransitionTable ttable) throws MaltChainedException {
		ttable.addTransition(SHIFT, "SH", false, null);
		ttable.addTransition(SWAP, "SW", false, null);
		ttable.addTransition(RIGHTARC, "RA", true, null);
		ttable.addTransition(LEFTARC, "LA", true, null);
	}
	
	protected void initWithDefaultTransitions(GuideUserHistory history) throws MaltChainedException {
		GuideUserAction currentAction = new ComplexDecisionAction((History)history);
		
		transActionContainer.setAction(SHIFT);
		//transActionContainer.setAction(SWAP);
		for (int i = 0; i < arcLabelActionContainers.length; i++) {
			arcLabelActionContainers[i].setAction(-1);
		}
		currentAction.addAction(actionContainers);
	}
	
	public String getName() {
		return "nonprojective";
	}
	
	public GuideUserAction defaultAction(GuideUserHistory history, ParserConfiguration configuration) throws MaltChainedException {
		if (((StackConfig)configuration).getInput().isEmpty()) {
			return updateActionContainers(history, RIGHTARC, ((StackConfig)configuration).getDependencyGraph().getDefaultRootEdgeLabels());
		}	
		return updateActionContainers(history, SHIFT, null);
	}
}
