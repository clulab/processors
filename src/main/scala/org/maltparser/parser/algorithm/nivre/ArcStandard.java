package org.maltparser.parser.algorithm.nivre;

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
public class ArcStandard extends TransitionSystem {
	protected static final int SHIFT = 1;
	protected static final int RIGHTARC = 2;
	protected static final int LEFTARC = 3;
	
	public ArcStandard() throws MaltChainedException {
		super();
	}
	
	public void apply(GuideUserAction currentAction, ParserConfiguration config) throws MaltChainedException {
		final NivreConfig nivreConfig = (NivreConfig)config;
		final Stack<DependencyNode> stack = nivreConfig.getStack();
		final Stack<DependencyNode> input = nivreConfig.getInput();
		currentAction.getAction(actionContainers);
		Edge e = null;
		switch (transActionContainer.getActionCode()) {
		case LEFTARC:
			e = nivreConfig.getDependencyStructure().addDependencyEdge(input.peek().getIndex(), stack.peek().getIndex());
			addEdgeLabels(e);
			stack.pop();
			break;
		case RIGHTARC:
			e = nivreConfig.getDependencyStructure().addDependencyEdge(stack.peek().getIndex(), input.peek().getIndex());
			addEdgeLabels(e);
			input.pop();
			if (!stack.peek().isRoot()) {
				input.push(stack.pop());	
			}
			break;
		default:
			stack.push(input.pop()); // SHIFT
			break;
		}
	}
	
	public GuideUserAction getDeterministicAction(GuideUserHistory history, ParserConfiguration config) throws MaltChainedException {
		NivreConfig nivreConfig = (NivreConfig)config;
		if (!nivreConfig.isAllowRoot() && nivreConfig.getStack().peek().isRoot()) {
			return updateActionContainers(history, ArcStandard.SHIFT, null);
		}
		return null;
	}
	
	protected void addAvailableTransitionToTable(TransitionTable ttable) throws MaltChainedException {
		ttable.addTransition(SHIFT, "SH", false, null);
		ttable.addTransition(RIGHTARC, "RA", true, null);
		ttable.addTransition(LEFTARC, "LA", true, null);
	}
	
	protected void initWithDefaultTransitions(GuideUserHistory history) throws MaltChainedException {
		GuideUserAction currentAction = new ComplexDecisionAction((History)history);
		
		transActionContainer.setAction(SHIFT);
		for (int i = 0; i < arcLabelActionContainers.length; i++) {
			arcLabelActionContainers[i].setAction(-1);
		}
		currentAction.addAction(actionContainers);
	}
	
	public String getName() {
		return "nivrestandard";
	}
	
	public boolean permissible(GuideUserAction currentAction, ParserConfiguration config) throws MaltChainedException {
		currentAction.getAction(actionContainers);
		int trans = transActionContainer.getActionCode();
		if ((trans == LEFTARC || trans == RIGHTARC) && !isActionContainersLabeled()) {
			return false;
		}
		DependencyNode stackTop = ((NivreConfig)config).getStack().peek();
		if (!((NivreConfig)config).isAllowRoot() && stackTop.isRoot() && trans != SHIFT) {
			return false;
		}
		if (trans == LEFTARC && stackTop.isRoot()) { 
			return false;
		}
		return true;
	}
	
	public GuideUserAction defaultAction(GuideUserHistory history, ParserConfiguration configuration) throws MaltChainedException {
		return updateActionContainers(history, ArcStandard.SHIFT, null);
	}
}
