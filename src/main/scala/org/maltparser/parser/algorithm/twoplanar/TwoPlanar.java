package org.maltparser.parser.algorithm.twoplanar;

import java.util.Stack;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
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
 * @author Carlos Gomez Rodriguez
 *
 */
public class TwoPlanar extends TransitionSystem {
	protected static final int SHIFT = 1;
	protected static final int SWITCH = 2;
	protected static final int RIGHTARC = 3;
	protected static final int LEFTARC = 4;
	protected static final int REDUCE = 5;
	protected static final int REDUCEBOTH = 6;
	
	

	
	public TwoPlanar() throws MaltChainedException {
		super();
	}
	
	public void apply(GuideUserAction currentAction, ParserConfiguration config) throws MaltChainedException {
		TwoPlanarConfig planarConfig = (TwoPlanarConfig)config;
		Stack<DependencyNode> activeStack = planarConfig.getActiveStack();
		Stack<DependencyNode> inactiveStack = planarConfig.getInactiveStack();
		Stack<DependencyNode> input = planarConfig.getInput();
		currentAction.getAction(actionContainers);
		Edge e = null;
		int actionCode = transActionContainer.getActionCode();
		switch ( actionCode ) {
		case LEFTARC:
			e = planarConfig.getDependencyStructure().addDependencyEdge(input.peek().getIndex(), activeStack.peek().getIndex());
			addEdgeLabels(e);
			break;
		case RIGHTARC:
			e = planarConfig.getDependencyStructure().addDependencyEdge(activeStack.peek().getIndex(), input.peek().getIndex());
			addEdgeLabels(e);
			break;
		case SWITCH:
			planarConfig.switchStacks();
			if ( planarConfig.reduceAfterSwitch() )
			{
				planarConfig.getActiveStack().pop();
			}
			break;
		case REDUCE:
			activeStack.pop();
			break;
		case REDUCEBOTH:
			activeStack.pop();
			inactiveStack.pop();
			break;
		default: //SHIFT
			DependencyNode n = input.pop();
			activeStack.push(n);
			inactiveStack.push(n);
			break;
		}
		planarConfig.setLastAction(actionCode);
	}
	

	public GuideUserAction getDeterministicAction(GuideUserHistory history, ParserConfiguration config) throws MaltChainedException {
		TwoPlanarConfig theConfig = (TwoPlanarConfig)config;
		if (theConfig.getRootHandling() != TwoPlanarConfig.NORMAL && theConfig.getActiveStack().peek().isRoot()) {
			return updateActionContainers(history, TwoPlanar.SHIFT, null);
		}
		return null;
	}
	
	protected void addAvailableTransitionToTable(TransitionTable ttable) throws MaltChainedException {
		ttable.addTransition(SHIFT, "SH", false, null);
		ttable.addTransition(SWITCH, "SW", false, null);
		ttable.addTransition(REDUCE, "RE", false, null);
		ttable.addTransition(REDUCEBOTH, "RB", false, null);
		ttable.addTransition(RIGHTARC, "RA", true, null);
		ttable.addTransition(LEFTARC, "LA", true, null);
	}
	
	protected void initWithDefaultTransitions(GuideUserHistory history) throws MaltChainedException {
		GuideUserAction currentAction = new ComplexDecisionAction((History)history);
		
		transActionContainer.setAction(SHIFT);
		transActionContainer.setAction(REDUCE);
		transActionContainer.setAction(SWITCH); //TODO it seems like a good idea to do this, but I don't know what it actually does
		transActionContainer.setAction(REDUCEBOTH); //TODO same as above
		for (int i = 0; i < arcLabelActionContainers.length; i++) {
			arcLabelActionContainers[i].setAction(-1);
		}
		currentAction.addAction(actionContainers);
	}
	
	public String getName() {
		return "two-planar arc-eager";
	}

	public boolean permissible(GuideUserAction currentAction, ParserConfiguration config) throws MaltChainedException {
		currentAction.getAction(actionContainers);
		int trans = transActionContainer.getActionCode();
		TwoPlanarConfig planarConfig = (TwoPlanarConfig)config;
		DependencyNode activeStackPeek = planarConfig.getActiveStack().peek();
		DependencyNode inactiveStackPeek = planarConfig.getInactiveStack().peek();
		DependencyNode inputPeek = planarConfig.getInput().peek();
		DependencyStructure dg = planarConfig.getDependencyGraph();
		//int rootHandling = planarConfig.getRootHandling();
		boolean singleHeadConstraint = planarConfig.requiresSingleHead();
		boolean noCoveredRootsConstraint = planarConfig.requiresNoCoveredRoots();
		boolean acyclicityConstraint = planarConfig.requiresAcyclicity();
		//boolean connectednessConstraintOnReduce = planarConfig.requiresConnectednessCheckOnReduce();
		//boolean connectednessConstraintOnShift = planarConfig.requiresConnectednessCheckOnShift();
		if ((trans == LEFTARC || trans == RIGHTARC) && !isActionContainersLabeled()) {
			return false;
		}
		//if ((trans == LEFTARC || trans == REDUCE) && stackPeek.isRoot()) { 
		//	return false;
		//}
		if (trans == LEFTARC) {
			//avoid making root child of something
			if ( activeStackPeek.isRoot() ) 
				return false;
			//enforce single-head constraint if present
			if ( activeStackPeek.hasHead() && singleHeadConstraint ) 
				return false;
			//avoid two links being created from and to the same node
			if ( activeStackPeek.hasHead() && dg.getTokenNode(activeStackPeek.getIndex()).getHead().getIndex() == inputPeek.getIndex() )
				return false;
			//enforce acyclicity constraint if present
			if ( acyclicityConstraint && activeStackPeek.findComponent().getIndex() == inputPeek.findComponent().getIndex() )
				return false;
		}
		if (trans == RIGHTARC) {
			//enforce single-head constraint if present
			if ( inputPeek.hasHead() && singleHeadConstraint )
				return false;
			//avoid two links being created from and to the same node
			if ( inputPeek.hasHead() && dg.getTokenNode(inputPeek.getIndex()).getHead().getIndex() == activeStackPeek.getIndex() )
				return false;
			//enforce acyclicity constraint if present
			if ( acyclicityConstraint && activeStackPeek.findComponent().getIndex() == inputPeek.findComponent().getIndex() )
				return false;
		}
		if (trans == REDUCE) {
			//do not reduce the dummy root
			if ( activeStackPeek.isRoot() ) 
				return false;
			//enforce no-covered-roots constraint if present
			if ( !activeStackPeek.hasHead() && noCoveredRootsConstraint )
				return false;
			//TODO does this line still make sense? (from Nivre arc-eager)
			//if ( !stackPeek.hasHead() && rootHandling == PlanarConfig.STRICT ) 
			//	return false;
			//enforce connectedness constraint if present
			/*
			if ( connectednessConstraintOnReduce )
			{
				boolean path1 = ( stackPeek.findComponent().getIndex() == inputPeek.findComponent().getIndex() );
				boolean path2;
				if ( planarConfig.getStack().size() < 2 ) path2=false;
				else
				{
					DependencyNode stackPrev = planarConfig.getStack().get(planarConfig.getStack().size()-2);
					path2 = stackPrev.findComponent().getIndex() == stackPeek.findComponent().getIndex();
				}
				return path1 || path2;
			}
			*/
		}
		if ( trans == SHIFT )
		{
			/*
			if ( connectednessConstraintOnShift && planarConfig.getInput().size() == 1 ) //last word
			{
				boolean path = ( planarConfig.getDependencyGraph().getTokenNode(1).findComponent().getIndex() == inputPeek.findComponent().getIndex() ); //require connection to 1st
				return path;
			}
			*/
		}
		if (trans == REDUCEBOTH) {
			//do not reduce the dummy root
			if ( activeStackPeek.isRoot() || inactiveStackPeek.isRoot() ) 
				return false;
			//enforce no-covered-roots constraint if present
			if ( (!activeStackPeek.hasHead() || inactiveStackPeek.hasHead()) && noCoveredRootsConstraint )
				return false;
			
			//TODO remove this:
			//not using this transition at the moment, so
			return false;
		}
		if ( trans == SWITCH )
		{
			if ( planarConfig.reduceAfterSwitch() )
			{
				if ( inactiveStackPeek.isRoot() ) 
					return false;
				//enforce no-covered-roots constraint if present
				if ( !inactiveStackPeek.hasHead() && noCoveredRootsConstraint )
					return false;
			}
			else
			{
				if ( planarConfig.getLastAction() == SWITCH ) return false;
			}
		}
		return true;
	}
	
	public GuideUserAction defaultAction(GuideUserHistory history, ParserConfiguration configuration) throws MaltChainedException {
		return updateActionContainers(history, TwoPlanar.SHIFT, null);
	}
	
	
}