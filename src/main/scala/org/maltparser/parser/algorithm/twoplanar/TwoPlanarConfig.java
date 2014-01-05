package org.maltparser.parser.algorithm.twoplanar;

import java.util.Stack;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.syntaxgraph.DependencyGraph;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.parser.ParserConfiguration;
import org.maltparser.parser.ParsingException;
/**
 * @author Carlos Gomez Rodriguez
 *
 */
public class TwoPlanarConfig extends ParserConfiguration {

	
	//Connectedness enforcing
	/*
	public static final int NO_CONNECTEDNESS = 1;
	public static final int REDUCE_ONLY = 2; //connectedness enforced on reduce only
	public static final int FULL_CONNECTEDNESS = 3; //connectedness enforced on shift and reduce
	*/
	
	// Root Handling
	public static final int NORMAL = 1; //root tokens attached to Root with RightArc
	public static final int RELAXED = 2; //root tokens unattached
	
	//Constraints
	public final boolean SINGLE_HEAD = true; //single-head constraint
	public boolean noCoveredRoots = false; //no-covered-roots constraint
	public boolean acyclicity = true; //acyclicity constraint
	
	//public int connectedness = NO_CONNECTEDNESS; //connectedness constraint
	
	public boolean reduceAfterSwitch = false;
	
	
	private Stack<DependencyNode> firstStack;
	private Stack<DependencyNode> secondStack;

	public static final boolean FIRST_STACK = false;
	public static final boolean SECOND_STACK = true;
	
	private boolean activeStack;
	
	private Stack<DependencyNode> input;
	
	private DependencyStructure dependencyGraph;
	
	//root handling: explicitly create links to dummy root or not?
	private int rootHandling;

	//needed to disallow two consecutive switches:
	private int lastAction;
	
	
	public TwoPlanarConfig(SymbolTableHandler symbolTableHandler, String noCoveredRoots , String acyclicity , String reduceAfterSwitch , String rootHandling) throws MaltChainedException {
		super();
		firstStack = new Stack<DependencyNode>();
		secondStack = new Stack<DependencyNode>();
		activeStack = FIRST_STACK;
		input = new Stack<DependencyNode>();
		dependencyGraph = new DependencyGraph(symbolTableHandler);
		setRootHandling(rootHandling);
		setNoCoveredRoots(Boolean.valueOf(noCoveredRoots));
		setAcyclicity(Boolean.valueOf(acyclicity));
		setReduceAfterSwitch(Boolean.valueOf(reduceAfterSwitch));
	}
	
	public void switchStacks()
	{
		activeStack = !activeStack;
	}
	
	public boolean reduceAfterSwitch ()
	{
		return reduceAfterSwitch;
	}
	
	public void setReduceAfterSwitch ( boolean ras )
	{
		reduceAfterSwitch = ras;
	}
	
	public void setLastAction ( int action )
	{
		lastAction = action;
	}
	
	public int getLastAction ( )
	{
		return lastAction;
	}
	
	public boolean getStackActivityState()
	{
		return activeStack;
	}
	
	private Stack<DependencyNode> getFirstStack() {
		return firstStack;
	}
	
	private Stack<DependencyNode> getSecondStack() {
		return secondStack;
	}
	
	public Stack<DependencyNode> getActiveStack() {
		if ( activeStack == FIRST_STACK ) return getFirstStack();
		else return getSecondStack();
	}
	
	public Stack<DependencyNode> getInactiveStack() {
		if ( activeStack == FIRST_STACK ) return getSecondStack();
		else return getFirstStack();
	}
	
	public Stack<DependencyNode> getInput() {
		return input;
	}
	
	public DependencyStructure getDependencyStructure() {
		return dependencyGraph;
	}
	
	public boolean isTerminalState() {
		return input.isEmpty();
	}
	
	private DependencyNode getStackNode(Stack<DependencyNode> stack , int index) throws MaltChainedException {
		if (index < 0) {
			throw new ParsingException("Stack index must be non-negative in feature specification. ");
		}
		if (stack.size()-index > 0) {
			return stack.get(stack.size()-1-index);
		}
		return null;
	}
	
	public DependencyNode getActiveStackNode ( int index ) throws MaltChainedException {
		return getStackNode ( getActiveStack() , index );
	}
	
	public DependencyNode getInactiveStackNode ( int index ) throws MaltChainedException {
		return getStackNode ( getInactiveStack() , index );
	}
	
	public DependencyNode getInputNode(int index) throws MaltChainedException {
		if (index < 0) {
			throw new ParsingException("Input index must be non-negative in feature specification. ");
		}
		if (input.size()-index > 0) {
			return input.get(input.size()-1-index);
		}	
		return null;
	}
	
	public void setDependencyGraph(DependencyStructure source) throws MaltChainedException {
		dependencyGraph.clear();
		for (int index : source.getTokenIndices()) {
			DependencyNode gnode = source.getTokenNode(index);
			DependencyNode pnode = dependencyGraph.addTokenNode(gnode.getIndex());
			for (SymbolTable table : gnode.getLabelTypes()) {
				pnode.addLabel(table, gnode.getLabelSymbol(table));
			}
			
			if (gnode.hasHead()) {
				Edge s = gnode.getHeadEdge();
				Edge t = dependencyGraph.addDependencyEdge(s.getSource().getIndex(), s.getTarget().getIndex());
				
				for (SymbolTable table : s.getLabelTypes()) {
					t.addLabel(table, s.getLabelSymbol(table));
				}
			}
		}
	}
	
	public DependencyStructure getDependencyGraph() {
		return dependencyGraph;
	}
	
	public void initialize(ParserConfiguration parserConfiguration) throws MaltChainedException {
		if (parserConfiguration != null) {
			TwoPlanarConfig planarConfig = (TwoPlanarConfig)parserConfiguration;
			this.activeStack = planarConfig.activeStack;
			Stack<DependencyNode> sourceActiveStack = planarConfig.getActiveStack();
			Stack<DependencyNode> sourceInactiveStack = planarConfig.getInactiveStack();
			Stack<DependencyNode> sourceInput = planarConfig.getInput();
			setDependencyGraph(planarConfig.getDependencyGraph());
			for (int i = 0, n = sourceActiveStack.size(); i < n; i++) {
				getActiveStack().add(dependencyGraph.getDependencyNode(sourceActiveStack.get(i).getIndex()));
			}
			for ( int i =0, n = sourceInactiveStack.size() ; i < n ; i++ ) {
				getInactiveStack().add(dependencyGraph.getDependencyNode(sourceInactiveStack.get(i).getIndex()));
			}
			for (int i = 0, n = sourceInput.size(); i < n; i++) {
				input.add(dependencyGraph.getDependencyNode(sourceInput.get(i).getIndex()));
			}
		} else {
			getActiveStack().push(dependencyGraph.getDependencyRoot());
			getInactiveStack().push(dependencyGraph.getDependencyRoot());
			for (int i = dependencyGraph.getHighestTokenIndex(); i > 0; i--) {
				final DependencyNode node = dependencyGraph.getDependencyNode(i);
				if (node != null) { 
					input.push(node);
				}
			}
		}
	}
	
	
	public int getRootHandling() {
		return rootHandling;
	}
	
	protected void setRootHandling(String rh) throws MaltChainedException {
		if (rh.equalsIgnoreCase("relaxed")) {
			rootHandling = RELAXED;
		} else if (rh.equalsIgnoreCase("normal")) {
			rootHandling = NORMAL;
		} else {
			throw new ParsingException("The root handling '"+rh+"' is unknown");
		}
	}
	
	
	public boolean requiresSingleHead()
	{
		return SINGLE_HEAD;
	}
	
	public boolean requiresNoCoveredRoots()
	{
		return noCoveredRoots;
	}
	
	public boolean requiresAcyclicity()
	{
		return acyclicity;
	}
	
	//does not make much sense to enforce the no-covered-roots constraint in 2-planar parsing, it won't capture some 2-planar structures
	public void setNoCoveredRoots ( boolean value ) {noCoveredRoots = value;}
	
	public void setAcyclicity ( boolean value ) {acyclicity = value;}	
	
	public void clear() throws MaltChainedException {
		dependencyGraph.clear();
		getActiveStack().clear();
		getInactiveStack().clear();
		input.clear();
		historyNode = null;
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		TwoPlanarConfig that = (TwoPlanarConfig)obj;
		
		if (getActiveStack().size() != that.getActiveStack().size()) 
			return false;
		if (getInactiveStack().size() != that.getInactiveStack().size()) 
			return false;
		if (input.size() != that.getInput().size())
			return false;
		if (dependencyGraph.nEdges() != that.getDependencyGraph().nEdges())
			return false;
		for (int i = 0; i < getActiveStack().size(); i++) {
			if (getActiveStack().get(i).getIndex() != that.getActiveStack().get(i).getIndex()) {
				return false;
			}
		}
		for (int i = 0; i < getInactiveStack().size(); i++) {
			if (getInactiveStack().get(i).getIndex() != that.getInactiveStack().get(i).getIndex()) {
				return false;
			}
		}
		for (int i = 0; i < input.size(); i++) {
			if (input.get(i).getIndex() != that.getInput().get(i).getIndex()) {
				return false;
			}
		}		
		return dependencyGraph.getEdges().equals(that.getDependencyGraph().getEdges());
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(getActiveStack().size());
		sb.append(", ");
		sb.append(getInactiveStack().size());
		sb.append(", ");
		sb.append(input.size());
		sb.append(", ");
		sb.append(dependencyGraph.nEdges());
		return sb.toString();
	}
}
