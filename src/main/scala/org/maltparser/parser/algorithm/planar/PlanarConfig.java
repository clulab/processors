package org.maltparser.parser.algorithm.planar;

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
public class PlanarConfig extends ParserConfiguration {
	// Root Handling
	/*
	public static final int STRICT = 1; //root tokens unattached, Reduce not permissible
	public static final int RELAXED = 2; //root tokens unattached, Reduce permissible
	public static final int NORMAL = 3; //root tokens attached to Root with RightArc
	*/
	
	//Connectedness enforcing
	public static final int NO_CONNECTEDNESS = 1;
	public static final int REDUCE_ONLY = 2; //connectedness enforced on reduce only
	public static final int FULL_CONNECTEDNESS = 3; //connectedness enforced on shift and reduce
	
	// Root Handling
	public static final int NORMAL = 1; //root tokens attached to Root with RightArc
	public static final int RELAXED = 2; //root tokens unattached
	
	//Constraints
	public final boolean SINGLE_HEAD = true; //single-head constraint
	public boolean noCoveredRoots = false; //no-covered-roots constraint
	public boolean acyclicity = true; //acyclicity constraint
	public int connectedness = NO_CONNECTEDNESS; //connectedness constraint
	
	
	private Stack<DependencyNode> stack;
	private Stack<DependencyNode> input;
	private DependencyStructure dependencyGraph;
	
	
	//root handling: explicitly create links to dummy root or not?
	private int rootHandling;

	
	public PlanarConfig(SymbolTableHandler symbolTableHandler, String noCoveredRoots , String acyclicity , String connectedness , String rootHandling ) throws MaltChainedException {
		super();
		stack = new Stack<DependencyNode>();
		input = new Stack<DependencyNode>();
		dependencyGraph = new DependencyGraph(symbolTableHandler);
		setRootHandling(rootHandling);
		setNoCoveredRoots(Boolean.valueOf(noCoveredRoots));
		setAcyclicity(Boolean.valueOf(acyclicity));
		setConnectedness(connectedness);
	}
	
	public Stack<DependencyNode> getStack() {
		return stack;
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
	
	public DependencyNode getStackNode(int index) throws MaltChainedException {
		if (index < 0) {
			throw new ParsingException("Stack index must be non-negative in feature specification. ");
		}
		if (stack.size()-index > 0) {
			return stack.get(stack.size()-1-index);
		}
		return null;
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
		for (SymbolTable table : source.getDefaultRootEdgeLabels().keySet()) {
			dependencyGraph.setDefaultRootEdgeLabel(table, source.getDefaultRootEdgeLabelSymbol(table));
		}
	}
	
	public DependencyStructure getDependencyGraph() {
		return dependencyGraph;
	}
	
	public void initialize(ParserConfiguration parserConfiguration) throws MaltChainedException {
		if (parserConfiguration != null) {
			PlanarConfig planarConfig = (PlanarConfig)parserConfiguration;
			Stack<DependencyNode> sourceStack = planarConfig.getStack();
			Stack<DependencyNode> sourceInput = planarConfig.getInput();
			setDependencyGraph(planarConfig.getDependencyGraph());
			for (int i = 0, n = sourceStack.size(); i < n; i++) {
				stack.add(dependencyGraph.getDependencyNode(sourceStack.get(i).getIndex()));
			}
			for (int i = 0, n = sourceInput.size(); i < n; i++) {
				input.add(dependencyGraph.getDependencyNode(sourceInput.get(i).getIndex()));
			}
		} else {
			stack.push(dependencyGraph.getDependencyRoot());
			for (int i = dependencyGraph.getHighestTokenIndex(); i > 0; i--) {
				final DependencyNode node = dependencyGraph.getDependencyNode(i);
				if (node != null && !node.hasHead()) { 
					input.push(node);
				}
			}
		}
	}
	
	/*
	public int getRootHandling() {
		return rootHandling;
	}
	*/
	
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
	
	public boolean requiresConnectednessCheckOnReduce()
	{
		return connectedness != NO_CONNECTEDNESS;
	}
	
	public boolean requiresConnectednessCheckOnShift()
	{
		return connectedness == FULL_CONNECTEDNESS;
	}
	
	public void setNoCoveredRoots ( boolean value ) {noCoveredRoots = value;}
	
	public void setAcyclicity ( boolean value ) {acyclicity = value;}

	protected void setConnectedness(String conn) throws MaltChainedException {
		if (conn.equalsIgnoreCase("none")) {
			connectedness = NO_CONNECTEDNESS;
		} else if (conn.equalsIgnoreCase("reduceonly")) {
			connectedness = REDUCE_ONLY;
		} else if (conn.equalsIgnoreCase("full")) {
			connectedness = FULL_CONNECTEDNESS;
		} else {
			throw new ParsingException("The connectedness constraint option '"+conn+"' is unknown");
		}
	}
	
	/*
	public void setRootHandling(int rootHandling) {
		this.rootHandling = rootHandling;
	}
	
	protected void setRootHandling(String rh) throws MaltChainedException {
		if (rh.equalsIgnoreCase("strict")) {
			rootHandling = STRICT;
		} else if (rh.equalsIgnoreCase("relaxed")) {
			rootHandling = RELAXED;
		} else if (rh.equalsIgnoreCase("normal")) {
			rootHandling = NORMAL;
		} else {
			throw new ParsingException("The root handling '"+rh+"' is unknown");
		}
	}
	*/
	
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
	
	public void clear() throws MaltChainedException {
		dependencyGraph.clear();
		stack.clear();
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
		PlanarConfig that = (PlanarConfig)obj;
		
		if (stack.size() != that.getStack().size()) 
			return false;
		if (input.size() != that.getInput().size())
			return false;
		if (dependencyGraph.nEdges() != that.getDependencyGraph().nEdges())
			return false;
		for (int i = 0; i < stack.size(); i++) {
			if (stack.get(i).getIndex() != that.getStack().get(i).getIndex()) {
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
		sb.append(stack.size());
		sb.append(", ");
		sb.append(input.size());
		sb.append(", ");
		sb.append(dependencyGraph.nEdges());
		return sb.toString();
	}
}
