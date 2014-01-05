package org.maltparser.parser.algorithm.stack;

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
 * @author Johan Hall
 *
 */
public class StackConfig extends ParserConfiguration {
	private Stack<DependencyNode> stack;
	private Stack<DependencyNode> input;
	private DependencyStructure dependencyGraph;
	private int lookahead;
	
	public StackConfig(SymbolTableHandler symbolTableHandler) throws MaltChainedException {
		super();
		stack = new Stack<DependencyNode>();
		input = new Stack<DependencyNode>();
		dependencyGraph = new DependencyGraph(symbolTableHandler);
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
		return input.isEmpty() && stack.size() == 1;
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
	
	private DependencyNode getBufferNode(int index) throws MaltChainedException {
		if (index < 0) {
			throw new ParsingException("Input index must be non-negative in feature specification. ");
		}
		if (input.size()-index > 0) {
			return input.get(input.size()-1-index);
		}	
		return null;
	}
	
	public DependencyNode getLookaheadNode(int index) throws MaltChainedException {	
		return getBufferNode(lookahead+index);
	}
	
	public DependencyNode getInputNode(int index) throws MaltChainedException {
		if (index < lookahead) {
			return getBufferNode(index);
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
	
	public void lookaheadIncrement() {
		lookahead++;
	}
	
	public void lookaheadDecrement() {
		if (lookahead > 0) { 
			lookahead--;	
		}
	}
	
	public DependencyStructure getDependencyGraph() {
		return dependencyGraph;
	}
	
	public void initialize(ParserConfiguration parserConfiguration) throws MaltChainedException {
		if (parserConfiguration != null) {
			StackConfig config = (StackConfig)parserConfiguration;
			Stack<DependencyNode> sourceStack = config.getStack();
			Stack<DependencyNode> sourceInput = config.getInput();
			setDependencyGraph(config.getDependencyGraph());
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
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		StackConfig that = (StackConfig)obj;
		
		if (lookahead != that.lookahead) 
			return false;
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
	
	public void clear() throws MaltChainedException {
		dependencyGraph.clear();
		stack.clear();
		input.clear();
		historyNode = null;
		lookahead = 0;
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
