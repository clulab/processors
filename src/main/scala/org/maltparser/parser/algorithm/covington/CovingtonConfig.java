package org.maltparser.parser.algorithm.covington;

import java.util.ArrayList;

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
public class CovingtonConfig extends ParserConfiguration {
	private ArrayList<DependencyNode> input;
	private int right;
	private int left;
	private int leftstop;
	private int rightstop;
	private DependencyStructure dependencyGraph;
	private boolean allowRoot;
	private boolean allowShift;

	
	public CovingtonConfig(SymbolTableHandler symbolTableHandler, boolean cr, boolean cs) throws MaltChainedException {
		super();
		input = new ArrayList<DependencyNode>();
		dependencyGraph = new DependencyGraph(symbolTableHandler);
		setAllowRoot(cr);
		setAllowShift(cs);
	}

	public DependencyStructure getDependencyStructure() {
		return dependencyGraph;
	}
	
	public ArrayList<DependencyNode> getInput() {
		return input;
	}

	public boolean isTerminalState() {
		return right > rightstop;
	}
	
	public int getRight() {
		return right;
	}
	
	public void setRight(int right) {
		this.right = right;
	}

	public int getLeft() {
		return left;
	}

	public void setLeft(int left) {
		this.left = left;
	}

	public int getLeftstop() {
		return leftstop;
	}

	public int getRightstop() {
		return rightstop;
	}

	public boolean isAllowRoot() {
		return allowRoot;
	}

	public void setAllowRoot(boolean allowRoot) {
		this.allowRoot = allowRoot;
	}

	public boolean isAllowShift() {
		return allowShift;
	}

	public void setAllowShift(boolean allowShift) {
		this.allowShift = allowShift;
	}

	public DependencyNode getLeftNode(int index) throws MaltChainedException {
		if (index < 0) {
			throw new ParsingException("Left index must be non-negative in feature specification. ");
		}
		if (left-index >= 0) {
			return input.get(left-index);
		}
		return null;
	}
	
	public DependencyNode getRightNode(int index) throws MaltChainedException {
		if (index < 0) {
			throw new ParsingException("Right index must be non-negative in feature specification. ");
		}
		if (right+index < input.size()) {
			return input.get(right+index);
		}
		return null;
	}
	
	public DependencyNode getLeftContextNode(int index) throws MaltChainedException {
		if (index < 0) {
			throw new ParsingException("LeftContext index must be non-negative in feature specification. ");
		}
		
		int tmpindex = 0;
		for (int i = left+1; i < right; i++) {
			if (!input.get(i).hasAncestorInside(left, right)) {
				if (tmpindex == index) {
					return input.get(i);
				} else {
					tmpindex++;
				}
			}
		}
		return null;
	}
	
	public DependencyNode getRightContextNode(int index) throws MaltChainedException {
		if (index < 0) {
			throw new ParsingException("RightContext index must be non-negative in feature specification. ");
		}
		int tmpindex = 0;
		for (int i = right-1; i > left; i--) {
			if (!input.get(i).hasAncestorInside(left, right)) {
				if (tmpindex == index) {
					return input.get(i);
				} else {
					tmpindex++;
				}
			}
		}
		return null;
	}
	
	public DependencyNode getLeftTarget() {
		return input.get(left);
	}
	
	public DependencyNode getRightTarget() {
		return input.get(right);
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
	
//	public void initAllowRoot(boolean allowRoot) throws MaltChainedException {
//		if (allowRoot == true) {
//			leftstop = 0;
//		} else {
//			leftstop = 1;
//		}
//	}
	
	
	public void initialize(ParserConfiguration parserConfiguration) throws MaltChainedException {	
		if (parserConfiguration != null) {
			CovingtonConfig covingtonConfig = (CovingtonConfig)parserConfiguration;
			ArrayList<DependencyNode> sourceInput = covingtonConfig.getInput();
			setDependencyGraph(covingtonConfig.getDependencyGraph());
			for (int i = 0, n = sourceInput.size(); i < n; i++) {
				input.add(dependencyGraph.getDependencyNode(sourceInput.get(i).getIndex()));
			}
			left = covingtonConfig.getLeft();
			right = covingtonConfig.getRight();
			rightstop = covingtonConfig.getRightstop();
			leftstop = covingtonConfig.getLeftstop();
		} else {
			for (int i = 0, n = dependencyGraph.getHighestTokenIndex(); i <= n; i++) {
				DependencyNode node = dependencyGraph.getDependencyNode(i);
				if (node != null) { 
					input.add(node);
				}
			}
			if (allowRoot == true) {
				leftstop = 0;
			} else {
				leftstop = 1;
			}
			rightstop = dependencyGraph.getHighestTokenIndex();
			left = leftstop;
			right = left + 1;
		}
	}
	
	public void clear() throws MaltChainedException {
		dependencyGraph.clear();
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
		CovingtonConfig that = (CovingtonConfig)obj;
		
		if (input.size() != that.getInput().size())
			return false;
		if (dependencyGraph.nEdges() != that.getDependencyGraph().nEdges())
			return false;
		for (int i = 0; i < input.size(); i++) {
			if (input.get(i).getIndex() != that.getInput().get(i).getIndex()) {
				return false;
			}
		}		
		return dependencyGraph.getEdges().equals(that.getDependencyGraph().getEdges());
	}
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(input.size());
		sb.append(", ");
		sb.append(dependencyGraph.nEdges());
		return sb.toString();
	}

}
