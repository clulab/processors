package org.maltparser.core.syntaxgraph.node;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.edge.Edge;


public interface PhraseStructureNode extends ComparableNode {
	public PhraseStructureNode getParent();
	public Edge getParentEdge() throws MaltChainedException;
	public String getParentEdgeLabelSymbol(SymbolTable table) throws MaltChainedException;
	public int getParentEdgeLabelCode(SymbolTable table) throws MaltChainedException;
	public boolean hasParentEdgeLabel(SymbolTable table) throws MaltChainedException;
}
