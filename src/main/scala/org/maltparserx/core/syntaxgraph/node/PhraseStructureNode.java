package org.maltparserx.core.syntaxgraph.node;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.symbol.SymbolTable;
import org.maltparserx.core.syntaxgraph.edge.Edge;


public interface PhraseStructureNode extends ComparableNode {
	public PhraseStructureNode getParent();
	public Edge getParentEdge() throws MaltChainedException;
	public String getParentEdgeLabelSymbol(SymbolTable table) throws MaltChainedException;
	public int getParentEdgeLabelCode(SymbolTable table) throws MaltChainedException;
	public boolean hasParentEdgeLabel(SymbolTable table) throws MaltChainedException;
}
