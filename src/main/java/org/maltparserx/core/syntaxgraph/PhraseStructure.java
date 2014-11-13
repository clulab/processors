package org.maltparserx.core.syntaxgraph;

import java.util.Set;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.syntaxgraph.edge.Edge;
import org.maltparserx.core.syntaxgraph.node.PhraseStructureNode;
/**
*
*
* @author Johan Hall
*/
public interface PhraseStructure extends TokenStructure, SecEdgeStructure {
	public PhraseStructureNode addTerminalNode() throws MaltChainedException;
	public PhraseStructureNode addTerminalNode(int index) throws MaltChainedException;
	public PhraseStructureNode getTerminalNode(int index);
	public int nTerminalNode();
	public Edge addPhraseStructureEdge(PhraseStructureNode source, PhraseStructureNode target) throws MaltChainedException;
	public void removePhraseStructureEdge(PhraseStructureNode source, PhraseStructureNode target) throws MaltChainedException;
	public int nEdges();
	public PhraseStructureNode getPhraseStructureRoot();
	public PhraseStructureNode getNonTerminalNode(int index) throws MaltChainedException;
	public PhraseStructureNode addNonTerminalNode() throws MaltChainedException;
	public PhraseStructureNode addNonTerminalNode(int index) throws MaltChainedException;
	public int getHighestNonTerminalIndex();
	public Set<Integer> getNonTerminalIndices();
	public boolean hasNonTerminals();
	public int nNonTerminals();
	public boolean isContinuous();
	public boolean isContinuousExcludeTerminalsAttachToRoot();
//	public void makeContinuous() throws MaltChainedException;
}
