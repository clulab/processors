package org.maltparser.core.syntaxgraph.node;

import java.util.SortedSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.headrules.HeadRules;

public interface NonTerminalNode extends PhraseStructureNode {
	public TokenNode identifyHead(HeadRules headRules) throws MaltChainedException;
	public TokenNode getLexicalHead(HeadRules headRules) throws MaltChainedException;
	public TokenNode getLexicalHead() throws MaltChainedException;
	public PhraseStructureNode getHeadChild(HeadRules headRules) throws MaltChainedException;
	public PhraseStructureNode getHeadChild() throws MaltChainedException;
	public SortedSet<PhraseStructureNode> getChildren();
	public PhraseStructureNode getChild(int index);
	public PhraseStructureNode getLeftChild();
	public PhraseStructureNode getRightChild();
	public int nChildren();
	public boolean hasNonTerminalChildren();
	public boolean hasTerminalChildren();
	public int getHeight();
	public boolean isContinuous();
	public boolean isContinuousExcludeTerminalsAttachToRoot();
	//public void reArrangeChildrenAccordingToLeftAndRightProperDesendant() throws MaltChainedException;
}
