package org.maltparser.parser.algorithm.nivre;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.Oracle;
import org.maltparser.parser.ParserConfiguration;
import org.maltparser.parser.history.GuideUserHistory;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * @author Johan Hall
 *
 */
public class ArcEagerOracle extends Oracle {

	public ArcEagerOracle(DependencyParserConfig manager, GuideUserHistory history) throws MaltChainedException {
		super(manager, history);
		setGuideName("ArcEager");
	}
	
	public GuideUserAction predict(DependencyStructure gold, ParserConfiguration config) throws MaltChainedException {
		final NivreConfig nivreConfig = (NivreConfig)config;
		final DependencyNode stackPeek = nivreConfig.getStack().peek();
		final int stackPeekIndex = stackPeek.getIndex();
		final int inputPeekIndex = nivreConfig.getInput().peek().getIndex();
		
		if (!stackPeek.isRoot() && gold.getTokenNode(stackPeekIndex).getHead().getIndex() == inputPeekIndex) {
			return updateActionContainers(ArcEager.LEFTARC, gold.getTokenNode(stackPeekIndex).getHeadEdge().getLabelSet());
		} else if (gold.getTokenNode(inputPeekIndex).getHead().getIndex() == stackPeekIndex) {
			return updateActionContainers(ArcEager.RIGHTARC, gold.getTokenNode(inputPeekIndex).getHeadEdge().getLabelSet());
		} else if (!nivreConfig.isAllowReduce() && !stackPeek.hasHead()) {
			return updateActionContainers(ArcEager.SHIFT, null);
		} else if (gold.getTokenNode(inputPeekIndex).hasLeftDependent() &&
				gold.getTokenNode(inputPeekIndex).getLeftmostDependent().getIndex() < stackPeekIndex) {
			return updateActionContainers(ArcEager.REDUCE, null);
		} else if (gold.getTokenNode(inputPeekIndex).getHead().getIndex() < stackPeekIndex && 
				(!gold.getTokenNode(inputPeekIndex).getHead().isRoot() || nivreConfig.isAllowRoot())) {
			return updateActionContainers(ArcEager.REDUCE, null);
		} else {
			return updateActionContainers(ArcEager.SHIFT, null);
		}
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {}
	
	public void terminate() throws MaltChainedException {}
}
