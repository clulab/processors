package org.maltparserx.parser.algorithm.nivre;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.syntaxgraph.DependencyStructure;
import org.maltparserx.core.syntaxgraph.node.DependencyNode;
import org.maltparserx.parser.DependencyParserConfig;
import org.maltparserx.parser.Oracle;
import org.maltparserx.parser.ParserConfiguration;
import org.maltparserx.parser.history.GuideUserHistory;
import org.maltparserx.parser.history.action.GuideUserAction;
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
