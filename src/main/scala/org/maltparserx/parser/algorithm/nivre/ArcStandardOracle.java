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
public class ArcStandardOracle extends Oracle {
	
	public ArcStandardOracle(DependencyParserConfig manager, GuideUserHistory history) throws MaltChainedException {
		super(manager, history);
		setGuideName("ArcStandard");
	}
	
	public GuideUserAction predict(DependencyStructure gold, ParserConfiguration config) throws MaltChainedException {
		NivreConfig nivreConfig = (NivreConfig)config;
		DependencyNode stackPeek = nivreConfig.getStack().peek();
		int stackPeekIndex = stackPeek.getIndex();
		int inputPeekIndex = nivreConfig.getInput().peek().getIndex();
		
		if (!nivreConfig.isAllowRoot() && stackPeek.isRoot()) {
			return updateActionContainers(ArcStandard.SHIFT, null);
		}
		if (!stackPeek.isRoot() && gold.getTokenNode(stackPeekIndex).getHead().getIndex() == inputPeekIndex) {
			return updateActionContainers(ArcStandard.LEFTARC, gold.getTokenNode(stackPeekIndex).getHeadEdge().getLabelSet());
		} else if (gold.getTokenNode(inputPeekIndex).getHead().getIndex() == stackPeekIndex && checkRightDependent(gold, nivreConfig.getDependencyGraph(), inputPeekIndex)) {
			return updateActionContainers(ArcStandard.RIGHTARC, gold.getTokenNode(inputPeekIndex).getHeadEdge().getLabelSet());
		} else {
			return updateActionContainers(ArcStandard.SHIFT, null);
		}
	}
	
	private boolean checkRightDependent(DependencyStructure gold, DependencyStructure parseDependencyGraph, int inputPeekIndex) throws MaltChainedException {
		if (gold.getTokenNode(inputPeekIndex).getRightmostDependent() == null) {
			return true;
		} else if (parseDependencyGraph.getTokenNode(inputPeekIndex).getRightmostDependent() != null) {
			if (gold.getTokenNode(inputPeekIndex).getRightmostDependent().getIndex() == parseDependencyGraph.getTokenNode(inputPeekIndex).getRightmostDependent().getIndex()) {
				return true;
			}
		}
		return false;
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {
		
	}
	
	public void terminate() throws MaltChainedException {
		
	}
}
