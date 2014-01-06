package org.maltparserx.parser.algorithm.covington;

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
public class CovingtonOracle extends Oracle {
	public CovingtonOracle(DependencyParserConfig manager, GuideUserHistory history) throws MaltChainedException {
		super(manager, history);
		setGuideName("NonProjective");
	}
	
	public GuideUserAction predict(DependencyStructure gold, ParserConfiguration config) throws MaltChainedException {
		CovingtonConfig covingtonConfig = (CovingtonConfig)config;
		DependencyNode leftTarget = covingtonConfig.getLeftTarget();
		int leftTargetIndex = leftTarget.getIndex();
		int rightTargetIndex = covingtonConfig.getRightTarget().getIndex();
		
		if (!leftTarget.isRoot() && gold.getTokenNode(leftTargetIndex).getHead().getIndex() == rightTargetIndex) {
			return updateActionContainers(NonProjective.LEFTARC, gold.getTokenNode(leftTargetIndex).getHeadEdge().getLabelSet());
		} else if (gold.getTokenNode(rightTargetIndex).getHead().getIndex() == leftTargetIndex) {
			return updateActionContainers(NonProjective.RIGHTARC, gold.getTokenNode(rightTargetIndex).getHeadEdge().getLabelSet());
		} else if (covingtonConfig.isAllowShift() == true && (!(gold.getTokenNode(rightTargetIndex).hasLeftDependent() 
				&& gold.getTokenNode(rightTargetIndex).getLeftmostDependent().getIndex() < leftTargetIndex)
				&& !(gold.getTokenNode(rightTargetIndex).getHead().getIndex() < leftTargetIndex 
						&& (!gold.getTokenNode(rightTargetIndex).getHead().isRoot() || covingtonConfig.getLeftstop() == 0)))) {
			return updateActionContainers(NonProjective.SHIFT, null);
		} else {
			return updateActionContainers(NonProjective.NOARC, null);
		}
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {
		
	}
	
	public void terminate() throws MaltChainedException {
		
	}
}
