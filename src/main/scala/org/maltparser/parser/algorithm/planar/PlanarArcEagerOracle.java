package org.maltparser.parser.algorithm.planar;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.core.syntaxgraph.node.DependencyNode;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.Oracle;
import org.maltparser.parser.ParserConfiguration;
import org.maltparser.parser.history.GuideUserHistory;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * @author Carlos Gomez Rodriguez
 *
 */
public class PlanarArcEagerOracle extends Oracle {

	public PlanarArcEagerOracle(DependencyParserConfig manager, GuideUserHistory history) throws MaltChainedException {
		super(manager, history);
		setGuideName("Planar");
	}
	
	public GuideUserAction predict(DependencyStructure gold, ParserConfiguration config) throws MaltChainedException {
		PlanarConfig planarConfig = (PlanarConfig)config;
		DependencyStructure dg = planarConfig.getDependencyGraph();
		DependencyNode stackPeek = planarConfig.getStack().peek();
		int stackPeekIndex = stackPeek.getIndex();
		int inputPeekIndex = planarConfig.getInput().peek().getIndex();
		
		if (!stackPeek.isRoot() && gold.getTokenNode(stackPeekIndex).getHead().getIndex() == inputPeekIndex
				&& !checkIfArcExists ( dg , inputPeekIndex , stackPeekIndex ) )  {
			return updateActionContainers(Planar.LEFTARC, gold.getTokenNode(stackPeekIndex).getHeadEdge().getLabelSet());	
		} else if (gold.getTokenNode(inputPeekIndex).getHead().getIndex() == stackPeekIndex
				&& !checkIfArcExists ( dg , stackPeekIndex , inputPeekIndex ) ) {
			return updateActionContainers(Planar.RIGHTARC, gold.getTokenNode(inputPeekIndex).getHeadEdge().getLabelSet());
		} else if (gold.getTokenNode(inputPeekIndex).hasLeftDependent() &&
				gold.getTokenNode(inputPeekIndex).getLeftmostDependent().getIndex() < stackPeekIndex) {
			return updateActionContainers(Planar.REDUCE, null);
		} else if (gold.getTokenNode(inputPeekIndex).getHead().getIndex() < stackPeekIndex
				&& 
				( !gold.getTokenNode(inputPeekIndex).getHead().isRoot() || planarConfig.getRootHandling()==PlanarConfig.NORMAL )	
			) {
			return updateActionContainers(Planar.REDUCE, null);
		} else {
			return updateActionContainers(Planar.SHIFT, null);
		}
			
	}
	
	private boolean checkIfArcExists ( DependencyStructure dg , int index1 , int index2 ) throws MaltChainedException
	{
		return dg.getTokenNode(index2).hasHead() && dg.getTokenNode(index2).getHead().getIndex() == index1;
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException {}
	
	public void terminate() throws MaltChainedException {}
}
