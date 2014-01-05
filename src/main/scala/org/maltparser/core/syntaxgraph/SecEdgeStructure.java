package org.maltparser.core.syntaxgraph;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.edge.Edge;
import org.maltparser.core.syntaxgraph.node.ComparableNode;
/**
*
*
* @author Johan Hall
*/
public interface SecEdgeStructure {
	public Edge addSecondaryEdge(ComparableNode source, ComparableNode target) throws MaltChainedException;
	public void removeSecondaryEdge(ComparableNode source, ComparableNode target) throws MaltChainedException;
}
