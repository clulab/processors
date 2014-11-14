package org.maltparserx.core.syntaxgraph;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.syntaxgraph.edge.Edge;
import org.maltparserx.core.syntaxgraph.node.ComparableNode;
/**
*
*
* @author Johan Hall
*/
public interface SecEdgeStructure {
	public Edge addSecondaryEdge(ComparableNode source, ComparableNode target) throws MaltChainedException;
	public void removeSecondaryEdge(ComparableNode source, ComparableNode target) throws MaltChainedException;
}
