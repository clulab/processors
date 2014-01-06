package org.maltparserx.parser.guide;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.syntaxgraph.DependencyStructure;
import org.maltparserx.parser.ParserConfiguration;
import org.maltparserx.parser.history.action.GuideUserAction;

public interface OracleGuide extends Guide {
	public GuideUserAction predict(DependencyStructure gold, ParserConfiguration config) throws MaltChainedException;
}
