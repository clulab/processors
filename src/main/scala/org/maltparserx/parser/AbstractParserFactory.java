package org.maltparserx.parser;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.AbstractFeatureFactory;
import org.maltparserx.parser.guide.OracleGuide;
import org.maltparserx.parser.history.GuideUserHistory;
/**
 * @author Johan Hall
 *
 */
public interface AbstractParserFactory extends AbstractFeatureFactory {
	/**
	 * Creates a parser configuration
	 * 
	 * @return a parser configuration
	 * @throws MaltChainedException
	 */
	public ParserConfiguration makeParserConfiguration() throws MaltChainedException;
	/**
	 * Creates a transition system
	 * 
	 * @return a transition system
	 * @throws MaltChainedException
	 */
	public TransitionSystem makeTransitionSystem() throws MaltChainedException;
	/**
	 * Creates an oracle guide
	 * 
	 * @param history a reference to the history
	 * @return  an oracle guide
	 * @throws MaltChainedException
	 */
	public OracleGuide makeOracleGuide(GuideUserHistory history) throws MaltChainedException;
}
