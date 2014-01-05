package org.maltparser.parser;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.AbstractFeatureFactory;
import org.maltparser.parser.guide.OracleGuide;
import org.maltparser.parser.history.GuideUserHistory;
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
