package org.maltparserx.parser.algorithm.nivre;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.parser.Algorithm;
import org.maltparserx.parser.TransitionSystem;
import org.maltparserx.parser.guide.OracleGuide;
import org.maltparserx.parser.history.GuideUserHistory;
/**
 * @author Johan Hall
 *
 */
public class NivreArcStandardFactory extends NivreFactory {
	public NivreArcStandardFactory(Algorithm algorithm) {
		super(algorithm);
	}
	
	public TransitionSystem makeTransitionSystem() throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Transition system    : Arc-Standard\n");
		}
		TransitionSystem ts = new ArcStandard();
		ts.setPropagationManager(manager.getPropagationManager());
		return ts;
	}
	
	public OracleGuide makeOracleGuide(GuideUserHistory history) throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Oracle               : Arc-Standard\n");
		}
		return new ArcStandardOracle(manager, history);
	}
}
