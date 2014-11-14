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
public class NivreArcEagerFactory extends NivreFactory {
	public NivreArcEagerFactory(Algorithm algorithm) {
		super(algorithm);
	}
	
	public TransitionSystem makeTransitionSystem() throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Transition system    : Arc-Eager\n");
		}
		TransitionSystem ts = new ArcEager();
		ts.setPropagationManager(manager.getPropagationManager());
		return ts;
	}
	
	public OracleGuide makeOracleGuide(GuideUserHistory history) throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Oracle               : Arc-Eager\n");
		}
		return new ArcEagerOracle(manager, history);
	}
}
