package org.maltparser.parser.algorithm.nivre;

import org.maltparser.core.exception.MaltChainedException;

import org.maltparser.parser.Algorithm;
import org.maltparser.parser.TransitionSystem;
import org.maltparser.parser.guide.OracleGuide;
import org.maltparser.parser.history.GuideUserHistory;
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
