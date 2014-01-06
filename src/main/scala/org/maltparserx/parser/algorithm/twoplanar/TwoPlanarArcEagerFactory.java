package org.maltparserx.parser.algorithm.twoplanar;

import org.maltparserx.core.exception.MaltChainedException;

import org.maltparserx.parser.Algorithm;
import org.maltparserx.parser.TransitionSystem;
import org.maltparserx.parser.guide.OracleGuide;
import org.maltparserx.parser.history.GuideUserHistory;
/**
 * @author Carlos Gomez Rodriguez
 *
 */
public class TwoPlanarArcEagerFactory extends TwoPlanarFactory {
	public TwoPlanarArcEagerFactory(Algorithm algorithm) {
		super(algorithm);
	}
	
	public TransitionSystem makeTransitionSystem() throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Transition system    : 2-Planar Arc-Eager\n");
		}
		TransitionSystem ts = new TwoPlanar();
		ts.setPropagationManager(manager.getPropagationManager());
		return ts;
	}
	
	public OracleGuide makeOracleGuide(GuideUserHistory history) throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Oracle               : 2-Planar Arc-Eager\n");
		}
		return new TwoPlanarArcEagerOracle(manager, history);
	}
}
