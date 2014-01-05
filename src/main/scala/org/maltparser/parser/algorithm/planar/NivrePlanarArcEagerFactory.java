package org.maltparser.parser.algorithm.planar;

import org.maltparser.core.exception.MaltChainedException;

import org.maltparser.parser.Algorithm;
import org.maltparser.parser.TransitionSystem;
import org.maltparser.parser.guide.OracleGuide;
import org.maltparser.parser.history.GuideUserHistory;
/**
 * @author Carlos Gomez Rodriguez
 *
 */
public class NivrePlanarArcEagerFactory extends PlanarFactory {
	public NivrePlanarArcEagerFactory(Algorithm algorithm) {
		super(algorithm);
	}
	
	public TransitionSystem makeTransitionSystem() throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Transition system    : Planar Arc-Eager\n");
		}
		TransitionSystem ts = new Planar();
		ts.setPropagationManager(manager.getPropagationManager());
		return ts;
	}
	
	public OracleGuide makeOracleGuide(GuideUserHistory history) throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Oracle               : Planar Arc-Eager\n");
		}
		return new PlanarArcEagerOracle(manager, history);
	}
}
