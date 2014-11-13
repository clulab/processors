package org.maltparserx.parser.algorithm.covington;

import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.parser.Algorithm;
import org.maltparserx.parser.TransitionSystem;
import org.maltparserx.parser.guide.OracleGuide;
import org.maltparserx.parser.history.GuideUserHistory;
/**
 * @author Johan Hall
 *
 */
public class CovingtonNonProjFactory extends CovingtonFactory {
	public CovingtonNonProjFactory(Algorithm algorithm) {
		super(algorithm);
	}
	
	public TransitionSystem makeTransitionSystem() throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Transition system    : Non-Projective\n");
		}
		TransitionSystem ts = new NonProjective();
		ts.setPropagationManager(manager.getPropagationManager());
		return ts;
	}
	
	public OracleGuide makeOracleGuide(GuideUserHistory history) throws MaltChainedException {
		if (manager.getConfigLogger().isInfoEnabled()) {
			manager.getConfigLogger().info("  Oracle               : Covington\n");
		}
		return new CovingtonOracle(manager, history);
	}
}
