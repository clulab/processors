package org.maltparser.parser.algorithm.stack;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.Algorithm;
import org.maltparser.parser.TransitionSystem;
import org.maltparser.parser.guide.OracleGuide;
import org.maltparser.parser.history.GuideUserHistory;
/**
 * @author Johan Hall
 *
 */
public class StackSwapEagerFactory extends StackFactory {
	public StackSwapEagerFactory(Algorithm algorithm) {
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
			manager.getConfigLogger().info("  Oracle               : Swap-Eager\n");
		}
		return new SwapEagerOracle(manager, history);
	}
}
