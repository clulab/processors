package org.maltparser.parser;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.parser.guide.ClassifierGuide;
import org.maltparser.parser.guide.OracleGuide;
import org.maltparser.parser.guide.SingleGuide;
import org.maltparser.parser.history.GuideHistory;
import org.maltparser.parser.history.action.GuideDecision;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * @author Johan Hall
 *
 */
public class BatchTrainer extends Trainer {
	private final OracleGuide oracleGuide;
	private int parseCount;
	
	public BatchTrainer(DependencyParserConfig manager) throws MaltChainedException {
		super(manager);
		((SingleMalt)manager).addRegistry(org.maltparser.parser.Algorithm.class, this);
		setManager(manager);
		initParserState(1);
		setGuide(new SingleGuide(manager, (GuideHistory)parserState.getHistory(), ClassifierGuide.GuideMode.BATCH));
		oracleGuide = parserState.getFactory().makeOracleGuide(parserState.getHistory());
	}
	
	public DependencyStructure parse(DependencyStructure goldDependencyGraph, DependencyStructure parseDependencyGraph) throws MaltChainedException {
		parserState.clear();
		parserState.initialize(parseDependencyGraph);
		currentParserConfiguration = parserState.getConfiguration();
		parseCount++;
		if (diagnostics == true) {
			writeToDiaFile(parseCount + "");
		}
		TransitionSystem transitionSystem = parserState.getTransitionSystem();
		while (!parserState.isTerminalState()) {
			GuideUserAction action = transitionSystem.getDeterministicAction(parserState.getHistory(), currentParserConfiguration);
			if (action == null) {
				action = oracleGuide.predict(goldDependencyGraph, currentParserConfiguration);
				try {
					classifierGuide.addInstance((GuideDecision)action);
				} catch (NullPointerException e) {
					throw new MaltChainedException("The guide cannot be found. ", e);
				}
			} else if (diagnostics == true) {
				writeToDiaFile(" *");
			}
			if (diagnostics == true) {
				writeToDiaFile(" " + transitionSystem.getActionString(action));
			}	
			parserState.apply(action);
		}
		copyEdges(currentParserConfiguration.getDependencyGraph(), parseDependencyGraph);
		parseDependencyGraph.linkAllTreesToRoot();
		oracleGuide.finalizeSentence(parseDependencyGraph);
		if (diagnostics == true) {
			writeToDiaFile("\n");
		}
		return parseDependencyGraph;
	}
	
	public OracleGuide getOracleGuide() {
		return oracleGuide;
	}
	
	public void train() throws MaltChainedException { }
	public void terminate() throws MaltChainedException {
		if (diagnostics == true) {
			closeDiaWriter();
		}
	}
}
