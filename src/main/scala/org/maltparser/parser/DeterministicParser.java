package org.maltparser.parser;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.syntaxgraph.DependencyStructure;

import org.maltparser.parser.guide.ClassifierGuide;
import org.maltparser.parser.guide.SingleGuide;
import org.maltparser.parser.history.GuideHistory;
import org.maltparser.parser.history.action.GuideDecision;
import org.maltparser.parser.history.action.GuideUserAction;
/**
 * @author Johan Hall
 *
 */
public class DeterministicParser extends Parser {
	private int parseCount;
	
	public DeterministicParser(DependencyParserConfig manager) throws MaltChainedException {
		super(manager);
		setManager(manager);
		initParserState(1);
		((SingleMalt)manager).addRegistry(org.maltparser.parser.Algorithm.class, this);
		setGuide(new SingleGuide(manager, (GuideHistory)parserState.getHistory(), ClassifierGuide.GuideMode.CLASSIFY));
	}
	
	public DependencyStructure parse(DependencyStructure parseDependencyGraph) throws MaltChainedException {
		if (diagnostics == true) {
			return parseDiagnostic(parseDependencyGraph);
		}
		parserState.clear();
		parserState.initialize(parseDependencyGraph);
		currentParserConfiguration = parserState.getConfiguration();
		parseCount++;
		TransitionSystem ts = parserState.getTransitionSystem();
		while (!parserState.isTerminalState()) {
			GuideUserAction action = ts.getDeterministicAction(parserState.getHistory(), currentParserConfiguration);
			if (action == null) {
				action = predict();
			}
			parserState.apply(action);
		} 
		copyEdges(currentParserConfiguration.getDependencyGraph(), parseDependencyGraph);
		copyDynamicInput(currentParserConfiguration.getDependencyGraph(), parseDependencyGraph);
		parseDependencyGraph.linkAllTreesToRoot();
		return parseDependencyGraph;
	}
	
	private DependencyStructure parseDiagnostic(DependencyStructure parseDependencyGraph) throws MaltChainedException {
		parserState.clear();
		parserState.initialize(parseDependencyGraph);
		currentParserConfiguration = parserState.getConfiguration();
		parseCount++;
		if (diagnostics == true) {
			writeToDiaFile(parseCount + "");
		}
		while (!parserState.isTerminalState()) {
			GuideUserAction action = parserState.getTransitionSystem().getDeterministicAction(parserState.getHistory(), currentParserConfiguration);
			if (action == null) {
				action = predict();
			} else if (diagnostics == true) {
				writeToDiaFile(" *");
			}
			if (diagnostics == true) {
				writeToDiaFile(" " + parserState.getTransitionSystem().getActionString(action));
			}
			parserState.apply(action);
		} 
		copyEdges(currentParserConfiguration.getDependencyGraph(), parseDependencyGraph);
		copyDynamicInput(currentParserConfiguration.getDependencyGraph(), parseDependencyGraph);
		parseDependencyGraph.linkAllTreesToRoot();
		if (diagnostics == true) {
			writeToDiaFile("\n");
		}
		return parseDependencyGraph;
	}
	
	
	private GuideUserAction predict() throws MaltChainedException {
		GuideUserAction currentAction = parserState.getHistory().getEmptyGuideUserAction();
		try {
			classifierGuide.predict((GuideDecision)currentAction);
			while (!parserState.permissible(currentAction)) {
				if (classifierGuide.predictFromKBestList((GuideDecision)currentAction) == false) {
					currentAction = getParserState().getTransitionSystem().defaultAction(parserState.getHistory(), currentParserConfiguration);
					break;
				}
			}
		} catch (NullPointerException e) {
			throw new MaltChainedException("The guide cannot be found. ", e);
		}
		return currentAction;
	}
	
	public void terminate() throws MaltChainedException {
		if (diagnostics == true) {
			closeDiaWriter();
		}
	}
}
