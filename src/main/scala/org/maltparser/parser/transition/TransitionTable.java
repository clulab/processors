package org.maltparser.parser.transition;

import java.util.SortedMap;
import java.util.TreeMap;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.HashMap;
import org.maltparser.core.symbol.Table;
import org.maltparser.parser.history.container.DecisionPropertyTable;
/**
*
* @author Johan Hall
* @since 1.1
**/
public class TransitionTable implements Table, DecisionPropertyTable {
	private String name;
	private final SortedMap<Integer,Transition> code2transitionMap;
	private final HashMap<String,Transition> symbol2transitionMap;
	private final HashMap<Transition,TransitionTable> childrenTables;
	
	public TransitionTable(String tableName) {
		setName(name);
		code2transitionMap = new TreeMap<Integer,Transition>();
		symbol2transitionMap = new HashMap<String,Transition>();
		childrenTables = new HashMap<Transition,TransitionTable>();
	}
	
	public void addTransition(int code, String symbol, boolean labeled, TransitionTable childrenTable) {
		final Transition transition = new Transition(code, symbol, labeled); 
		code2transitionMap.put(code,transition);
		symbol2transitionMap.put(symbol, transition);
		if (childrenTable != null) {
			childrenTables.put(transition, childrenTable);
		}
	}
	
	public boolean continueWithNextDecision(int code) throws MaltChainedException {
		if (code2transitionMap.containsKey(code)) {
			return code2transitionMap.get(code).isLabeled();
		}
		return true;
	}
	
	public boolean continueWithNextDecision(String symbol) throws MaltChainedException {
		if (symbol2transitionMap.containsKey(symbol)) {
			return symbol2transitionMap.get(symbol).isLabeled();
		}
		return true;
	}
	
	public Table getTableForNextDecision(int code) throws MaltChainedException {
		if (code2transitionMap.containsKey(code)) {
			return childrenTables.get(code2transitionMap.get(code));
		}
		return null;
	}
	
	public Table getTableForNextDecision(String symbol) throws MaltChainedException {
		if (symbol2transitionMap.containsKey(symbol)) {
			return childrenTables.get(symbol2transitionMap.get(symbol));
		}
		return null;
	}
	
	public Transition getTransition(String symbol) {
		return symbol2transitionMap.get(symbol);
	}
	
	public Transition getTransition(int code) {
		return code2transitionMap.get(code);
	}
	
	public int addSymbol(String symbol) throws MaltChainedException {
		return -1;
	}

	public String getName() {
		return name;
	}

	
	public String getSymbolCodeToString(int code) throws MaltChainedException {
		if (code < 0) {
			return null;
		}
		return code2transitionMap.get(code).getSymbol();
	}

	public int getSymbolStringToCode(String symbol) throws MaltChainedException {
		if (symbol == null) {
			return -1;
		}
		return symbol2transitionMap.get(symbol).getCode();
	}

	protected void setName(String name) {
		this.name = name;
	}
	
	public int size() {
		return code2transitionMap.size();
	}

}
