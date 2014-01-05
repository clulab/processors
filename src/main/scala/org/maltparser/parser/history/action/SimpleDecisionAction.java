package org.maltparser.parser.history.action;


import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.history.GuideHistory;
import org.maltparser.parser.history.HistoryException;
import org.maltparser.parser.history.History;
import org.maltparser.parser.history.container.TableContainer;
import org.maltparser.parser.history.container.TableContainer.RelationToNextDecision;
import org.maltparser.parser.history.kbest.KBestList;
/**
*
* @author Johan Hall
* @since 1.1
**/
public class SimpleDecisionAction implements  SingleDecision {
	private History history;
	private TableContainer tableContainer;
	private int decision;
	private KBestList kBestList;
	
	
	public SimpleDecisionAction(History history, TableContainer tableContainer) throws MaltChainedException {
		this.history = history;
		this.tableContainer = tableContainer;
		createKBestList();
		clear();
	}
	
	/* Action interface */
	public void clear() {
		decision = -1;
		if (kBestList != null) {
			kBestList.reset();
		}
	}

	public int numberOfDecisions() {
		return 1;
	}
	
	/* SingleDecision interface */
	public void addDecision(int code) throws MaltChainedException {
		if (code == -1 || !tableContainer.containCode(code)) {
			decision = -1;
		}
		decision = code;
	}

	public void addDecision(String symbol) throws MaltChainedException {
		decision = tableContainer.getCode(symbol);
	}

	public int getDecisionCode() throws MaltChainedException {
		return decision;
	}

	public int getDecisionCode(String symbol) throws MaltChainedException {
		return tableContainer.getCode(symbol);
	}

	public String getDecisionSymbol() throws MaltChainedException {
		return tableContainer.getSymbol(decision);
	}
	
	public boolean updateFromKBestList() throws MaltChainedException {
		if (kBestList == null) {
			return false;
		}
		return kBestList.updateActionWithNextKBest();
	}
	
	public boolean continueWithNextDecision() throws MaltChainedException {
		return tableContainer.continueWithNextDecision(decision);
	}
	
	public GuideHistory getGuideHistory() {
		return (GuideHistory)history;
	}
	
	/* Getters and Setters */
	public History getActionHistory() {
		return history;
	}

	public TableContainer getTableContainer() {
		return tableContainer;
	}
	
	public KBestList getKBestList() throws MaltChainedException {
		return kBestList;
	}
	
	public RelationToNextDecision getRelationToNextDecision() {
		return tableContainer.getRelationToNextDecision();
	}
	
	private void createKBestList() throws MaltChainedException {
		final Class<?> kBestListClass = history.getKBestListClass();
		if (kBestListClass == null) {
			return;
		}
		final Class<?>[] argTypes = { java.lang.Integer.class, org.maltparser.parser.history.action.SingleDecision.class };
	
		final Object[] arguments = new Object[2];
		arguments[0] = history.getKBestSize();
		arguments[1] = this;
		try {
			final Constructor<?> constructor = kBestListClass.getConstructor(argTypes);
			kBestList = (KBestList)constructor.newInstance(arguments);
		} catch (NoSuchMethodException e) {
			throw new HistoryException("The kBestlist '"+kBestListClass.getName()+"' cannot be initialized. ", e);
		} catch (InstantiationException e) {
			throw new HistoryException("The kBestlist '"+kBestListClass.getName()+"' cannot be initialized. ", e);
		} catch (IllegalAccessException e) {
			throw new HistoryException("The kBestlist '"+kBestListClass.getName()+"' cannot be initialized. ", e);
		} catch (InvocationTargetException e) {
			throw new HistoryException("The kBestlist '"+kBestListClass.getName()+"' cannot be initialized. ", e);
		}
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(decision);
		return sb.toString();
	}
}
