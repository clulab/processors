package org.maltparser.parser.history.action;

import java.util.ArrayList;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.parser.history.GuideHistory;
import org.maltparser.parser.history.GuideUserHistory;
import org.maltparser.parser.history.HistoryException;
import org.maltparser.parser.history.History;
import org.maltparser.parser.history.container.ActionContainer;
import org.maltparser.parser.history.container.CombinedTableContainer;
import org.maltparser.parser.history.kbest.ScoredKBestList;

/**
*
* @author Johan Hall
* @since 1.1
**/
public class ComplexDecisionAction implements GuideUserAction, MultipleDecision {
	private History history;
	private ArrayList<SimpleDecisionAction> decisions;
	
	public ComplexDecisionAction(History history) throws MaltChainedException {
		this.history = history;
		decisions = new ArrayList<SimpleDecisionAction>(history.getDecisionTables().size());
		for (int i=0, n = history.getDecisionTables().size(); i < n; i++) {
			decisions.add(new SimpleDecisionAction(history, history.getDecisionTables().get(i)));
		}
	}
	
	public ComplexDecisionAction(GuideHistory history) throws MaltChainedException {
		this((History)history);
	}
	
	/* GuideUserAction interface */
	public void addAction(ArrayList<ActionContainer> actionContainers) throws MaltChainedException {
		if (actionContainers == null || actionContainers.size() != history.getActionTables().size()) {
			throw new HistoryException("The action containers does not exist or is not of the same size as the action table. ");
		}
		int j = 0;
		for (int i = 0, n = history.getDecisionTables().size(); i < n; i++) {
			if (history.getDecisionTables().get(i) instanceof CombinedTableContainer) {
				CombinedTableContainer tableContainer = (CombinedTableContainer)history.getDecisionTables().get(i);
				int nContainers = tableContainer.getNumberContainers();
				decisions.get(i).addDecision(tableContainer.getCombinedCode(actionContainers.subList(j, j + nContainers)));
				j = j + nContainers;
			} else {
				decisions.get(i).addDecision(actionContainers.get(j).getActionCode());
				j++;
			}
		}
	}
	
	public void getAction(ArrayList<ActionContainer> actionContainers) throws MaltChainedException {
		if (actionContainers == null || actionContainers.size() != history.getActionTables().size()) {
			throw new HistoryException("The action containers does not exist or is not of the same size as the action table. ");
		}
		int j = 0;
		for (int i = 0, n=history.getDecisionTables().size(); i < n; i++) {
			if (history.getDecisionTables().get(i) instanceof CombinedTableContainer) {
				CombinedTableContainer tableContainer = (CombinedTableContainer)history.getDecisionTables().get(i);
				int nContainers = tableContainer.getNumberContainers();
				tableContainer.setActionContainer(actionContainers.subList(j, j + nContainers), decisions.get(i).getDecisionCode());
				j = j + nContainers;
			} else {
				actionContainers.get(j).setAction(decisions.get(i).getDecisionCode());
				j++;
			}
		}
	}
	
	public void addAction(ActionContainer[] actionContainers) throws MaltChainedException {
		if (actionContainers == null || actionContainers.length != history.getActionTables().size()) {
			throw new HistoryException("The action containers does not exist or is not of the same size as the action table. ");
		}
		int j = 0;
		for (int i = 0, n = history.getDecisionTables().size(); i < n; i++) {
			if (history.getDecisionTables().get(i) instanceof CombinedTableContainer) {
				CombinedTableContainer tableContainer = (CombinedTableContainer)history.getDecisionTables().get(i);
				int nContainers = tableContainer.getNumberContainers();
				decisions.get(i).addDecision(tableContainer.getCombinedCode(actionContainers, j));
				j = j + nContainers;
			} else {
				decisions.get(i).addDecision(actionContainers[j].getActionCode());
				j++;
			}
		}
	}
	
	public void getAction(ActionContainer[] actionContainers) throws MaltChainedException {
		if (actionContainers == null || actionContainers.length != history.getActionTables().size()) {
			throw new HistoryException("The action containers does not exist or is not of the same size as the action table. ");
		}
		int j = 0;
		for (int i = 0, n=history.getDecisionTables().size(); i < n; i++) {
			if (history.getDecisionTables().get(i) instanceof CombinedTableContainer) {
				CombinedTableContainer tableContainer = (CombinedTableContainer)history.getDecisionTables().get(i);
				int nContainers = tableContainer.getNumberContainers();
				tableContainer.setActionContainer(actionContainers, j, decisions.get(i).getDecisionCode());
				j = j + nContainers;
			} else {
				actionContainers[j].setAction(decisions.get(i).getDecisionCode());
				j++;
			}
		}
	}
	
	
	public void getKBestLists(ArrayList<ScoredKBestList> kbestListContainers) throws MaltChainedException {
//		if (kbestListContainers == null || kbestListContainers.size() != history.getActionTables().size()) {
//			throw new HistoryException("The action containers does not exist or is not of the same size as the action table. ");
//		}
		kbestListContainers.clear();
		for (int i = 0, n=decisions.size(); i < n; i++) {
			kbestListContainers.add((ScoredKBestList)decisions.get(i).getKBestList());
		}
	}
	
	public void getKBestLists(ScoredKBestList[] kbestListContainers) throws MaltChainedException {
		for (int i = 0, n=decisions.size(); i < n; i++) {
			kbestListContainers[0] = (ScoredKBestList)decisions.get(i).getKBestList();
		}
	}
	
	public int numberOfActions() {
		return history.getActionTables().size();
	}
	
	public GuideUserHistory getGuideUserHistory() {
		return (GuideUserHistory)history;
	}
	
	public void clear() {
		for (int i=0, n = decisions.size(); i < n;i++) {
			decisions.get(i).clear();
		}
	}
	
	/* MultipleDecision */
	public SingleDecision getSingleDecision(int decisionIndex) throws MaltChainedException {
		return decisions.get(decisionIndex);
	}

	/* GuideDecision */
	public int numberOfDecisions() {
		return history.getDecisionTables().size();
	}

	public GuideHistory getGuideHistory() {
		return (GuideHistory)history;
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ComplexDecisionAction other = (ComplexDecisionAction) obj;
		if (decisions == null) {
			if (other.decisions != null)
				return false;
		} else if (decisions.size() != other.decisions.size()) {
			return false;
		} else {
			for (int i = 0; i < decisions.size(); i++) {
				try {
					if (decisions.get(i).getDecisionCode() != other.decisions.get(i).getDecisionCode()) {
						return false;
					}
				} catch (MaltChainedException e) {
					System.err.println("Error in equals. ");
				}
			}
		}
		
		return true;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (int i = 0, n = decisions.size(); i < n; i++) {
			sb.append(decisions.get(i));
			sb.append(';');
		}
		if (sb.length() > 0) {
			sb.setLength(sb.length()-1);
		}
		return sb.toString();
	}
}
