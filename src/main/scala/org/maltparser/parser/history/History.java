package org.maltparser.parser.history;

import java.util.ArrayList;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.HashMap;
import org.maltparser.core.pool.ObjectPoolList;
import org.maltparser.core.symbol.TableHandler;
import org.maltparser.parser.history.action.ActionDecision;
import org.maltparser.parser.history.action.ComplexDecisionAction;
import org.maltparser.parser.history.action.GuideDecision;
import org.maltparser.parser.history.action.GuideUserAction;
import org.maltparser.parser.history.container.ActionContainer;
import org.maltparser.parser.history.container.CombinedTableContainer;
import org.maltparser.parser.history.container.TableContainer;
import org.maltparser.parser.history.kbest.KBestList;

/**
*
* @author Johan Hall
* @since 1.1
**/
public class History implements GuideUserHistory, GuideHistory {
	protected final ObjectPoolList<ComplexDecisionAction> actionPool;
	protected Class<? extends KBestList> kBestListClass = null;
	protected int kBestSize;
	protected String separator = "~";
	protected String decisionSettings;
	protected ArrayList<TableContainer> decisionTables;
	protected ArrayList<TableContainer> actionTables; 
	protected HashMap<String, TableHandler> tableHandlers;
	
	public History(String decisionSettings, String separator, HashMap<String, TableHandler> tableHandlers) throws MaltChainedException {
		setTableHandlers(tableHandlers);
		setSeparator(separator);
		initDecisionSettings(decisionSettings);
		actionPool = new ObjectPoolList<ComplexDecisionAction>() {
			protected ComplexDecisionAction create() throws MaltChainedException { return new ComplexDecisionAction(getThis()); }
			public void resetObject(ComplexDecisionAction o) throws MaltChainedException { o.clear(); }
		};
		clear();
	}
	
	private History getThis() {
		return this;
	}
	
	/* GuideUserHistory interface */
	public GuideUserAction getEmptyGuideUserAction() throws MaltChainedException {
		return (GuideUserAction)getEmptyActionObject();
	}
	
	public ArrayList<ActionContainer> getActionContainers() {
		ArrayList<ActionContainer> actionContainers = new ArrayList<ActionContainer>();
		for (int i=0; i<actionTables.size(); i++) {
			actionContainers.add(new ActionContainer(actionTables.get(i)));
		}
		return actionContainers;
	}
	
	public ActionContainer[] getActionContainerArray() {
		ActionContainer[] actionContainers = new ActionContainer[actionTables.size()];
		for (int i=0; i<actionTables.size(); i++) {
			actionContainers[i] = new ActionContainer(actionTables.get(i));
		}
		return actionContainers;
	}
	
	
	public void clear() throws MaltChainedException {
		actionPool.checkInAll();
	}
	
//	public void clear() {
//		currentAction = -1;
//	}
	
	/* GuideHistory interface */
	public GuideDecision getEmptyGuideDecision() throws MaltChainedException {
		return (GuideDecision)getEmptyActionObject();
	}
	
	public int getNumberOfDecisions() {
		return decisionTables.size();
	}
	
	public TableHandler getTableHandler(String name) {
		return tableHandlers.get(name);
	}

	public Class<? extends KBestList> getKBestListClass() {
		return kBestListClass;
	}
	
	public void setKBestListClass(Class<?> kBestListClass) throws MaltChainedException {
		try {
			if (kBestListClass != null) {
				this.kBestListClass = kBestListClass.asSubclass(org.maltparser.parser.history.kbest.KBestList.class);
			}
		} catch (ClassCastException e) {
			throw new HistoryException("The class '"+kBestListClass.getName()+"' is not a subclass of '"+org.maltparser.parser.history.kbest.KBestList.class.getName()+"'. ", e);
		}
	}
	
	public int getKBestSize() {
		return kBestSize;
	}

	public void setKBestSize(int kBestSize) {
		this.kBestSize = kBestSize;
	}

	public int getNumberOfActions() {
		return actionTables.size();
	}
	
	public ArrayList<TableContainer> getDecisionTables() {
		return decisionTables;
	}

	public ArrayList<TableContainer> getActionTables() {
		return actionTables;
	}

	public HashMap<String, TableHandler> getTableHandlers() {
		return tableHandlers;
	}
	
	public String getSeparator() {
		return separator;
	}

	public void setSeparator(String separator) throws MaltChainedException {
		if (separator == null || separator.length() < 1) {
			throw new HistoryException("The class item separator (--guide-classitem_separator) does not have correct value. ");
		}
		this.separator = separator;
	}

	public String getDecisionSettings() {
		return decisionSettings;
	}

	public void setDecisionSettings(String decisionSettings) {
		this.decisionSettings = decisionSettings;
	}

	protected void setTableHandlers(HashMap<String, TableHandler> tableHandlers) {
		this.tableHandlers = tableHandlers;
	}
	
	protected ActionDecision getEmptyActionObject() throws MaltChainedException {
		return actionPool.checkOut();
	}
	
	protected void initDecisionSettings(String decisionSettings) throws MaltChainedException {
		decisionTables = new ArrayList<TableContainer>();
		actionTables = new ArrayList<TableContainer>();
		this.decisionSettings = decisionSettings;
		int start = 0;
		int k = 0;
		char prevDecisionSeparator = ' ';
		TableContainer tmp = null;
		final StringBuilder sbTableHandler = new StringBuilder();
		final StringBuilder sbTable = new StringBuilder();
		int state = 0;
		for (int i = 0; i < decisionSettings.length(); i++) {
			switch (decisionSettings.charAt(i)) {
			case '.':
				if (state != 0) {
					//error
				}
				state = 1;
				break;
			case '+':
				tmp = new TableContainer(tableHandlers.get(sbTableHandler.toString()).getSymbolTable(sbTable.toString()), sbTableHandler.toString()+"."+sbTable.toString(), '+');
				actionTables.add(tmp);
				k++;
				sbTableHandler.setLength(0);
				sbTable.setLength(0);
				state = 0;
				break;
			case '#':
				state = 2;
				break;
			case ';':
				state = 2;
				break;
			case ',':
				state = 2;
				break;
			default:
				if (state == 0) {
					sbTableHandler.append(decisionSettings.charAt(i));
				} else if (state == 1) {
					sbTable.append(decisionSettings.charAt(i));
				}
			}
			if (state == 2 || i == decisionSettings.length()-1) {
				char decisionSeparator = decisionSettings.charAt(i);
				if (i == decisionSettings.length()-1) {
					//decisionSeparator = ' ';
					decisionSeparator = prevDecisionSeparator;
				}
				tmp = new TableContainer(tableHandlers.get(sbTableHandler.toString()).getSymbolTable(sbTable.toString()), sbTableHandler.toString()+"."+sbTable.toString(), decisionSeparator);
				actionTables.add(tmp);
				k++;
				if (k-start > 1) {
					decisionTables.add(new CombinedTableContainer(getTableHandler("A"), separator, actionTables.subList(start, k), decisionSeparator));
				} else {
					decisionTables.add(tmp);
				}
				sbTableHandler.setLength(0);
				sbTable.setLength(0);
				state = 0;
				start = k;
				prevDecisionSeparator = decisionSeparator;
			}
		}
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		return sb.toString();
	}
}
