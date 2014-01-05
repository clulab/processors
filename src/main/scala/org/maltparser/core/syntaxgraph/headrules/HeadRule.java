package org.maltparser.core.syntaxgraph.headrules;

import java.util.ArrayList;

import org.apache.log4j.Logger;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.node.NonTerminalNode;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;
/**
*
*
* @author Johan Hall
*/
public class HeadRule extends ArrayList<PrioList> {
	public static final long serialVersionUID = 8045568022124826323L;
	protected HeadRules headRules;
	protected SymbolTable table;
	protected int symbolCode;
	protected Direction defaultDirection;
	
	public HeadRule(HeadRules headRules, String ruleSpec) throws MaltChainedException {
		setHeadRules(headRules);
		init(ruleSpec);
	}
	
	public void init(String ruleSpec) throws MaltChainedException {
		String spec = ruleSpec.trim();
		String[] items = spec.split("\t");
		if (items.length != 3) {
			throw new HeadRuleException("The specification of the head rule is not correct '"+ruleSpec+"'. ");
		}
		
		int index = items[0].indexOf(':');
		if (index != -1) {
			SymbolTable t = getDataFormatInstance().getSymbolTables().getSymbolTable(items[0].substring(0, index));
			if (t == null) {
				throw new HeadRuleException("The specification of the head rule is not correct '"+ruleSpec+"'. ");
			}
			setTable(t);
			setSymbolCode(table.addSymbol(items[0].substring(index+1)));
		} else {
			throw new HeadRuleException("The specification of the head rule is not correct '"+ruleSpec+"'. ");
		}
		if (items[1].charAt(0) == 'r') {
			defaultDirection = Direction.RIGHT;	
		} else if (items[1].charAt(0) == 'l') {
			defaultDirection = Direction.LEFT;
		} else {
			throw new HeadRuleException("Could not determine the default direction of the head rule '"+ruleSpec+"'. ");
		}
		if (items[2].length() > 1) {
			if (items[2].indexOf(';') == -1) {
				add(new PrioList(this, items[2]));
			} else {
				String[] lists = items[2].split(";");
				for (int i = 0; i < lists.length; i++) {
					add(new PrioList(this, lists[i]));
				}
			}
		}
	}

	public PhraseStructureNode getHeadChild(NonTerminalNode nt) throws MaltChainedException {
		PhraseStructureNode headChild = null;
		for (int i = 0; i < size(); i++) {
			headChild = get(i).getHeadChild(nt);
			if (headChild != null) {
				break;
			}
		}
		return headChild;
	}
	
	public SymbolTable getTable() {
		return table;
	}
	
	public void setTable(SymbolTable table) {
		this.table = table;
	}
	
	public int getSymbolCode() {
		return symbolCode;
	}
	
	public void setSymbolCode(int symbolCode) {
		this.symbolCode = symbolCode;
	}
	
	public String getSymbolString() throws MaltChainedException {
		return table.getSymbolCodeToString(symbolCode);
	}
	
	public Direction getDefaultDirection() {
		return defaultDirection;
	}
	
	public void setDefaultDirection(Direction direction) {
		this.defaultDirection = direction;
	}
	
	public Logger getLogger() {
		return headRules.getLogger();
	}
	
	public void setHeadRules(HeadRules headRules) {
		this.headRules = headRules;
	}
	
	public DataFormatInstance getDataFormatInstance() {
		return headRules.getDataFormatInstance();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(table.getName());
		sb.append(':');
		try {
			sb.append(getSymbolString());
		} catch (MaltChainedException e) {
			if (getLogger().isDebugEnabled()) {
				getLogger().debug("",e);
			} else {
				getLogger().error(e.getMessageChain());
			}
		}
		sb.append('\t');
		if (defaultDirection == Direction.LEFT) {
			sb.append('l');
		} else if (defaultDirection == Direction.RIGHT) {
			sb.append('r');
		}
		sb.append('\t');
		if (size() == 0) {
			sb.append('*');
		} else {
			for (int i = 0; i < size(); i++) {
				sb.append(get(i));
				if (i < size()-1) {
					sb.append(';');
				}
			}
		}
		return sb.toString();
	}
}
