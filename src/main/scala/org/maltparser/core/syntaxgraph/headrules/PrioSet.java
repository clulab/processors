package org.maltparser.core.syntaxgraph.headrules;

import java.util.ArrayList;

import org.apache.log4j.Logger;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.symbol.SymbolTable;
import org.maltparser.core.syntaxgraph.headrules.PrioSetMember.RelationToPrevMember;
import org.maltparser.core.syntaxgraph.node.NonTerminalNode;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;
import org.maltparser.core.syntaxgraph.node.TokenNode;
/**
*
*
* @author Johan Hall
*/
public class PrioSet extends ArrayList<PrioSetMember> {
	public static final long serialVersionUID = 8045568022124816313L;
	protected PrioList prioList;
	protected PrioSetMember cache;
	
	public PrioSet(PrioList prioList) {
		setPrioList(prioList);
		cache = new PrioSetMember(this, null, null, -1, RelationToPrevMember.START);
	}
	
	public PrioSet(PrioList prioList, String setSpec) throws MaltChainedException {
		setPrioList(prioList);
		cache = new PrioSetMember(this, null, null, -1, RelationToPrevMember.START);
		init(setSpec);
	}
	
	public void init(String setSpec) throws MaltChainedException {
		String spec = setSpec.trim();
		String[] disItems = spec.split("\\|");
		for (int i = 0; i < disItems.length; i++) {
			String[] conItems = spec.split("\\&");
			for (int j = 0; j < conItems.length; j++) {
				int index = conItems[j].indexOf(':');
				if (index != -1) {
					SymbolTable table = prioList.getDataFormatInstance().getSymbolTables().getSymbolTable(conItems[j].substring(0, index));
					ColumnDescription column = prioList.getDataFormatInstance().getColumnDescriptionByName(conItems[j].substring(0, index));
					if (i == 0 && j == 0) {
						addPrioSetMember(table, column, conItems[j].substring(index+1), RelationToPrevMember.START);
					} else if (j == 0) {
						addPrioSetMember(table, column, conItems[j].substring(index+1), RelationToPrevMember.DISJUNCTION);
					} else {
						addPrioSetMember(table, column, conItems[j].substring(index+1), RelationToPrevMember.CONJUNCTION);
					}
				} else {
					throw new HeadRuleException("The specification of the priority list is not correct '"+setSpec+"'. ");
				}
			}
		}
	}
	
	public PrioSetMember addPrioSetMember(SymbolTable table, ColumnDescription column, String symbolString, RelationToPrevMember relationToPrevMember) throws MaltChainedException {
		if (table == null) {
			throw new HeadRuleException("Could add a member to priority set because the symbol table could be found. ");
		}
		return this.addPrioSetMember(table, column, table.addSymbol(symbolString), relationToPrevMember);
	}
	
	public PrioSetMember addPrioSetMember(SymbolTable table, ColumnDescription column, int symbolCode, RelationToPrevMember relationToPrevMember) throws MaltChainedException {
		cache.setTable(table);
		cache.setSymbolCode(symbolCode);
		if (!contains(cache)) {
			PrioSetMember newItem = new PrioSetMember(this, table, column, symbolCode, relationToPrevMember);
			add(newItem);
			return newItem;
		}
		return cache;
	}
	
	public PhraseStructureNode getHeadChild(NonTerminalNode nt, Direction direction) throws MaltChainedException {
		boolean match = false;
		if (direction == Direction.LEFT) {
			for (PhraseStructureNode child : nt.getChildren()) {
				for (int j = 0; j < size(); j++) {
					match = matchHeadChild(child, get(j));
					if (match == true) {
						if (j+1 >= size()) {
							return child;
						} else if (get(j).getRelationToPrevMember() != RelationToPrevMember.CONJUNCTION) {
							return child;
						}
					}
				}
			}
		} else if (direction == Direction.RIGHT) {
			for (int i = nt.nChildren()-1; i >= 0; i--) {
				PhraseStructureNode child = nt.getChild(i);
				for (int j = 0; j < size(); j++) {
					match = matchHeadChild(child, get(j));
					if (match == true) {
						if (j+1 >= size()) {
							return child;
						} else if (get(j).getRelationToPrevMember() != RelationToPrevMember.CONJUNCTION) {
							return child;
						}
					}
				}
			}
		}
		return null;
	}
	
	private boolean matchHeadChild(PhraseStructureNode child, PrioSetMember member) throws MaltChainedException {
		if (child instanceof NonTerminalNode && member.getTable().getName().equals("CAT") && member.getSymbolCode() == child.getLabelCode(member.getTable())) {
			return true;
		} else if (member.getTable().getName().equals("LABEL") && member.getSymbolCode() == child.getParentEdgeLabelCode(member.getTable())) {
			return true;
		} else if (child instanceof TokenNode && member.getColumn().getCategory() == ColumnDescription.INPUT && member.getSymbolCode() == child.getLabelCode(member.getTable())) {
			return true;
		}
		return false;
	}
	
	public Logger getLogger() {
		return prioList.getLogger();
	}

	public PrioList getPrioList() {
		return prioList;
	}

	protected void setPrioList(PrioList prioList) {
		this.prioList = prioList;
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return super.equals(obj);
	}
	
	public int hashCode() {
		return super.hashCode();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		for (int i = 0; i < size(); i++) {
			if (i != 0) {
				if (get(i).getRelationToPrevMember() == RelationToPrevMember.CONJUNCTION) {
					sb.append('&');
				} else if (get(i).getRelationToPrevMember() == RelationToPrevMember.DISJUNCTION) {
					sb.append('|');
				}
			}
			sb.append(get(i));
		}
		return sb.toString();
	}
}
