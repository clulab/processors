package org.maltparser.core.syntaxgraph.headrules;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.ColumnDescription;
import org.maltparser.core.symbol.SymbolTable;
/**
*
*
* @author Johan Hall
*/
public class PrioSetMember {
	protected enum RelationToPrevMember {
		START, DISJUNCTION, CONJUNCTION
	};
	protected PrioSet prioSet;
	protected SymbolTable table;
	protected ColumnDescription column;
	protected int symbolCode;
	protected RelationToPrevMember relationToPrevMember;
    
	public PrioSetMember(PrioSet prioSet, SymbolTable table, ColumnDescription column, int symbolCode, RelationToPrevMember relationToPrevMember) {
		setPrioSet(prioSet);
		setTable(table);
		setColumn(column);
		setSymbolCode(symbolCode);
		setRelationToPrevMember(relationToPrevMember);
	}

	public PrioSetMember(PrioSet prioSet, SymbolTable table, ColumnDescription column, String symbolString, RelationToPrevMember relationToPrevMember) throws MaltChainedException {
		setPrioSet(prioSet);
		setTable(table);
		setColumn(column);
		if (table != null) {
			setSymbolCode(table.getSymbolStringToCode(symbolString));
		} else {
			setSymbolCode(-1);
		}
		setRelationToPrevMember(relationToPrevMember);
	}
	
	public PrioSet getPrioSet() {
		return prioSet;
	}

	public void setPrioSet(PrioSet prioSet) {
		this.prioSet = prioSet;
	}

	
	public ColumnDescription getColumn() {
		return column;
	}

	public void setColumn(ColumnDescription column) {
		this.column = column;
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

	public String getSymbolString() throws MaltChainedException {
		if (table != null && symbolCode >= 0) {
			return table.getSymbolCodeToString(symbolCode);
		} else {
			return null;
		}
	}
	
	public void setSymbolCode(int symbolCode) {
		this.symbolCode = symbolCode;
	}
	
	public RelationToPrevMember getRelationToPrevMember() {
		return relationToPrevMember;
	}

	public void setRelationToPrevMember(RelationToPrevMember relationToPrevMember) {
		this.relationToPrevMember = relationToPrevMember;
	}
	
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + symbolCode;
		result = prime
				* result
				+ ((relationToPrevMember == null) ? 0 : relationToPrevMember
						.hashCode());
		return result;
	}


	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		PrioSetMember other = (PrioSetMember) obj;
		if (symbolCode != other.symbolCode)
			return false;
		if (relationToPrevMember == null) {
			if (other.relationToPrevMember != null)
				return false;
		} else if (!relationToPrevMember.equals(other.relationToPrevMember))
			return false;

		return true;
	}

	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(table.getName());
		sb.append(':');
		try {
			sb.append(getSymbolString());
		} catch (MaltChainedException e) {
			if (prioSet.getLogger().isDebugEnabled()) {
				prioSet.getLogger().debug("",e);
			} else {
				prioSet.getLogger().error(e.getMessageChain());
			}
		}
		return sb.toString();
	}
}
