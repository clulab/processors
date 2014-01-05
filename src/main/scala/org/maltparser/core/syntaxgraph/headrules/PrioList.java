package org.maltparser.core.syntaxgraph.headrules;

import java.util.ArrayList;

import org.apache.log4j.Logger;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.syntaxgraph.node.NonTerminalNode;
import org.maltparser.core.syntaxgraph.node.PhraseStructureNode;
/**
*
*
* @author Johan Hall
*/
public class PrioList extends ArrayList<PrioSet> {
	public static final long serialVersionUID = 8045568022124816323L;
	protected HeadRule headRule;
	protected Direction direction;

	public PrioList(HeadRule headRule, String listSpec) throws MaltChainedException {
		setHeadRule(headRule);
		init(listSpec);
	}
	
	public void init(String listSpec) throws MaltChainedException {
		String spec = listSpec.trim();
		if (spec.length() < 8) {
			throw new HeadRuleException("The specification of the priority list is not correct '"+listSpec+"'. ");
		}
		if (spec.charAt(0) == 'r') {
			direction = Direction.RIGHT;	
		} else if (spec.charAt(0) == 'l') {
			direction = Direction.LEFT;
		} else {
			throw new HeadRuleException("Could not determine the direction of the priority list '"+listSpec+"'. ");
		}
		if (spec.charAt(1) == '[' && spec.charAt(spec.length()-1) == ']') {
			String[] items = spec.substring(2,spec.length()-1).split(" ");
			for (int i=0; i<items.length; i++) {
				add(new PrioSet(this, items[i]));
			}
		} else {
			throw new HeadRuleException("The specification of the priority list is not correct '"+listSpec+"'. ");
		}
	}
	
	public PhraseStructureNode getHeadChild(NonTerminalNode nt) throws MaltChainedException {
		PhraseStructureNode headChild = null;
		for (int i = 0, n = size(); i < n; i++) {
			headChild = get(i).getHeadChild(nt, direction);
			if (headChild != null) {
				break;
			}
		}
		return headChild; 
	}
	
	public Logger getLogger() {
		return headRule.getLogger();
	}
	
	public DataFormatInstance getDataFormatInstance() {
		return headRule.getDataFormatInstance();
	}
	
	public HeadRule getHeadRule() {
		return headRule;
	}

	public void setHeadRule(HeadRule headRule) {
		this.headRule = headRule;
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
		if (direction == Direction.LEFT) {
			sb.append("l[");
		} else if (direction == Direction.RIGHT) {
			sb.append("r[");
		}
		for (PrioSet set : this) {
			sb.append(set);
			sb.append(' ');
		}
		if (sb.length() != 0) {
			sb.setLength(sb.length()-1);
		}
		sb.append("]");
		return sb.toString();
	}


}
