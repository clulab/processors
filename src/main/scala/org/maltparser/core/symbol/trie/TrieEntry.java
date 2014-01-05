package org.maltparser.core.symbol.trie;

/**

@author Johan Hall
@since 1.0
*/
public class TrieEntry {
	private int code;
//	private boolean known;
	
	public TrieEntry(int code, boolean known) {
		this.code = code;
//		this.known = known;
	}

	public int getCode() {
		return code;
	}
	
//	public boolean isKnown() {
//		return known;
//	}
//	
//	public void setKnown(boolean known) {
//		this.known = known;
//	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return code == ((TrieEntry)obj).code; // && known == ((TrieEntry)obj).known;
	}


	public int hashCode() {
		return 	31 * 7 + code;
//		int hash = 7;
//		hash = 31 * hash + code;
//		return 31 * hash + (known ? 1 : 0);
	}


	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(code);
//		sb.append(' ');
//		sb.append(known);
		return sb.toString();
	}
}
