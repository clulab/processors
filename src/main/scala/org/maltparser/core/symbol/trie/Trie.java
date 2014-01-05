package org.maltparser.core.symbol.trie;


import org.maltparser.core.symbol.SymbolException;

/**
*
* @author Johan Hall
* @since 1.0
**/
public class Trie {
	private final TrieNode root;
	private final StringBuilder sb;


	
	public Trie() {
		root = new TrieNode(' ', null);
		sb = new StringBuilder();
	}
	
	public TrieNode addValue(String value, TrieSymbolTable table, int code) throws SymbolException {
		TrieNode node = root;
		final char[] chars = value.toCharArray();
		for (int i = chars.length-1; i>=0; i--) {
			if (i == 0) {
				node = node.getOrAddChild(true, chars[i], table, code);
			} else {
				node = node.getOrAddChild(false, chars[i], table, code);
			}
		}
		return node;
	}
	
	public TrieNode addValue(StringBuilder symbol, TrieSymbolTable table, int code) throws SymbolException {
		TrieNode node = root;
		for (int i = symbol.length()-1; i>=0; i--) {
			if (i == 0) {
				node = node.getOrAddChild(true, symbol.charAt(i), table, code);
			} else {
				node = node.getOrAddChild(false, symbol.charAt(i), table, code);
			}
		}
		return node;
	}
	
	public String getValue(TrieNode node, TrieSymbolTable table) {
		sb.setLength(0);
		TrieNode tmp = node;
		while (tmp != root) { // && tmp != null) {
			sb.append(tmp.getCharacter());
			tmp = tmp.getParent();
		}
		return sb.toString();
	}
	
	public Integer getEntry(String value, TrieSymbolTable table) {
		TrieNode node = root;
		final char[] chars = value.toCharArray();
		int i=chars.length-1;
		for (;i>=0 && node != null;i--) {
			node = node.getChild(chars[i]);
		}
		if (i < 0 && node != null) {
			return node.getEntry(table);
		} 
		return null;
	}
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return ((root == null) ? ((Trie)obj).root == null : root.equals(((Trie)obj).root));
	}

	public int hashCode() {
		return 31 * 7 + (null == root ? 0 : root.hashCode());
	}
}
