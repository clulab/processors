package org.maltparser.core.symbol.trie;

import org.maltparser.core.helper.HashMap;

import org.maltparser.core.symbol.SymbolException;

/**

@author Johan Hall
*/
public class TrieNode {
	/**
	 * Initial capacity of the hash maps.
	 */
//	private final static int INITIAL_CAPACITY = 2;
	/**
	 * the character that corresponds to the trie node
	 */
	private final char character;
	/**
	 * Maps a symbol table into an entry (if not cached)
	 */
	private HashMap<TrieSymbolTable,Integer> entries;
	/**
	 * Maps a symbol table (cachedKeyEntry) into an entry (cachedValueEntry), caches only the first occurrence.
	 */
	private TrieSymbolTable cachedKeyEntry;
	private Integer cachedValueEntry;

	/**
	 * Maps a character into a child trie node (if not cached)
	 */
	private HashMap<Character,TrieNode> children;
	private char cachedKeyChar;
	private TrieNode cachedValueTrieNode;

	/**
	 * The parent trie node
	 */
	private final TrieNode parent;
	
	/**
	 * Constructs a trie node
	 * 
	 * @param character	which character that the trie node belongs to
	 * @param parent the parent trie node
	 */
	public TrieNode(char character, TrieNode parent) {
		this.character = character;
		this.parent = parent;
	}
	
	/**
	 * Adds and/or retrieve a child trie node. It only adds a entry if the parameter isWord is true.
	 * 
	 * @param isWord true if it is a word (entry), otherwise false
	 * @param c	the character to the child node
	 * @param table	which symbol table to look in or add to
	 * @param code	the integer representation of the string value
	 * @return the child trie node that corresponds to the character
	 * @throws SymbolException
	 */
	public TrieNode getOrAddChild(boolean isWord, char c, TrieSymbolTable table, int code) throws SymbolException {
		if (cachedValueTrieNode == null) {
			cachedValueTrieNode = new TrieNode(c, this);
			cachedKeyChar = c;
			if (isWord) {
				cachedValueTrieNode.addEntry(table, code);
			} 
			return cachedValueTrieNode;
		} else if (cachedKeyChar == c) {
			if (isWord) {
				cachedValueTrieNode.addEntry(table, code);
			} 
			return cachedValueTrieNode;
		} else {
			TrieNode child = null; 
			if (children == null) {
				children = new HashMap<Character, TrieNode>();
//				children = new HashMap<Character, TrieNode>(INITIAL_CAPACITY);
				child = new TrieNode(c, this);
				children.put(c,child);
			} else {
				child = children.get(c);
				if (child == null) {
					child = new TrieNode(c, this);
					children.put(c,child);
				}
			}
			if (isWord) {
				child.addEntry(table, code);
			} 
			return child;
		}
	} 
	
	/**
	 * Adds an entry if it does not exist
	 * 
	 * @param table which symbol table to add an entry
	 * @param code the integer representation of the string value
	 * @throws SymbolException
	 */
	private void addEntry(TrieSymbolTable table, int code) throws SymbolException {
		if (table == null) {
			throw new SymbolException("Symbol table cannot be found. ");
		}
		if (cachedValueEntry == null) {
			if (code != -1) {
				cachedValueEntry = code; //new TrieEntry(code,true);
				table.updateValueCounter(code);
			} else {
				cachedValueEntry = table.increaseValueCounter(); //new TrieEntry(table.increaseValueCounter(),false);
			}
			cachedKeyEntry = table; 
		} else if (!table.equals(cachedKeyEntry)) {
			if (entries == null) {
				entries = new HashMap<TrieSymbolTable, Integer>();
//				entries = new HashMap<TrieSymbolTable, TrieEntry>(INITIAL_CAPACITY);
			}
			if (!entries.containsKey(table)) {
				if (code != -1) {
					entries.put(table, code); //new TrieEntry(code,true));
					table.updateValueCounter(code);
				} else {
					entries.put(table, table.increaseValueCounter()); //new TrieEntry(table.increaseValueCounter(),false));
				}
			}
		}
	}
	
	/**
	 * Returns the child node that corresponds to the character
	 * 
	 * @param c the character of the child node
	 * @return the child node
	 */
	public TrieNode getChild(char c) {
		if (cachedKeyChar == c) {
			return cachedValueTrieNode;
		} else if (children != null) {
			return children.get(c);
		}
		return null;
	}
	

	
	/**
	 * Returns the entry of the symbol table 'table'
	 * 
	 * @param table	which symbol table
	 * @return the entry of the symbol table 'table'
	 */
	public Integer getEntry(TrieSymbolTable table) {
		if (table != null) {
			if (table.equals(cachedKeyEntry)) {
				return cachedValueEntry;
			} else if (entries != null) {
				return entries.get(table);
			}
		}
		return null;
	}

	/**
	 * Returns the character of the trie node
	 * 
	 * @return the character of the trie node
	 */
	public char getCharacter() {
		return character;
	}
	
	/**
	 * Returns the parent node
	 * 
	 * @return the parent node
	 */
	public TrieNode getParent() {
		return parent;
	}
	
	public boolean equals(Object obj) {
		return super.equals(obj);
	}

	public int hashCode() {
		return super.hashCode();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(character);
		return sb.toString();
	}
}
