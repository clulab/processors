package org.thunlp.thulac.data;

import java.io.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Vector;

/**
 * A class used to construct instances of {@link Dat} from user-specified dictionary
 * files. It extends {@link Dat} to avoid unnecessary array copies and to increase
 * performance.<br>
 * A confusing algorithm is used to construct the two-array Trie Tree used by
 * {@link Dat}, see in-line comments for more information.
 */
public class DatMaker extends Dat {
	// a record of a word with an related integer
	private static class Record {
		public String word;
		public int num;

		public Record() {
			this("", 0);
		}

		public Record(String key, int value) {
			this.word = key;
			this.num = value;
		}
	}

	// pairs of Records are compared by comparing their words
	private static Comparator<Record> RECORDS_COMPARATOR =
			new Comparator<Record>() {
				@Override
				public int compare(Record a, Record b) {
					return a.word.compareTo(b.word);
				}
			};

	/**
	 * Reads (or more precisely, constructs) an instance of {@link Dat} from the given
	 * {@link InputStream}. This is used to generate {@link Dat} from a user-specified
	 * dictionary, which consists of multiple lines, each one representing a word in the
	 * dictionary.
	 *
	 * @param in
	 * 		The {@link InputStream} to read.
	 *
	 * @return The generated {@link Dat}.
	 *
	 * @throws IOException
	 * 		If an I/O error happens.
	 */
	public static Dat readFromInputStream(InputStream in) throws IOException {
		List<String> words = new ArrayList<>();
		BufferedReader reader = new BufferedReader(new InputStreamReader(in));
		String str;
		while ((str = reader.readLine()) != null) words.add(str);
		reader.close();

		DatMaker dat = new DatMaker();
		dat.buildDat(words);
		return dat;
	}

	/**
	 * Reads (or more precisely, constructs) an instance of {@link Dat} from the given
	 * file. This is used to generate {@link Dat} from a user-specified dictionary,
	 * which consists of multiple lines, each one representing a word in the dictionary.
	 *
	 * @param filename
	 * 		The name of the file.
	 *
	 * @return The generated {@link Dat}.
	 *
	 * @throws IOException
	 * 		If the given file does not exist or is not readable.
	 */
	public static Dat readFromTxtFile(String filename) throws IOException {
		return readFromInputStream(new FileInputStream(filename));
	}

	// The main idea of this ingenious algorithm that generates a Dat instance from the
	// input string is that it makes use of the unused space of the original double-array
	// Trie Tree to store a double-linked list. This means that it is fully
	// compatible with the standard double-array Trie Tree data structure. What's more,
	// this algorithm achieves its goal without extra storage space, expect for the head
	// and tail fields. But these only require O(1) space, so they can be safely ignored.

	// this.dat, the only storage block used by this algorithm, is an
	// array of ELEMENTS. An ELEMENT contains two values, called BASE and CHECK, both
	// integers. this.dat is structured in this way:
	// ELEMENTS[0].BASE, ELEMENTS[0].CHECK, ELEMENTS[1].BASE, ELEMENTS[1].CHECK, ...
	// this.datSize is the total number of ELEMENTS, so
	// this.dat.length = 2 * this.datSize.
	// In the following parts,BASE and CHECK will be referred to as the
	// FIELDS of an ELEMENT, for example, "the BASE FIELD of ELEMENT[4]".

	// The program distinguishes the two different data structures stored in this.dat by
	// the sign of the ELEMENTS' FIELDS.
	// ELEMENTS whose CHECK and BASE FIELDS are positive belong to the double-array Trie
	// Tree, while those whose CHECK and BASE FIELDS are negative belong to the
	// double-linked list. When an ELEMENT belongs to the Trie Tree, we call it USED.
	// Otherwise, we call it UNUSED.

	// Here the specific data structures are explained.
	// The data structure of the Trie Tree:
	// FIELDS of USED ELEMENTS strictly follow the definitions of the double-array Trie
	// Tree. (If unfamiliar, consult Google) For the current stage S and input
	// character C, we have:
	// ELEMENTS[ELEMENTS[S].BASE + C].CHECK = S
	// ELEMENTS[S].BASE + C = T
	// where T is the next stage the DFA (Deterministic Finite Automaton) described by
	// the Trie Tree should jump to.

	// The data structure of the double-linked list:
	// In a double-linked list there are multiple NODES, each containing two
	// pointers PREV and NEXT. In accordance with the c-style arrow (->) operator, this
	// list conforms to the following equations:
	// NODE->NEXT->PREV = NODE
	// NODE->PREV->NEXT = NODE
	// In this implementation, pointers take the negative of the values of the indices of
	// the NODES they point to. The PREV pointer is stored in the BASE field, and the
	// NEXT pointer in the CHECK field. We have,
	// -ELEMENTS[ -ELEMENTS[i].CHECK ].BASE = i
	// -ELEMENTS[ -ELEMENTS[i].BASE ].CHECK = i
	// The negative signs appear because fields of ELEMENTs in the double-linked list
	// are negative.

	// The pointers to the HEAD NODE and the TAIL NODE are stored in this.head and
	// this.tail, respectively. -this.head is the index of the first NODE in the
	// double-linked list, and -this.tail is the index of the last NODE.

	// After so many explanations of the data structure, we finally come to the
	// actual behavior of this algorithm.
	// The buildDat() method takes a list of strings as input and sorts them in
	// alphabetical order. Afterwards, findChildren() breaks strings - char sequences -
	// into a tree of characters, as described in the Trie Tree.
	// Since the Trie Tree is a representation of an DFA (Deterministic Finite
	// Automaton), a stage has to be generated for each node in the tree. Such a stage,
	// stored as ELEMENTS, have the BASE and CHECK FIELDS. The CHECK field of an ELEMENT
	// is assigned when its parent stage is generated. The assignment of the value in
	// BASE FIELD is implemented in allocate() and described below:

	// 1. Set variable BASE to this.head.
	// 2. Determine whether BASE is available. (If all ELEMENTS[BASE + C] are UNUSED
	//    for every C of the child nodes of the current one)
	// 3. If BASE is available, return BASE; otherwise, set BASE to the next UNUSED
	//    ELEMENT, using the double-linked list.
	// In this process, if no available BASE is found, the size of this.dat is doubled
	// through the expandDat() method, which also maintains the double-linked list in
	// the newly allocated ELEMENTS.

	// After an available BASE has been found for the current stage, markAsUsed()
	// is called with BASE and all BASE + C, updating the double-linked list.

	// Afterwards, populate() is called. It sets ELEMENTS[BASE + C].CHECK to S
	// for all C in the child nodes and sets ELEMENTS[S].BASE to BASE. ELEMENTS[S]
	// .CHECK is set to S if stage BASE can be the end of a word; otherwise, it is set
	// to BASE otherwise. For each word in lexicon, its corresponding leaf node in the
	// Trie Tree will have its BASE field set to the line number of the word. (Remember
	// that the user-specified dictionary consists of multiple lines, each one
	// representing a word in the dictionary.

	// Finally, method packDat() is invoked to minimize the size of this.dat and reduce
	// memory usage.

	private int head, tail;

	private DatMaker() {
		super(1);

		// initialize the double-linked list: head = 0, next = 1
		this.dat[0] = this.dat[1] = -1;
		this.head = this.tail = 0;
	}

	// mark element as used by modifying the double-linked list
	private void markAsUsed(int index) {
		// -base -> the previous element, -check -> the next element
		int base = this.dat[index << 1], check = this.dat[(index << 1) + 1];

		// if the the next element is already USED, print an error message
		if (check >= 0) throw new RuntimeException("Cell reused! Index: " + index);

		// maintain the double-linked list
		if (base == -1) this.head = check;
		else this.dat[((-base) << 1) + 1] = check;
		if (check == -this.datSize) this.tail = base;
		else this.dat[(-check) << 1] = base;

		this.dat[(index << 1) + 1] = index; // positive check: element used
	}

	// expand size of this.dat
	private void expandDat() {
		int oldSize = this.datSize;

		// alloc & copy
		this.datSize *= 2;
		int[] newDat = new int[this.dat.length << 1];
		System.arraycopy(this.dat, 0, newDat, 0, this.dat.length);
		this.dat = newDat;

		// expand the double-linked list
		for (int i = 0; i < oldSize; i++) {
			int pos = (oldSize + i) << 1;
			newDat[pos] = -(oldSize + i - 1);
			newDat[pos + 1] = -(oldSize + i + 1);
		}
		this.dat[oldSize << 1] = this.tail;
		this.dat[((-this.tail) << 1) + 1] = -oldSize;
		this.tail = -(oldSize * 2 - 1); // set tail to the last element
	}

	// remove unused elements to save memory
	private void packDat() {
		// calculate minimum size
		int last = this.datSize - 1;
		for (; this.dat[(last << 1) + 1] < 0; --last) ;
		this.datSize = last + 1;

		// truncate this.dat
		int[] newDat = new int[this.datSize << 1];
		System.arraycopy(this.dat, 0, newDat, 0, this.datSize << 1);
		this.dat = newDat;
	}

	// allocate elements according to offsets and return BASE
	private int allocate(List<Integer> offsets) {
		int size = offsets.size();
		int base = -this.head; // initialized to the head of the double-linked list
		while (true) {
			// expand this.dat as needed
			if (base == this.datSize) this.expandDat();
			if (size != 0) {
				// sorted, offsets.get(size - 1) is the greatest
				int requiredSize = base + offsets.get(size - 1);
				while (requiredSize >= this.datSize) this.expandDat();
			}

			boolean available = true; // check availability
			if (this.dat[(base << 1) + 1] >= 0) available = false; // ELEMENTS[BASE] USED
			else {
				// if any ELEMENTS[BASE + C] is USED, available = false
				int i = 0;
				for (; i < size && this.dat[(base + offsets.get(i) << 1) + 1] < 0; i++) ;
				if (i < size) available = false;
			}

			if (available) { // if BASE is available, update double-linked list
				this.markAsUsed(base);
				for (int offset : offsets) this.markAsUsed(base + offset);

				return base;
			}

			// find next BASE to check availability
			int newBase = -this.dat[(base << 1) + 1];
			if (newBase == this.datSize) this.expandDat(); // ensure capacity
			base = newBase;
		}
	}

	// find characters in lexicon which might follow the prefix
	private List<Integer> findChildren(List<Record> lexicon, int start, String prefix) {
		List<Integer> children = new ArrayList<>();
		int length = prefix.length(), currentChild = -1;
		for (int i = start, size = lexicon.size(); i < size; ++i) {
			String word = lexicon.get(i).word;
			if (!word.startsWith(prefix)) return children;
			if (word.length() == length) continue;
			int nextCh = word.charAt(length);
			if (nextCh != currentChild) children.add(currentChild = nextCh);
		}
		return children;
	}

	// populate BASE and CHECK FIELDS of allocated BASE and BASE + C
	// @param isWord Whether the end of a word has been reached.
	private int populate(int check, List<Integer> offsets, boolean isWord) {
		int base = this.allocate(offsets);

		this.dat[base << 1] = 0;
		this.dat[(base << 1) + 1] = isWord ? check : base;

		for (int offset : offsets) { // update Trie Tree
			int pos = base + offset << 1;
			this.dat[pos] = 0;
			this.dat[pos + 1] = check; // ELEMENTS[ELEMENTS[S].BASE + C].CHECK = S
		}
		this.dat[check << 1] = base; // ELEMENTS[CHECK].BASE = BASE

		return base;
	}

	// build the Dat structure with a word list as input
	private void buildDat(List<String> words) {
		// construct lexicon
		Vector<Record> lexicon = new Vector<>();
		lexicon.add(new Record());
		for (int i = 0, size = words.size(); i < size; ++i)
			lexicon.add(new Record(words.get(i), i));
		lexicon.sort(RECORDS_COMPARATOR); // sort input

		// root elements
		this.dat[0] = this.populate(0, this.findChildren(lexicon, 0, ""), true);

		for (int i = 0, size = lexicon.size(); i < size; i++) {
			String word = lexicon.get(i).word;

			int off = this.getInfo(word);
			if (off <= 0) off = word.length(); // if dat already contains word

			// iterate through characters after offset and add new entries
			for (int offset = off; offset <= word.length(); offset++) {
				String prefix = word.substring(0, offset);
				int pBase = -this.getInfo(prefix); // should always be positive
				this.populate(pBase, this.findChildren(lexicon, i, prefix),
						offset == word.length()); // on word end
			}

			off = -this.getInfo(word); // should always be positive
			this.dat[this.dat[off << 1] << 1] = lexicon.get(i).num; // leaf node value
		}

		this.packDat();
	}
}