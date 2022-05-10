package org.thunlp.thulac.data;

import org.thunlp.thulac.util.BufferUtils;
import org.thunlp.thulac.util.StringUtils;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * A class which loads data files from disk and provide necessary operations. Instances
 * are created with the {@link #Dat(String)} constructor which reads from a file of
 * with {@link DatMaker#readFromTxtFile(String)} which constructs a {@code Dat}
 * structure with the user-specified dictionary.<br>
 * Internally, {@code Dat} uses the two-array Trie Tree to store information that can
 * be searched though at high speed, (sometimes) even faster than using
 * {@link java.util.HashMap}.
 */
public class Dat {
	/**
	 * The two-array Trie Tree, use {@code dat[i << 1]} to access {@code base[i]} and
	 * {@code dat[(i << 1) + 1]} to access {@code check[i]}.
	 */
	public int[] dat;
	/**
	 * The size of the Trie Tree, should be {@code this.dat.length / 2}.
	 */
	public int datSize;

	protected Dat(int size) {
		this.dat = new int[size << 1];
		this.datSize = size;
	}

	/**
	 * Read a {@link Dat} from a given file.
	 *
	 * @param filename
	 * 		The name of the {@link Dat} file.
	 *
	 * @throws IOException
	 * 		If an I/O error occurred while reading the file.
	 */
	public Dat(String filename) throws IOException {
		SeekableByteChannel channel = Files.newByteChannel(Paths.get(filename));
		// DWORD base + DWORD check -> 8 bytes per record
		this.datSize = (int) (channel.size() >> 3);
		this.dat = new int[this.datSize << 1];
		// strange though, dat files are stored little endian
		ByteBuffer bb = ByteBuffer.allocateDirect(64 * 1024)
				.order(ByteOrder.LITTLE_ENDIAN);
		bb.clear();
		if (!BufferUtils.readInts(channel, bb, this.dat))
			throw new IOException("File does not contain enough data!");
		channel.close();
	}

	// if word in dat, return leaf element, otherwise return -1
	private int match(String word) {
		int ind = 0;
		int base = 0;
		int[] codePoints = StringUtils.toCodePoints(word);
		for (int c : codePoints) {
			ind = this.dat[ind << 1] + c;
			if (ind >= this.datSize || this.dat[(ind << 1) + 1] != base) return -1;
			base = ind;
		}
		ind = this.dat[base << 1];
		return ind < this.datSize && this.dat[(ind << 1) + 1] == base ? ind : -1;
	}

	// if prefix in dat, return -base, otherwise return longest substring of prefix in dat
	public int getInfo(String prefix) {
		int ind = 0;
		int base = 0;
		for (int i = 0; i < prefix.length(); i++) {
			ind = this.dat[ind << 1] + prefix.charAt(i);
			if (ind >= this.datSize || this.dat[(ind << 1) + 1] != base) return i;
			base = ind;
		}
		return -base;
	}

	/**
	 * Returns whether this {@link Dat} contains one or more words that begin with
	 * {@code prefix}.
	 *
	 * @param prefix
	 * 		The query prefix.
	 *
	 * @return Whether this {@link Dat} contains one or more words that begin with
	 * {@code prefix}.
	 */
	public boolean containsPrefix(String prefix) {
		return getInfo(prefix) < 0;
	}

	/**
	 * Returns whether this {@link Dat} contains the given word.
	 *
	 * @param word
	 * 		The query word.
	 *
	 * @return Whether this {@link Dat} contains {@code word}.
	 */
	public boolean contains(String word) {
		return this.match(word) != -1;
	}
}
