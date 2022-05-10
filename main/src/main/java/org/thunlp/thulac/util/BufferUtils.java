package org.thunlp.thulac.util;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.SeekableByteChannel;

/**
 * An utility class which deals with buffers.
 *
 * @see java.nio.Buffer
 */
public class BufferUtils {
	/**
	 * Read ints from {@code channel} using {@code buf} as buffer and putting them
	 * sequentially into the array of {@code int[]} represented by {@code arrays}.<br>
	 * {@code buf} is always in read mode after this method returns, that is, users
	 * have to call {@code buf.flip()} first if they wish to reuse it. {@code
	 * channel} is NOT closed after this method returns (since the EOF might not have been
	 * reached yet), therefore users should close it manually.<br>
	 *
	 * @param channel
	 * 		The {@link FileChannel} to read from.
	 * @param buf
	 * 		The {@link ByteBuffer} to use as buffer.
	 * @param arrays
	 * 		The array of {@code int[]} to store the read ints.
	 *
	 * @return A return value of {@code true} means that all the arrays are successfully
	 * filled with data read from {@code channel}, while {@code false} means that the
	 * EOF is reached before all the arrays are filled. In special case that all arrays
	 * are filled and EOF is reached, {@code true} is returned.
	 *
	 * @throws IOException
	 * 		If an exception is thrown while reading from {@code channel}.
	 * @throws NullPointerException
	 * 		If either channel is null, buf is null, or any element of {@code arrays} is
	 * 		null.
	 */
	public static boolean readInts(
			SeekableByteChannel channel, ByteBuffer buf, int[]... arrays)
			throws IOException {
		int position = 0, offset = 0;
		int[] current = arrays[position];
		int currentLeft = current.length, readBytes, readInts;

		while (true) {
			// read buffer
			readBytes = channel.read(buf);
			// if EOF is reached and there are still arrays left not filled
			if (readBytes == -1) return false;
			buf.flip();
			IntBuffer intBuf = buf.asIntBuffer();
			readInts = readBytes >> 2;

			// copy buffer content to arrays
			while (readInts > 0) {
				int getLen = Math.min(readInts, currentLeft);
				intBuf.get(current, offset, getLen);
				offset += getLen;
				readInts -= getLen;
				currentLeft -= getLen;

				if (currentLeft == 0) { // if current array is filled
					++position;
					if (position == arrays.length) { // if all arrays have been filled
						buf.clear();
						return true;
					}
					current = arrays[position];
					offset = 0;
					currentLeft = current.length;
				}
			}

			buf.clear();
		}
	}
}
