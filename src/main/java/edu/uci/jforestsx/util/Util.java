/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.uci.jforestsx.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Util {

	public static int putLongInByteArray(long l, byte[] array, int offset) {
		int i, shift;
		for (i = 0, shift = 56; i < 8; i++, shift -= 8) {
			array[offset] = (byte) (0xFF & (l >> shift));
			offset++;
		}
		return offset;
	}

	public static int putIntInByteArray(int value, byte[] array, int offset) {
		for (int i = 0; i < 4; i++) {
			int off = (3 - i) * 8;
			array[offset] = (byte) ((value >>> off) & 0xFF);
			offset++;
		}
		return offset;
	}
	
	public static int putBooleanInByteArray(boolean value, byte[] array, int offset) {
		array[offset] = (byte) (value ? 1 : 0);
		return offset + 1;
	}

	public static int putShortInByteArray(short value, byte[] array, int offset) {
		array[offset] = (byte) ((value >>> 8) & 0xFF);
		array[offset + 1] = (byte) (value & 0xFF);
		return offset + 2;
	}

	public static int putFloatInByteArray(float f, byte[] array, int offset) {
		int i = Float.floatToIntBits(f);
		return putIntInByteArray(i, array, offset);
	}

	public static int putDoubleInByteArray(double d, byte[] array, int offset) {
		long l = Double.doubleToRawLongBits(d);
		return putLongInByteArray(l, array, offset);
	}

	public static int putIntArrayInByteArray(int[] src, byte[] dest, int offset) {
		offset = Util.putIntInByteArray(src.length, dest, offset);

		for (int i = 0; i < src.length; i++) {
			offset = Util.putIntInByteArray(src[i], dest, offset);
		}
		return offset;
	}

	public static int putDoubleArrayInByteArray(double[] src, byte[] dest,
			int offset) {
		offset = Util.putIntInByteArray(src.length, dest, offset);

		for (int i = 0; i < src.length; i++) {
			offset = Util.putDoubleInByteArray(src[i], dest, offset);
		}
		return offset;
	}

	public static int putStringInByteArray(String src, byte[] dest, int offset) {
		if (src == null) {
			offset = Util.putShortInByteArray((short) 0, dest, offset);
		} else {
			char[] chars = src.toCharArray();
			offset = Util.putShortInByteArray((short) chars.length, dest,
					offset);

			for (int i = 0; i < chars.length; i++) {
				dest[offset] = (byte) chars[i];
				offset++;
			}
		}
		return offset;
	}

	public static byte[] toByteArray(int value) {
		byte[] array = new byte[4];
		putIntInByteArray(value, array, 0);
		return array;
	}

	public static long toLong(byte[] bytearray, int offset) {
		long result = 0;
		for (int i = offset; i < offset + 8 /* Bytes in long */; i++) {
			result = (result << 8 /* Bits in byte */)
					| (0xff & (byte) (bytearray[i] & 0xff));
		}
		return result;
	}

	public static int toInt(byte[] b, int offset) {
		int value = 0;
		for (int i = 0; i < 4; i++) {
			int shift = (4 - 1 - i) * 8;
			value += (b[i + offset] & 0x000000FF) << shift;
		}
		return value;
	}

	public static short toShort(byte[] b, int offset) {
		return (short) (((b[offset] & 0x000000FF) << 8) + ((b[offset + 1] & 0x000000FF)));
	}
	
	public static boolean toBoolean(byte[] data, int offset) {
		return (data[offset] == 1);
	}

	public static float toFloat(byte[] data, int offset) {
		int i = toInt(data, offset);
		return Float.intBitsToFloat(i);
	}

	public static double toDouble(byte[] data, int offset) {
		long l = toLong(data, offset);
		return Double.longBitsToDouble(l);
	}

	public static int[] toIntArray(byte[] data, int offset) {
		int length = toInt(data, offset);
		offset += 4;
		int[] arr = new int[length];
		for (int i = 0; i < length; i++) {
			arr[i] = toInt(data, offset);
			offset += 4;
		}
		return arr;
	}

	public static String toString(byte[] data, int offset) {
		int length = toShort(data, offset);
		offset += 2;
		char[] chars = new char[length];
		for (int i = 0; i < length; i++) {
			chars[i] = (char) data[offset];
			offset++;
		}
		return new String(chars);
	}
	
	public static String getFileNameWithoutExtension(String name) {
		int idx = name.lastIndexOf('.');
		if (idx < 0) {
			return name;
		}
		return name.substring(0, idx);
	}
	
	public static List<Integer> loadIntegersFromFile(String filename) throws Exception {
		List<Integer> result = new ArrayList<Integer>();
		BufferedReader reader = new BufferedReader(new FileReader(new File(filename)));
		String line;
		while ((line = reader.readLine()) != null) {
			result.add(Integer.parseInt(line.trim()));
		}
		return result;
	}
}
