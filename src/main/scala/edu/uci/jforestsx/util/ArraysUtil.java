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

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class ArraysUtil {

	public static void increment(double[] arr, double inc) {
		for (int i = 0; i < arr.length; i++) {
			arr[i] += inc;
		}
	}
	
	public static int findIndex(int[] arr, int value, int length) {
		for (int i = 0; i < length; i++) {
			if (arr[i] == value) {
				return i;
			}
		}
		return -1;
	}

	public static int findMinIndex(double[] arr, int offset, int length) {
		int minIndex = -1;
		double minValue = Double.MAX_VALUE;
		int endIdx = offset + length;
		for (int i = offset; i < endIdx; i++) {
			if (arr[i] < minValue) {
				minValue = arr[i];
				minIndex = i;
			}
		}
		return minIndex;
	}
	
	public static int findMinIndex(float[] arr, int offset, int length) {
		int minIndex = -1;
		float minValue = Float.MAX_VALUE;
		int endIdx = offset + length;
		for (int i = offset; i < endIdx; i++) {
			if (arr[i] < minValue) {
				minValue = arr[i];
				minIndex = i;
			}
		}
		return minIndex;
	}

	public static int findMinIndex(double[] arr) {
		return findMinIndex(arr, 0, arr.length);
	}

	public static int findMaxIndex(int[] arr) {
		return findMaxIndex(arr, 0, arr.length);
	}

	public static int findMaxIndex(int[] arr, int offset, int length) {
		int maxIndex = -1;
		int maxValue = Integer.MIN_VALUE;
		int endIdx = offset + length;
		for (int i = offset; i < endIdx; i++) {
			if (arr[i] > maxValue) {
				maxValue = arr[i];
				maxIndex = i;
			}
		}
		return maxIndex;
	}

	public static int findMaxIndex(double[] arr) {
		return findMaxIndex(arr, 0, arr.length);
	}

	public static int findMaxIndex(double[] arr, int offset, int length) {
		int maxIndex = -1;
		double maxValue = Double.NEGATIVE_INFINITY;
		int endIdx = offset + length;
		for (int i = offset; i < endIdx; i++) {
			if (arr[i] > maxValue) {
				maxValue = arr[i];
				maxIndex = i;
			}
		}
		return maxIndex;
	}
	
	public static int findMaxIndex(float[] arr, int offset, int length) {
		int maxIndex = -1;
		float maxValue = Float.NEGATIVE_INFINITY;
		int endIdx = offset + length;
		for (int i = offset; i < endIdx; i++) {
			if (arr[i] > maxValue) {
				maxValue = arr[i];
				maxIndex = i;
			}
		}
		return maxIndex;
	}

	public static double getMax(double[] arr, int offset, int length) {
		return arr[findMaxIndex(arr, offset, length)];
	}

	public static double getMax(double[] arr, int length) {
		return arr[findMaxIndex(arr, 0, length)];
	}

	public static double getMax(double[] arr) {
		return arr[findMaxIndex(arr, 0, arr.length)];
	}

	public static double getMin(double[] arr, int offset, int length) {
		return arr[findMinIndex(arr, offset, length)];
	}

	public static double getMin(double[] arr, int length) {
		return arr[findMinIndex(arr, 0, length)];
	}

	public static double getMin(double[] arr) {
		return arr[findMinIndex(arr, 0, arr.length)];
	}
	
	public static int[] toArray(List<Integer> list) {
		int[] result = new int[list.size()];
		for (int i = 0; i < result.length; i++) {
			result[i] = list.get(i);
		}
		return result;
	}
	
	public static String toTabDelimitedString(double[] arr) {
		StringBuilder sb = new StringBuilder();
		for (double value : arr) {
			sb.append("\t" + value);
		}
		return sb.toString().trim();
	}
	
	public static String toTabDelimitedString(int[] arr) {
		StringBuilder sb = new StringBuilder();
		for (int value : arr) {
			sb.append("\t" + value);
		}
		return sb.toString().trim();
	}

	public static <T> List<T> getRandomSubset(List<T> fullSet, int size, Random rnd) {
		List<T> subset = new ArrayList<T>();
		subset.addAll(fullSet);
		while (subset.size() > size) {
			subset.remove(rnd.nextInt(subset.size()));
		}
		return subset;
	}

	public static void insertionSort(int[] arr, ScoreBasedComparator comparator) {
		insertionSort(arr, arr.length, comparator);
	}

	public static void insertionSort(int[] arr, int length, ScoreBasedComparator comparator) {
		for (int i = 0; i < length; i++) {
			for (int j = i; j > 0 && comparator.compare(arr[j - 1], arr[j]) > 0; j--) {
				swap(arr, j, j - 1);
			}
		}
	}

	public static void sort(int[] arr, ScoreBasedComparator comparator) {
		sort(arr, arr.length, comparator);
	}

	public static void sort(int[] arr, int length, ScoreBasedComparator comparator) {
		int[] dest = new int[length];
		System.arraycopy(arr, 0, dest, 0, length);
		mergeSort(arr, dest, 0, length, 0, comparator);
		System.arraycopy(dest, 0, arr, 0, length);
	}

	public static int[] loadIntArrayFromLine(String line, int partsCount) throws Exception {
		String[] parts = line.split(" ");
		if (parts.length != partsCount) {
			throw new Exception("Invalid input.");
		}
		int[] result = new int[partsCount];
		for (int n = 0; n < partsCount; n++) {
			result[n] = Integer.parseInt(parts[n]);
		}
		return result;
	}

	public static double[] loadDoubleArrayFromLine(String line, int partsCount) throws Exception {
		String[] parts = line.split(" ");
		if (parts.length != partsCount) {
			throw new Exception("Invalid input.");
		}
		double[] result = new double[partsCount];
		for (int n = 0; n < partsCount; n++) {
			result[n] = Double.parseDouble(parts[n]);
		}
		return result;
	}
	
	public static double[][] loadDoubleMatrixFromLine(String line, int rows, int cols) throws Exception {
		String[] parts = line.split(" ");
		if (parts.length != rows * cols) {
			throw new Exception("Invalid input.");
		}
		double[][] result = new double[rows][cols];
		int idx = 0;
		for (int r = 0; r < rows; r++) {
			for (int c = 0; c < cols; c++) {
				result[r][c] = Double.parseDouble(parts[idx]);
				idx++;
			}
		}
		return result;
	}

	private static final int INSERTIONSORT_THRESHOLD = 7;

	private static void mergeSort(int[] src, int[] dest, int low, int high, int off, ScoreBasedComparator c) {
		int length = high - low;

		// Insertion sort on smallest arrays
		if (length < INSERTIONSORT_THRESHOLD) {
			for (int i = low; i < high; i++) {
				for (int j = i; j > low && c.compare(dest[j - 1], dest[j]) > 0; j--) {
					swap(dest, j, j - 1);
				}
			}
			return;
		}

		// Recursively sort halves of dest into src
		int destLow = low;
		int destHigh = high;
		low += off;
		high += off;
		int mid = (low + high) >>> 1;
		mergeSort(dest, src, low, mid, -off, c);
		mergeSort(dest, src, mid, high, -off, c);

		// If list is already sorted, just copy from src to dest. This is an
		// optimization that results in faster sorts for nearly ordered lists.
		if (c.compare(src[mid - 1], src[mid]) <= 0) {
			System.arraycopy(src, low, dest, destLow, length);
			return;
		}

		// Merge sorted halves (now in src) into dest
		for (int i = destLow, p = low, q = mid; i < destHigh; i++) {
			if (q >= high || p < mid && c.compare(src[p], src[q]) <= 0) {
				dest[i] = src[p++];
			} else {
				dest[i] = src[q++];
			}
		}
	}

	/*
	 * Swaps x[a] with x[b].
	 */
	public static void swap(int[] arr, int a, int b) {
		int t = arr[a];
		arr[a] = arr[b];
		arr[b] = t;
	}
	
	public static void swap(double[] arr, int a, int b) {
		double t = arr[a];
		arr[a] = arr[b];
		arr[b] = t;
	}

	public static void shuffle(int[] list, Random rnd) {
		shuffle(list, list.length, rnd);
	}

	public static void shuffle(int[] list, int len, Random rnd) {
		for (int i = len; i > 1; i--) {
			swap(list, i - 1, rnd.nextInt(i));
		}
	}
}
