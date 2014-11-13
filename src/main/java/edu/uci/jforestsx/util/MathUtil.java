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

import java.util.List;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class MathUtil {
	
	public static double log2(double value) {
		return Math.log(value) / Constants.LN2;
	}

	public static double getAvg(double[] arr) {
		double sum = 0;
		for (double item : arr) {
			sum += item;
		}
		return sum / arr.length;
	}

	public static double getAvg(float[] arr) {
		double sum = 0;
		for (double item : arr) {
			sum += item;
		}
		return sum / arr.length;
	}

	public static double getAvg(List<Double> arr) {
		double sum = 0;
		for (double item : arr) {
			sum += item;
		}
		return sum / arr.size();
	}

	public static double getStd(double[] arr) {
		return getStd(arr, getAvg(arr));
	}

	public static double getStd(List<Double> arr) {
		return getStd(arr, getAvg(arr));
	}

	public static double getStd(double[] arr, double avg) {
		return getStd(arr, avg, 0, arr.length);
	}
	
	public static double getStd(double[] arr, double avg, int offset, int count) {
		double sum = 0;
		int end = offset + count;
		for (int i = offset; i < end; i++) {
			sum += Math.pow(arr[i] - avg, 2);
		}
		return Math.sqrt(sum / count);
	}

	public static double getStd(List<Double> arr, double avg) {
		double sum = 0;
		for (double item : arr) {
			sum += Math.pow(item - avg, 2);
		}
		return Math.sqrt(sum / arr.size());
	}
	
	public static double getVectorSize(double[] arr) {
		double sum = 0;
		for (int i = 0; i < arr.length; i++) {
			sum += arr[i] * arr[i];
		}
		return Math.sqrt(sum);
	}

	public static double getDotProduct(float[] vector1, float[] vector2, int length) {
		double product = 0;
		for (int i = 0; i < length; i++) {
			product += vector1[i] * vector2[i];
		}
		return product;
	}
	
	public static double getDotProduct(double[] vector1, double[] vector2, int length) {
		double product = 0;
		for (int i = 0; i < length; i++) {
			product += vector1[i] * vector2[i];
		}
		return product;
	}
	
	public static double getDotProduct(float[] vector1, float[] vector2)
    {
        return getDotProduct(vector1, vector2, vector1.length);
    }
	
	/**
	 * Divides the second vector from the first one (vector1[i] /= val)
	 * @param vector	the input vector.
	 * @param val		the denominator
	 */
    public static void divideInPlace(float[] vector, float val)
    {
        int length = vector.length;
        for (int i = 0; i < length; i++)
        {
        	vector[i] /= val;
        }
    }
    
    public static double[][] allocateDoubleMatrix(int m, int n) {
		double[][] mat = new double[m][];
		for (int i = 0; i < m; i++) {
			mat[i] = new double[n];
		}
		return mat;
	}
    
    public static void clearDoubleMatrix(double[][] mat) {
    	int rows = mat.length;
    	for (int r = 0; r < rows; r++) {
    		int cols = mat[r].length;
    		for (int c = 0; c < cols; c++) {
    			mat[r][c] = 0;
    		}
    	}
    }
    
    public static double[][] cloneDoubleMatrix(double[][] src) {
    	int rows = src.length;
    	double[][] clone = new double[rows][];
    	for (int r = 0; r < rows; r++) {
    		int cols = src[r].length;
    		clone[r] = new double[cols];
    		for (int c = 0; c < cols; c++) {
    			clone[r][c] = src[r][c];
    		}
    	}
    	return clone;
    }
}
