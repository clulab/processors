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

package edu.uci.jforestsx.dataset;

import edu.uci.jforestsx.dataset.NumericArrayFactory.NumericArrayType;
import edu.uci.jforestsx.util.Util;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Feature implements ByteSerializable {

	public NumericArray bins;
	public int[] upperBounds;

	private String name;
	private double min;
	private double max;
	private double factor;
	private boolean onLogScale;

	public NumericArrayType getType() {
		return bins.getType();
	}

	public int getNumberOfValues() {
		return upperBounds.length;
	}
	
	public double getOriginalValue(int scaledValue) {
		double value = scaledValue / factor;
		if (onLogScale) {
			value = Math.exp(value);
			value = value + min - 1;
		} else {
			value += min;
		}
		return value;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		if (name != null) {
			if (name.length() > Byte.MAX_VALUE) {
				this.name = name.substring(0, Byte.MAX_VALUE);
			} else {
				this.name = name;
			}
		}
	}

	public double getMin() {
		return min;
	}

	public void setMin(double min) {
		this.min = min;
	}

	public double getMax() {
		return max;
	}

	public void setMax(double max) {
		this.max = max;
	}

	public double getFactor() {
		return factor;
	}

	public void setFactor(double factor) {
		this.factor = factor;
	}

	public boolean isOnLogScale() {
		return onLogScale;
	}

	public void setOnLogScale(boolean onLogScale) {
		this.onLogScale = onLogScale;
	}

	public Feature getSubSampleFeature(int[] indices) {
		Feature subSampleFeature = new Feature();
		subSampleFeature.bins = bins.getSubSampleNumericArray(indices);
		subSampleFeature.upperBounds = upperBounds;
		subSampleFeature.name = name;
		subSampleFeature.min = min;
		subSampleFeature.max = max;
		subSampleFeature.factor = factor;
		subSampleFeature.onLogScale = onLogScale;
		return subSampleFeature;
	}

	@Override
	public int getSizeInBytes() {
		int size = bins.getSizeInBytes();

		// 4 bytes for keeping the size of the upperBounds array and then 4
		// bytes for each array element
		size += 4 + upperBounds.length * 4;

		// 1 byte for name length and then the number of characters
		size += 2 + (name != null ? name.length() : 0);
		
		// min, max, factor, onLogScale
		size += 8 + 8 + 8 + 1;

		return size;
	}

	@Override
	public int toByteArray(byte[] arr, int offset) {
		offset = bins.toByteArray(arr, offset);
		offset = Util.putIntArrayInByteArray(upperBounds, arr, offset);
		offset = Util.putStringInByteArray(name, arr, offset);
		offset = Util.putDoubleInByteArray(min, arr, offset);
		offset = Util.putDoubleInByteArray(max, arr, offset);
		offset = Util.putDoubleInByteArray(factor, arr, offset);
		offset = Util.putBooleanInByteArray(onLogScale, arr, offset);
		return offset;
	}

	@Override
	public int loadFromByteArray(byte[] arr, int offset) {
		offset = bins.loadFromByteArray(arr, offset);
		
		upperBounds = Util.toIntArray(arr, offset);
		offset += 4 + upperBounds.length * 4;
		
		name = Util.toString(arr, offset);
		offset += 2 + name.length();
		
		min = Util.toDouble(arr, offset);
		offset += 8;
		
		max = Util.toDouble(arr, offset);
		offset += 8;
		
		factor = Util.toDouble(arr, offset);
		offset += 8;
		
		onLogScale = Util.toBoolean(arr, offset);
		offset += 1;
		
		return offset;
	}
}
