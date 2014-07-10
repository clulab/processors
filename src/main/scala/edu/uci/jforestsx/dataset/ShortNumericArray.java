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

public class ShortNumericArray extends NumericArray {
	
	private short[] data;
	
	public ShortNumericArray(int length) {
		super(length);
		data = new short[length];
	}

	@Override
	public int getSizeInBytes() {
		return 2 * length;
	}

	@Override
	public int get(int index) {
		return data[index];
	}

	@Override
	public void set(int index, int value) {
		data[index] = (short)value;
	}

	@Override
	public int getBitsPerItem() {
		return Short.SIZE;
	}
	
	@Override
	public int toByteArray(byte[] arr, int offset) {
		for (int i = 0; i<length; i++) {
			Util.putShortInByteArray(data[i], arr, offset);
			offset += 2;
		}
		return offset;
	}

	@Override
	public int loadFromByteArray(byte[] arr, int offset) {
		for (int i = 0; i<length; i++) {
			data[i] = Util.toShort(arr, offset);
			offset += 2;
		}
		return offset;
	}

	@Override
	public NumericArrayType getType() {		
		return NumericArrayType.SHORT;
	}

	@Override
	public NumericArray getSubSampleNumericArray(int[] indices) {
		ShortNumericArray subsampleArray = new ShortNumericArray(indices.length);
		for (int i = 0 ; i < indices.length; i++) {
			subsampleArray.data[i] = data[indices[i]];
		}
		return subsampleArray;
	}
}