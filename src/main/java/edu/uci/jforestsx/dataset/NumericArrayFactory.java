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

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class NumericArrayFactory {
	public enum NumericArrayType {
		NULL, BIT, BYTE, SHORT, INT;

		public static NumericArrayType getFromOrdinal(int ordinal) {
			for (NumericArrayType type : values()) {
				if (type.ordinal() == ordinal) {
					return type;
				}
			}
			return null;
		}
	}

	public static NumericArray createNumericArray(NumericArrayType type, int length) throws Exception {
		switch (type) {
		case NULL:
			return NullNumericArray.getInstance();
		case BIT:
			return new BitNumericArray(length);
		case BYTE:
			return new ByteNumericArray(length);
		case SHORT:
			return new ShortNumericArray(length);
		default:
			throw new Exception(type + " is not implemented yet.");
		}
	}
}
