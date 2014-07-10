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

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Constants {
	public final static double EPSILON = 1.4e-45;
	public final static double MIN_EXP_POWER = -50;
	public final static double LN2 = Math.log(2);

	public static int[] ONE_TWO_THREE_ETC;
	public static double[] DOUBLE_ONE_ONE_ONE_ETC;
	
	public static synchronized void init(int maxSize) {
		if (ONE_TWO_THREE_ETC == null || ONE_TWO_THREE_ETC.length < maxSize) {
			ONE_TWO_THREE_ETC = new int[maxSize];
			DOUBLE_ONE_ONE_ONE_ETC = new double[maxSize];
			for (int i = 0; i < maxSize; i++) {
				ONE_TWO_THREE_ETC[i] = i;
				DOUBLE_ONE_ONE_ONE_ETC[i] = 1.0;
			}
		}
	}
}
