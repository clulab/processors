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

package edu.uci.jforestsx.input.sparse;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class MetaLineParser {
	
	private static Map<String, String> getMap(String line) {
		String[] parts = line.split("\t");
		Map<String, String> result = new HashMap<String, String>();
		for (int i = 1; i < parts.length; i++) {
			String[] subParts = parts[i].split(":");
			result.put(subParts[0], subParts[1]);
		}
		return result;
	}
	
	public static MetaData parse(String line) {
		if (line.startsWith("@Feature")) {
			return new FeatureMetaData(getMap(line));			
		}
		return null;
	}
}
