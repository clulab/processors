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

package edu.uci.jforestsx.tuning;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class TuningConfig {
	
	private int id;	
	private String configKey;
	private String configText;
	private int fold;
	private int randomSeed;
	
	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getConfigKey() {
		return configKey;
	}

	public void setConfigKey(String configKey) {
		configKey = configKey.replaceAll("(\\r\\n|\\n)+", "\n");
		configKey = configKey.replaceAll("(\\n)+", ";");
		configKey = configKey.replaceAll("\\s+", "");
		this.configKey = configKey;
	}

	public String getConfigText() {
		return configText;
	}

	public void setConfigText(String configText) {
		this.configText = configText;
	}
	
	public int getFold() {
		return fold;
	}

	public void setFold(int fold) {
		this.fold = fold;
	}
	
	public int getRandomSeed() {
		return randomSeed;
	}

	public void setRandomSeed(int randomSeed) {
		this.randomSeed = randomSeed;
	}

}
