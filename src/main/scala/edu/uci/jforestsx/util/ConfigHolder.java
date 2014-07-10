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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import edu.uci.jforestsx.config.ComponentConfig;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class ConfigHolder {

	private Properties properties;
	private Map<String, ComponentConfig> configs;

	public ConfigHolder(Properties properties) {
		this.properties = properties;		
		configs = new HashMap<String, ComponentConfig>();
	}
	
	@SuppressWarnings("unchecked")
	public <T extends ComponentConfig> T getConfig(Class<T> _c) throws Exception {
		ComponentConfig config = configs.get(_c.getCanonicalName());
		if (config == null) {
			T newConfig = _c.newInstance();
			newConfig.init(this);
			configs.put(_c.getCanonicalName(), newConfig);
			return newConfig;
		}
		return (T) config;
	}
	
	public boolean hasProperty(String key) {
		return properties.containsKey(key);
	}

	public String getStringProperty(String key) {
		return properties.getProperty(key);
	}

	public int getIntProperty(String key) {
		return Integer.parseInt(properties.getProperty(key));
	}

	public double getDoubleProperty(String key) {
		return Double.parseDouble(properties.getProperty(key));
	}
	
	public boolean getBooleanProperty(String key) {
		return properties.getProperty(key).toLowerCase().trim().equals("true");
	}
	
	public Set<Entry<Object, Object>> getEntries() {
		return properties.entrySet();
	}
}
