package org.maltparser.core.config;

import java.util.HashMap;
/**
*
* @author Johan Hall
**/
public class ConfigurationRegistry extends HashMap<Class<?>, Object> {
	public final static long serialVersionUID = 3256444702936019250L;
	
	public ConfigurationRegistry() {
		super();
	}

	@Override
	public Object get(Object key) {
		return super.get(key);
	}

	@Override
	public Object put(Class<?> key, Object value) {
		return super.put(key, value);
	}
}
