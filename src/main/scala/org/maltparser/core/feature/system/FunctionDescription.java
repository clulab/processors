package org.maltparser.core.feature.system;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.maltparser.core.config.ConfigurationRegistry;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.AbstractFeatureFactory;
import org.maltparser.core.feature.FeatureException;
import org.maltparser.core.feature.function.Function;
/**
 *  
 *
 * @author Johan Hall
 * @since 1.0
**/
public class FunctionDescription {
	private String name;
	private Class<?> functionClass;
	private boolean hasSubfunctions;
	private boolean hasFactory;
	
//	public FunctionDescription(String name, Class<?> functionClass, boolean hasSubfunctions) {
//		setName(name);
//		setFunctionClass(functionClass);
//		setHasSubfunctions(hasSubfunctions);
//	}

	public FunctionDescription(String name, Class<?> functionClass, boolean hasSubfunctions, boolean hasFactory) {
		setName(name);
		setFunctionClass(functionClass);
		setHasSubfunctions(hasSubfunctions);
		setHasFactory(hasFactory);
	}
	
	public Function newFunction(ConfigurationRegistry registry) throws MaltChainedException {
		if (hasFactory) {
			for (Class<?> c : registry.keySet()) {
				try {
					c.asSubclass(functionClass);
				} catch (ClassCastException e) {
					continue;
				}
				return ((AbstractFeatureFactory)registry.get(c)).makeFunction(name);
			}
			return null;
		}
		Constructor<?>[] constructors = functionClass.getConstructors();
		if (constructors.length == 0) {
			try {
				return (Function)functionClass.newInstance();
			} catch (InstantiationException e) {
				throw new FeatureException("The function '"+functionClass.getName()+"' cannot be initialized. ", e);
			} catch (IllegalAccessException e) {
				throw new FeatureException("The function '"+functionClass.getName()+"' cannot be initialized. ", e);
			}
		}
		Class<?>[] params = constructors[0].getParameterTypes();
		if (params.length == 0) {
			try {
				return (Function)functionClass.newInstance();
			} catch (InstantiationException e) {
				throw new FeatureException("The function '"+functionClass.getName()+"' cannot be initialized. ", e);
			} catch (IllegalAccessException e) {
				throw new FeatureException("The function '"+functionClass.getName()+"' cannot be initialized. ", e);
			}
		}
		Object[] arguments = new Object[params.length];
		for (int i = 0; i < params.length; i++) {
			if (hasSubfunctions && params[i] == java.lang.String.class) {
				arguments[i] = name;
			} else {
				arguments[i] = registry.get(params[i]);
				if (arguments[i] == null) {
					return null;
				}
			}
		}
		try {
			return (Function)constructors[0].newInstance(arguments);
		} catch (InstantiationException e) {
			throw new FeatureException("The function '"+functionClass.getName()+"' cannot be initialized. ", e);
		} catch (IllegalAccessException e) {
			throw new FeatureException("The function '"+functionClass.getName()+"' cannot be initialized. ", e);
		} catch (InvocationTargetException e) {
			throw new FeatureException("The function '"+functionClass.getName()+"' cannot be initialized. ", e);
		}
	}
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Class<?> getFunctionClass() {
		return functionClass;
	}

	public void setFunctionClass(Class<?> functionClass) {
		this.functionClass = functionClass;
	}

	public boolean isHasSubfunctions() {
		return hasSubfunctions;
	}

	public void setHasSubfunctions(boolean hasSubfunctions) {
		this.hasSubfunctions = hasSubfunctions;
	}

	public boolean isHasFactory() {
		return hasFactory;
	}

	public void setHasFactory(boolean hasFactory) {
		this.hasFactory = hasFactory;
	}

	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		if (!(name.equalsIgnoreCase(((FunctionDescription)obj).getName()))) {
			return false;
		} else if (!(functionClass.equals(((FunctionDescription)obj).getFunctionClass()))) {
			return false;
		}
		return true;
	}

	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(name);
		sb.append("->");
		sb.append(functionClass.getName());
		return sb.toString();
	}
}
