package org.maltparser.core.options.option;

import java.util.Formatter;
import java.util.HashMap;
import java.util.TreeSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.options.OptionException;
import org.maltparser.core.options.OptionGroup;
import org.maltparser.core.plugin.PluginLoader;

/**
 * A class type option is an option that can only contain string value that corresponds to a class. 
 *
 * @author Johan Hall
 * @since 1.0
**/
public class ClassOption extends Option{
	private Class<?> defaultValue;
	private TreeSet<String> legalValues;
	private HashMap<String,String> legalValueDesc;
	private HashMap<String,Class<?>> legalValueClass;
	private HashMap<Class<?>, String> classLegalValues;
	
	/**
	 * Creates a class type option description
	 * 
	 * @param group	a reference to the option group.
	 * @param name	the name of the option.
	 * @param shortDescription	a short description of the option.
	 * @param flag	a short string that can be used in the command line.
	 * @param usage	a string that explains the usage of the option.
	 * @throws OptionException
	 */
	public ClassOption(OptionGroup group, 
						String name, 
						String shortDescription, 
						String flag, 
						String usage) throws MaltChainedException {
		super(group, name, shortDescription, flag, usage);
		legalValues = new TreeSet<String>();
		legalValueDesc = new HashMap<String,String>();
		legalValueClass = new HashMap<String,Class<?>>();
		classLegalValues = new HashMap<Class<?>, String>();
	}

	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getValueObject(java.lang.String)
	 */
	public Object getValueObject(String value) throws MaltChainedException {
		if (value == null) {
			return null;
		} else if (legalValues.contains(value)) {
			return legalValueClass.get(value);
		} else {
			throw new OptionException("'"+value+"' is not a legal value for the '"+getName()+"' option. ");
		}	
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueObject()
	 */
	public Object getDefaultValueObject() throws OptionException {
		return defaultValue;
	}

	/**
	 * Returns a string representation of particular class
	 * 
	 * @param clazz	an class object
	 * @return	a string representation of particular class, if not present null is returned.
	 * @throws MaltChainedException
	 */
	public String getLegalValueString(Class<?> clazz) throws MaltChainedException {
		return classLegalValues.get(clazz);
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#setDefaultValue(java.lang.String)
	 */
	public void setDefaultValue(String defaultValue) throws MaltChainedException {
		if (defaultValue == null) {
			if (legalValues.isEmpty()) {
				throw new OptionException("The default value is null and the legal value set is empty for the '"+getName()+"' option. ");
			} else {
				this.defaultValue = legalValueClass.get(((TreeSet<String>)legalValueClass.keySet()).first()); 
			}
		} else if (legalValues.contains(defaultValue.toLowerCase())) {
			this.defaultValue = legalValueClass.get(defaultValue.toLowerCase());
		} else {
			throw new OptionException("The default value '"+defaultValue+"' is not a legal value for the '"+getName()+"' option. ");
		}
	}
	
	/**
	 * Returns the class that corresponds to the enumerate string value.
	 * 
	 * @param value	an enumerate string value
	 * @return the class that corresponds to the enumerate string value.
	 */
	public Class<?> getClazz(String value) {
		return legalValueClass.get(value);
	}

	/**
	 * Adds a legal value that corresponds to a class
	 * 
	 * @param value	a legal value name
	 * @param desc	a short description of the legal value
	 * @param classname	the fully qualified name of the class
	 * @throws OptionException
	 */
	public void addLegalValue(String value, String desc, String classname) throws MaltChainedException {
		if (value == null || value.equals("")) {
			throw new OptionException("The legal value is missing for the '"+getName()+"' option. ");
		} else if (legalValues.contains(value.toLowerCase())) {
			throw new OptionException("The legal value for the '"+getName()+"' option already exists. ");
		} else {
			legalValues.add(value.toLowerCase());
			if (desc == null || desc.equals("")) {
				legalValueDesc.put(value.toLowerCase(), "Description is missing. ");
			} else {
				legalValueDesc.put(value.toLowerCase(), desc);
			}
			if (classname == null || classname.equals("")) {
				throw new OptionException("The class name used by the '"+getName()+"' option is missing. ");
			} else {
				try {
					Class<?> clazz = null;
					if (PluginLoader.instance() != null) {
						clazz = PluginLoader.instance().getClass(classname);
					}
					if (clazz == null) {
						clazz = Class.forName(classname);
					}
					legalValueClass.put(value, clazz);
					classLegalValues.put(clazz, value);
				} catch (ClassNotFoundException e) {
					throw new OptionException("The class "+classname+" for the '"+getName()+"' option could not be found. ", e);
				}
			}
		}
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueString()
	 */
	public String getDefaultValueString() {
		return classLegalValues.get(defaultValue);
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getStringRepresentation(java.lang.Object)
	 */
	public String getStringRepresentation(Object value) {
		if (value instanceof Class && classLegalValues.containsKey(value)) {
			return classLegalValues.get(value);
		}
		return null;
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#toString()
	 */
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		Formatter formatter = new Formatter(sb);
		for (String value : legalValues) {
			formatter.format("%2s%-10s - %-20s\n", "", value, legalValueDesc.get(value));
		}
		sb.append("-----------------------------------------------------------------------------\n");
		return sb.toString();
	}
}