package org.maltparser.core.options.option;

import java.util.Formatter;
import java.util.HashMap;
import java.util.TreeSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.options.OptionException;
import org.maltparser.core.options.OptionGroup;

/**
 * A string enum type option is an option that can only contain string value that corresponds to another string.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class StringEnumOption extends Option{
	private String defaultValue;
	private TreeSet<String> legalValues;
	private HashMap<String,String> legalValueDesc;
	private HashMap<String,String> valueMapto;
	private HashMap<String, String> maptoValue;
	
	/**
	 * Creates a stringenum type option description
	 * 
	 * @param group	a reference to the option group.
	 * @param name	the name of the option.
	 * @param shortDescription	a short description of the option.
	 * @param flag	a short string that can be used in the command line.
	 * @param usage	a string that explains the usage of the option.
	 * @throws OptionException
	 */
	public StringEnumOption(OptionGroup group, 
						String name, 
						String shortDescription, 
						String flag, 
						String usage) throws MaltChainedException {
		super(group, name, shortDescription, flag, usage);
		legalValues = new TreeSet<String>();
		legalValueDesc = new HashMap<String,String>();
		valueMapto = new HashMap<String,String>();
		maptoValue = new HashMap<String, String>();
	}

	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getValueObject(java.lang.String)
	 */
	public Object getValueObject(String value) throws MaltChainedException {
		if (value == null) {
			return null;
		} else if (legalValues.contains(value)) {
			return new String(valueMapto.get(value));
		} else {
			return new String(value);
		}	
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueObject()
	 */
	public Object getDefaultValueObject() throws MaltChainedException {
			return new String(defaultValue);
	}

	/**
	 * Returns the legal value identifier name (an enumerate string value)
	 * 
	 * @param value the mapped legal value
	 * @return the legal value identifier name, null if it could not be found
	 * @throws MaltChainedException
	 */
	public String getLegalValueString(String value) throws MaltChainedException {
		return new String(maptoValue.get(value));
	}
	
	/**
	 * Returns the mapped legal value
	 * 
	 * @param value an enumerate string value
	 * @return the mapped legal value, null if it could not be found
	 * @throws MaltChainedException
	 */
	public String getLegalValueMapToString(String value) throws MaltChainedException {
		return new String(valueMapto.get(value));
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#setDefaultValue(java.lang.String)
	 */
	public void setDefaultValue(String defaultValue) throws MaltChainedException {
		if (defaultValue == null) {
			if (legalValues.isEmpty()) {
				throw new OptionException("The default value is null and the legal value set is empty for the '"+getName()+"' option. ");
			} else {
				this.defaultValue = valueMapto.get(((TreeSet<String>)valueMapto.keySet()).first()); 
			}
		} else if (legalValues.contains(defaultValue.toLowerCase())) {
			this.defaultValue = valueMapto.get(defaultValue.toLowerCase());
		} else if (defaultValue.equals("")) {
			this.defaultValue = defaultValue;
		} else {
			throw new OptionException("The default value '"+defaultValue+"' for the '"+getName()+"' option is not a legal value. ");
		}
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueString()
	 */
	public String getDefaultValueString() {
		return defaultValue.toString();
	}
	
	/**
	 * Returns the mapped legal value that corresponds to the enumerate string value.
	 * 
	 * @param value	an enumerate string value
	 * @return the mapped legal value that corresponds to the enumerate string value.
	 */
	public String getMapto(String value) {
		return new String(valueMapto.get(value));
	}

	/**
	 * Adds a legal value that corresponds to another string
	 * 
	 * @param value	a legal value name
	 * @param desc	a short description of the legal value
	 * @param mapto	a mapto string value
	 * @throws OptionException
	 */
	public void addLegalValue(String value, String desc, String mapto) throws MaltChainedException {
		if (value == null || value.equals("")) {
			throw new OptionException("The legal value is missing for the option "+getName()+".");
		} else if (legalValues.contains(value.toLowerCase())) {
			throw new OptionException("The legal value "+value+" already exists for the option "+getName()+". ");
		} else {
			legalValues.add(value.toLowerCase());
			if (desc == null || desc.equals("")) {
				legalValueDesc.put(value.toLowerCase(), "Description is missing. ");
			} else {
				legalValueDesc.put(value.toLowerCase(), desc);
			}
			if (mapto == null || mapto.equals("")) {
				throw new OptionException("A mapto value is missing for the option "+getName()+". ");
			} else {
				valueMapto.put(value, mapto);
				maptoValue.put(mapto, value);
			}
		}
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getStringRepresentation(java.lang.Object)
	 */
	public String getStringRepresentation(Object value) {
		if (value instanceof String) {
			if (legalValues.contains(value)) {
				return valueMapto.get(value);
			} else {
				return value.toString();
			}
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
		return sb.toString();
	}
}