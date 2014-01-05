package org.maltparser.core.options.option;

import java.util.Formatter;
import java.util.HashMap;
import java.util.TreeSet;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.options.OptionException;
import org.maltparser.core.options.OptionGroup;

/**
 * An enumerate option is an option that can only contain string value, which is in the legal value set.
 *
 * @author Johan Hall
 * @since 1.0
**/
public class EnumOption extends Option {
	private String defaultValue;
	private TreeSet<String> legalValues;
	private HashMap<String,String> legalValueDesc;
	
	/**
	 * Creates an enumerate option description
	 * 
	 * @param group	a reference to the option group.
	 * @param name	the name of the option.
	 * @param shortDescription	a short description of the option.
	 * @param flag	a short string that can be used in the command line.
	 * @param usage	a string that explains the usage of the option.
	 * @throws OptionException
	 */
	public EnumOption(OptionGroup group, 
						String name, 
						String shortDescription, 
						String flag, 
						String usage) throws MaltChainedException {
		super(group, name, shortDescription, flag, usage);
		legalValues = new TreeSet<String>();
		legalValueDesc = new HashMap<String,String>();
	}

	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getValueObject(java.lang.String)
	 */
	public Object getValueObject(String value) throws MaltChainedException {
		if (value == null) {
			return null;
		} else if (legalValues.contains(value)) {
			return new String(value);
		} else {
			throw new OptionException("'"+value+"' is not a legal value for the '"+getName()+"' option. ");
		}	
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueObject()
	 */
	public Object getDefaultValueObject() throws MaltChainedException {
		return new String(defaultValue);
	}

	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getDefaultValueString()
	 */
	public String getDefaultValueString() {
		return defaultValue.toString();
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#setDefaultValue(java.lang.String)
	 */
	public void setDefaultValue(String defaultValue) throws MaltChainedException {
		if (defaultValue == null) {
			if (legalValues.isEmpty()) {
				throw new OptionException("The default value of the '"+getName()+"' option is null and the legal value set is empty.");
			} else {
				this.defaultValue = legalValues.first();
			}
		} else if (legalValues.contains(defaultValue.toLowerCase())) {
			this.defaultValue = defaultValue.toLowerCase();
		} else {
			throw new OptionException("The default value '"+defaultValue+"' for the '"+getName()+"' option is not a legal value. ");
		}
	}
	
	/**
	 * Adds a legal value
	 * 
	 * @param value	a legal value name
	 * @param desc	a short description of the legal value
	 * @throws OptionException
	 */
	public void addLegalValue(String value, String desc) throws MaltChainedException {
		if (value == null || value.equals("")) {
			throw new OptionException("The legal value is missing for the '"+getName()+"' option. ");
		} else if (legalValues.contains(value.toLowerCase())) {
			throw new OptionException("The legal value '"+value+"' already exists for the '"+getName()+"' option. ");
		} else {
			legalValues.add(value.toLowerCase());
			if (desc == null || desc.equals("")) {
				legalValueDesc.put(value.toLowerCase(), "Description is missing. ");
			} else {
				legalValueDesc.put(value.toLowerCase(), desc);
			}
		}
	}
	
	/**
	 * Adds a legal value without a description
	 * 
	 * @param value	a legal value name
	 * @throws OptionException
	 */
	public void addLegalValue(String value) throws MaltChainedException {
		addLegalValue(value, null);
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.core.options.option.Option#getStringRepresentation(java.lang.Object)
	 */
	public String getStringRepresentation(Object value) {
		if (value instanceof String && legalValues.contains(value)) {
			return value.toString();
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
