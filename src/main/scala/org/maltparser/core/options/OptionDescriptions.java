package org.maltparser.core.options;

import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.HashSet;
import org.maltparser.core.helper.SystemLogger;
import org.maltparser.core.options.option.BoolOption;
import org.maltparser.core.options.option.ClassOption;
import org.maltparser.core.options.option.EnumOption;
import org.maltparser.core.options.option.IntegerOption;
import org.maltparser.core.options.option.Option;
import org.maltparser.core.options.option.StringEnumOption;
import org.maltparser.core.options.option.StringOption;
import org.maltparser.core.options.option.UnaryOption;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Organizes all the option descriptions. Option descriptions can be loaded from the application data <code>/appdata/options.xml</code>, but also 
 * from a plugin option description file (always with the name <code>plugin.xml</code>).
 * 
* @author Johan Hall
* @since 1.0
**/
public class OptionDescriptions {
//	private static Logger logger = SystemLogger.logger();
	private HashMap<String, OptionGroup> optionGroups;
	private TreeSet<String> ambiguous;
	private HashMap<String, Option> unambiguousOptionMap;
	private HashMap<String, Option> ambiguousOptionMap;
	private HashMap<String, Option> flagOptionMap;

	/**
	 * Creates the Option Descriptions
	 */
	public OptionDescriptions() {
		optionGroups = new HashMap<String, OptionGroup>();
		ambiguous = new TreeSet<String>();
		unambiguousOptionMap = new HashMap<String, Option>();
		ambiguousOptionMap = new HashMap<String, Option>();
		flagOptionMap = new HashMap<String, Option>();
	}
	
	
	/**
	 * Returns an option based on the option name and/or the option group name 
	 * 
	 * @param optiongroup	the name of the option group
	 * @param optionname	the option name
	 * @return an option based on the option name and/or the option group name 
	 * @throws MaltChainedException
	 */
	public Option getOption(String optiongroup, String optionname) throws MaltChainedException {		
		if (optionname == null || optionname.length() <= 0) {
			throw new OptionException("The option name '"+optionname+"' cannot be found" ); 
		}
		Option option;
		if (ambiguous.contains(optionname.toLowerCase())) {
			if (optiongroup == null || optiongroup.length() <= 0) {
				throw new OptionException("The option name '"+optionname+"' is ambiguous use option group name to distinguish the option. ");
			}
			else {
				option = ambiguousOptionMap.get(optiongroup.toLowerCase()+"-"+optionname.toLowerCase());
				if (option == null) {
					throw new OptionException("The option '--"+optiongroup.toLowerCase()+"-"+optionname.toLowerCase()+" does not exist. ");
				}
			}
		} else {
			option = unambiguousOptionMap.get(optionname.toLowerCase());
			if (option == null) {
				throw new OptionException("The option '--"+optionname.toLowerCase()+" doesn't exist. ");
			}
		}
		return option;
	}
	
	/**
	 * Returns an option based on the option flag
	 * 
	 * @param optionflag the option flag
	 * @return an option based on the option flag
	 * @throws MaltChainedException
	 */
	public Option getOption(String optionflag) throws MaltChainedException {
		Option option = flagOptionMap.get(optionflag);
		if (option == null) {
			throw new OptionException("The option flag -"+optionflag+" could not be found. ");
		}
		return option;
	}
	
	/**
	 * Returns a set of option that are marked as SAVEOPTION
	 * 
	 * @return a set of option that are marked as SAVEOPTION
	 */
	public Set<Option> getSaveOptionSet() {
		Set<Option> optionToSave = new HashSet<Option>();
		
		for (String optionname : unambiguousOptionMap.keySet()) {
			if (unambiguousOptionMap.get(optionname).getUsage() == Option.SAVE) {
				optionToSave.add(unambiguousOptionMap.get(optionname));
			}
		}
		for (String optionname : ambiguousOptionMap.keySet()) {
			if (ambiguousOptionMap.get(optionname).getUsage() == Option.SAVE) {
				optionToSave.add(ambiguousOptionMap.get(optionname));
			}
		}
		return optionToSave;
	}
	
	/**
	 * Return a sorted set of option group names
	 * 
	 * @return a sorted set of option group names
	 */
	public TreeSet<String> getOptionGroupNameSet() {
		return new TreeSet<String>(optionGroups.keySet());
	}
	
	/**
	 * Returns a collection of option that are member of an option group 
	 * 
	 * @param groupname the name of the option group
	 * @return a collection of option that are member of an option group 
	 */
	public Collection<Option> getOptionGroupList(String groupname) {
		return optionGroups.get(groupname).getOptionList();
	}
	
	/**
	 * Parse a XML file that contains the options used for controlling the application. The method
	 * calls the parseOptionGroups to parse the set of option groups in the DOM tree. 
	 * 
	 * @param url	The path to a XML file that explains the options used in the application.  
	 * @throws OptionException
	 */
	public void parseOptionDescriptionXMLfile(URL url) throws MaltChainedException {
		if (url == null) {
			throw new OptionException("The URL to the default option file is null. ");
		}

        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();

    		Element root = db.parse(url.openStream()).getDocumentElement();
    		NodeList groups = root.getElementsByTagName("optiongroup");
            Element group;
            for (int i = 0; i < groups.getLength(); i++) {
            	group = (Element)groups.item(i);
            	String groupname = group.getAttribute("groupname").toLowerCase();
            	OptionGroup og = null;
            	if (optionGroups.containsKey(groupname)) {
            		og = optionGroups.get(groupname);
            	} else {
            		optionGroups.put(groupname, new OptionGroup(groupname));
            		og = optionGroups.get(groupname);
            	}
            	parseOptionsDescription(group, og);
            }
        } catch (java.io.IOException e) {
        	throw new OptionException("Can't find the file "+url.toString()+".", e);
        } catch (OptionException e) {
        	throw new OptionException("Problem parsing the file "+url.toString()+". ", e);
        } catch (ParserConfigurationException e) {
        	throw new OptionException("Problem parsing the file "+url.toString()+". ", e);
        } catch (SAXException e) {
        	throw new OptionException("Problem parsing the file "+url.toString()+". ", e);
        }
	}
	
	
	/**
	 * Parse a set of options within an option group to collect all information of individual options. 
	 * 
	 * @param group a reference to an individual option group in the DOM tree.
	 * @param og a reference to the corresponding option group in the HashMap.
	 * @throws OptionException
	 */
	private void parseOptionsDescription(Element group, OptionGroup og) throws MaltChainedException {
		NodeList options = group.getElementsByTagName("option");
        Element option;
        for (int i = 0; i < options.getLength(); i++) {
        	option = (Element)options.item(i);
        	String optionname = option.getAttribute("name").toLowerCase();
        	String optiontype = option.getAttribute("type").toLowerCase();
        	String defaultValue = option.getAttribute("default");
        	String usage = option.getAttribute("usage").toLowerCase();
        	String flag = option.getAttribute("flag");

    		NodeList shortdescs = option.getElementsByTagName("shortdesc");
    		Element shortdesc;
    		String shortdesctext = "";
    		if (shortdescs.getLength() == 1) {
    			shortdesc = (Element)shortdescs.item(0);
    			shortdesctext = shortdesc.getTextContent();
    		}
    		
    		if (optiontype.equals("string") || optiontype.equals("bool") || optiontype.equals("integer") || optiontype.equals("unary")) {
    			Option op = og.getOption(optionname);
    			if (op != null) {
    				throw new OptionException("The option name '"+optionname+"' for option group '"+og.getName()+"' already exists. It is only allowed to override the class and enum option type to add legal value. ");
    			}
    		} else if (optiontype.equals("class") || optiontype.equals("enum") || optiontype.equals("stringenum")) {
    			Option op = og.getOption(optionname);
    			if (op != null) {
    				if (op instanceof EnumOption && !optiontype.equals("enum")) {
    					throw new OptionException("The option name '"+optionname+"' for option group '"+og.getName()+"' already exists. The existing option is of enum type, but the new option is of '"+optiontype+"' type. ");
    				}
    				if (op instanceof ClassOption && !optiontype.equals("class")) {
    					throw new OptionException("The option name '"+optionname+"' for option group '"+og.getName()+"' already exists. The existing option is of class type, but the new option is of '"+optiontype+"' type. ");
    				}
    				if (op instanceof StringEnumOption && !optiontype.equals("stringenum")) {
    					throw new OptionException("The option name '"+optionname+"' for option group '"+og.getName()+"' already exists. The existing option is of urlenum type, but the new option is of '"+optiontype+"' type. ");
    				}
    			}
    		}
        	if (optiontype.equals("string")) {
        		og.addOption(new StringOption(og, optionname, shortdesctext, flag, usage, defaultValue));
        	} else if (optiontype.equals("bool")) {
        		og.addOption(new BoolOption(og, optionname, shortdesctext, flag, usage, defaultValue));
        	} else if (optiontype.equals("integer")) {
        		og.addOption(new IntegerOption(og, optionname, shortdesctext, flag, usage, defaultValue));
        	} else if (optiontype.equals("unary")) {
        		og.addOption(new UnaryOption(og, optionname, shortdesctext, flag, usage));
        	} else if (optiontype.equals("enum")) {
        		Option op = og.getOption(optionname);
        		EnumOption eop = null;
    			if (op == null) {
    				eop = new EnumOption(og, optionname, shortdesctext, flag, usage);
    			} else {
    				if (op instanceof EnumOption) {
    					eop = (EnumOption)op;
    				}
    			}
    			
        		NodeList legalvalues = option.getElementsByTagName("legalvalue");
        		Element legalvalue;
        		for (int j = 0; j < legalvalues.getLength(); j++) {
        			legalvalue = (Element)legalvalues.item(j);
        			String legalvaluename = legalvalue.getAttribute("name");
        			String legalvaluetext = legalvalue.getTextContent();
        			eop.addLegalValue(legalvaluename, legalvaluetext);
        		}
        		if (op == null) {
        			eop.setDefaultValue(defaultValue);
        			og.addOption(eop);
        		}
        		
        	} else if (optiontype.equals("class") ) {
        		Option op = og.getOption(optionname);
        		ClassOption cop = null;
    			if (op == null) {
    				cop = new ClassOption(og, optionname, shortdesctext, flag, usage);
    			} else {
    				if (op instanceof ClassOption) {
    					cop = (ClassOption)op;
    				}
    			}
    			
        		NodeList legalvalues = option.getElementsByTagName("legalvalue");
        		Element legalvalue;
        		for (int j = 0; j < legalvalues.getLength(); j++) {
        			legalvalue = (Element)legalvalues.item(j);
        			String legalvaluename = legalvalue.getAttribute("name").toLowerCase();
        			String classname = legalvalue.getAttribute("class");
        			String legalvaluetext = legalvalue.getTextContent();
        			cop.addLegalValue(legalvaluename, legalvaluetext, classname);
        		} 
        		if (op == null) {
        			cop.setDefaultValue(defaultValue);
        			og.addOption(cop);
        		}
        	} else if (optiontype.equals("stringenum") ) {
        		Option op = og.getOption(optionname);
        		StringEnumOption ueop = null;
    			if (op == null) {
    				ueop = new StringEnumOption(og, optionname, shortdesctext, flag, usage);
    			} else {
    				if (op instanceof StringEnumOption) {
    					ueop = (StringEnumOption)op;
    				}
    			}
    			
        		NodeList legalvalues = option.getElementsByTagName("legalvalue");
        		Element legalvalue;
        		for (int j = 0; j < legalvalues.getLength(); j++) {
        			legalvalue = (Element)legalvalues.item(j);
        			String legalvaluename = legalvalue.getAttribute("name").toLowerCase();
        			String url = legalvalue.getAttribute("mapto");
        			String legalvaluetext = legalvalue.getTextContent();
        			ueop.addLegalValue(legalvaluename, legalvaluetext, url);
        		} 
        		if (op == null) {
        			ueop.setDefaultValue(defaultValue);
        			og.addOption(ueop);
        		}	
        	} else {
        		throw new OptionException("Illegal option type found in the setting file. ");
        	}
        }
	}
	
	/**
	 * Creates several option maps for fast access to individual options.  
	 * 
	 * @throws OptionException
	 */
	public void generateMaps() throws MaltChainedException {
        for (String groupname : optionGroups.keySet()) {
        	OptionGroup og = optionGroups.get(groupname);
        	Collection<Option> options = og.getOptionList();
        	
        	for (Option option : options) {
        		if (ambiguous.contains(option.getName())) {
        			option.setAmbiguous(true);
        			ambiguousOptionMap.put(option.getGroup().getName()+"-"+option.getName(), option);
        		} else {
	        		if (!unambiguousOptionMap.containsKey(option.getName())) {
	        			unambiguousOptionMap.put(option.getName(), option);
	        		} else {
	        			Option ambig = unambiguousOptionMap.get(option.getName());
	        			unambiguousOptionMap.remove(ambig);
	        			ambig.setAmbiguous(true);
	        			option.setAmbiguous(true);
	        			ambiguous.add(option.getName());
	        			ambiguousOptionMap.put(ambig.getGroup().getName()+"-"+ambig.getName(), ambig);
	        			ambiguousOptionMap.put(option.getGroup().getName()+"-"+option.getName(), option);
	        		}
        		}
    			if (option.getFlag() != null) {
    				Option co = flagOptionMap.get(option.getFlag());
    				if (co != null) {
    					flagOptionMap.remove(co);
    					co.setFlag(null);
    					option.setFlag(null);
    					if (SystemLogger.logger().isDebugEnabled()) {
    						SystemLogger.logger().debug("Ambiguous use of an option flag -> the option flag is removed for all ambiguous options\n");
    					}
    				} else {
    					flagOptionMap.put(option.getFlag(), option);
    				}
    			}
        	}
        }
	}
	
	/**
	 * Returns a string representation that contains printable information of several options maps
	 * 
	 * @return a string representation that contains printable information of several options maps
	 */
	public String toStringMaps() {
		final StringBuilder sb = new StringBuilder();
		sb.append("UnambiguousOptionMap\n");
        for (String optionname : new TreeSet<String>(unambiguousOptionMap.keySet())) {
        	sb.append("   "+optionname+"\n");
        }	
        sb.append("AmbiguousSet\n");
        for (String optionname : ambiguous) {
        	sb.append("   "+optionname+"\n");
        }
        sb.append("AmbiguousOptionMap\n");
        for (String optionname : new TreeSet<String>(ambiguousOptionMap.keySet())) {
        	sb.append("   "+optionname+"\n");
        }
        sb.append("CharacterOptionMap\n");
        for (String flag : new TreeSet<String>(flagOptionMap.keySet())) {
        	sb.append("   -"+flag+" -> "+flagOptionMap.get(flag).getName()+"\n");
        }
		return sb.toString();	
	}
	
	/**
	 * Returns a string representation of a option group without the option group name in the string. 
	 * 
	 * @param groupname	The option group name
	 * @return a string representation of a option group
	 */
	public String toStringOptionGroup(String groupname) {
		OptionGroup.toStringSetting = OptionGroup.NOGROUPNAME; 
		return optionGroups.get(groupname).toString()+"\n";
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		OptionGroup.toStringSetting = OptionGroup.WITHGROUPNAME;
		final StringBuilder sb = new StringBuilder();
        for (String groupname : new TreeSet<String>(optionGroups.keySet())) {
        	sb.append(optionGroups.get(groupname).toString()+"\n");
        }	
		return sb.toString();
	}
}
