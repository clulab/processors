package org.maltparser.core.options;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;

import java.util.Formatter;
import java.util.HashMap;
import java.util.Set;
import java.util.regex.Pattern;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.options.option.ClassOption;
import org.maltparser.core.options.option.Option;
import org.maltparser.core.options.option.UnaryOption;
import org.maltparser.core.plugin.PluginLoader;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


/**
 *  Option Manager is the management class for all option handling. All queries and manipulations of an option or an option value
 *   should go through this class. 
 *  
 * @author Johan Hall
 * @since 1.0
**/
public class OptionManager {
	public static final int DEFAULTVALUE = -1;
	private OptionDescriptions optionDescriptions;
	private OptionValues optionValues;
	private static OptionManager uniqueInstance = new OptionManager();
	
	/**
	 * Creates the Option Manager
	 */
	private OptionManager() {
		optionValues = new OptionValues();
		optionDescriptions = new OptionDescriptions();
	}
	
	/**
	* Returns a reference to the single instance.
	*/
	public static OptionManager instance() {
		return uniqueInstance;
	}
	
	/**
	 * Loads the option description file <code>/appdata/options.xml</code>
	 * 
	 * @throws MaltChainedException
	 */
	public void loadOptionDescriptionFile() throws MaltChainedException {
		optionDescriptions.parseOptionDescriptionXMLfile(getClass().getResource("/appdata/options.xml"));
	}
	

	/**
	 * Loads the option description file
	 * 
	 * @param url	URL of the option description file
	 * @throws MaltChainedException
	 */
	public void loadOptionDescriptionFile(URL url) throws MaltChainedException {
		optionDescriptions.parseOptionDescriptionXMLfile(url);
	}
	
	/**
	 * Returns the option description 
	 * 
	 * @return the option description 
	 */
	public OptionDescriptions getOptionDescriptions() {
		return optionDescriptions;
	}
	
	/**
	 * Returns the option value for an option that is specified by the option group name and option name. The
	 * container name points out the specific option container. 
	 * 
	 * 
	 * @param containerIndex	The index of the option container (0..n and -1 is default values). 
	 * @param optiongroup	The name of the option group.
	 * @param optionname	The name of the option.
	 * @return an object that contains the value of the option, <i>null</i> if the option value could not be found.
	 * @throws OptionException
	 */
	public Object getOptionValue(int containerIndex, String optiongroup, String optionname) throws MaltChainedException {
		Option option = optionDescriptions.getOption(optiongroup, optionname);

		if (containerIndex == OptionManager.DEFAULTVALUE) {
			return option.getDefaultValueObject();
		} 
		Object value = optionValues.getOptionValue(containerIndex, option);
		if (value == null) {
			value = option.getDefaultValueObject();
		}
		return value;
	}
	
	public Object getOptionDefaultValue(String optiongroup, String optionname) throws MaltChainedException {
		Option option = optionDescriptions.getOption(optiongroup, optionname);
		return option.getDefaultValueObject();
	}
	
	public Object getOptionValueNoDefault(int containerIndex, String optiongroup, String optionname) throws MaltChainedException {
		Option option = optionDescriptions.getOption(optiongroup, optionname);

		if (containerIndex == OptionManager.DEFAULTVALUE) {
			return option.getDefaultValueObject();
		} 
		return optionValues.getOptionValue(containerIndex, option);
	}
	
	/**
	 * Returns a string representation of the option value for an option that is specified by the option group name and the option name. The
	 * container name points out the specific option container. 
	 * 
	 * @param containerIndex	The index of the option container (0..n and -1 is default values). 
	 * @param optiongroup	The name of the option group.
	 * @param optionname	The name of the option.
	 * @return a string representation of the option value
	 * @throws MaltChainedException
	 */
	public String getOptionValueString(int containerIndex, String optiongroup, String optionname) throws MaltChainedException {
		Option option = optionDescriptions.getOption(optiongroup, optionname);
		String value = optionValues.getOptionValueString(containerIndex, option);
		if (value == null) {
			value = option.getDefaultValueString();
		}
		return value;
	}
	
	public String getOptionValueStringNoDefault(int containerIndex, String optiongroup, String optionname) throws MaltChainedException {
		return optionValues.getOptionValueString(containerIndex, optionDescriptions.getOption(optiongroup, optionname));
	}
	
	/**
	 * Overloads the option value specified by the container index, the option group name, the option name.
	 * This method is used to override option that have specific dependencies. 
	 * 
	 * @param containerIndex	the index of the option container (0..n and -1 is default values). 
	 * @param optiongroup	the name of the option group.
	 * @param optionname	the name of the option.
	 * @param value the option value that should replace the current option value.
	 * @throws MaltChainedException
	 */
	public void overloadOptionValue(int containerIndex, String optiongroup, String optionname, String value) throws MaltChainedException {
		Option option = optionDescriptions.getOption(optiongroup, optionname);
		if (value == null) {
    		throw new OptionException("The option value is missing. ");
    	}
    	Object ovalue = option.getValueObject(value);
    	optionValues.addOptionValue(OptionContainer.DEPENDENCIES_RESOLVED, containerIndex, option, ovalue);
	}
	
	/**
	 * Returns the number of option values for a particular option container.
	 * 
	 * @param containerIndex	The index of the option container (0..n). 
	 * @return the number of option values for a particular option container.
	 */
	public int getNumberOfOptionValues(int containerIndex) {
		return optionValues.getNumberOfOptionValues(containerIndex);
	}
	
	/**
	 * Returns a sorted set of container names.
	 * 
	 * @return	a sorted set of container names.
	 */
	public Set<Integer> getOptionContainerIndices() {
		return optionValues.getOptionContainerIndices();
	}
	/**
	 * Loads the saved options (options that are marked with <code>usage=save</code>). 
	 * 
	 * @param fileName	The path to the file where to load the saved options.
	 * @throws MaltChainedException
	 */
	public void loadOptions(int containerIndex, String fileName) throws MaltChainedException { 
		try {
			loadOptions(containerIndex, new InputStreamReader(new FileInputStream(fileName), "UTF-8"));
		} catch (FileNotFoundException e) {
			throw new OptionException("The saved option file '"+fileName+"' cannot be found. ", e);
		} catch (UnsupportedEncodingException e) {
			throw new OptionException("The charset is unsupported. ", e);
		}
	}
	

	/**
	 * Loads the saved options (options that are marked with <code>usage=Option.SAVE</code>). 
	 * 
	 * @param isr	the input stream reader of the saved options file.
	 * @throws MaltChainedException
	 */
	public void loadOptions(int containerIndex, InputStreamReader isr) throws MaltChainedException { 
		try {
			BufferedReader br = new BufferedReader(isr);
			String line = null;
			Option option = null;
			Pattern tabPattern = Pattern.compile("\t");
			while ((line = br.readLine()) != null) {
				String[] items = tabPattern.split(line);
				if (items.length < 3 || items.length > 4) {
					throw new OptionException("Could not load the saved option. ");
				}
				option = optionDescriptions.getOption(items[1], items[2]);
				Object ovalue;
				if (items.length == 3) {
					ovalue = new String("");
				} else {
					if (option instanceof ClassOption) {
						if (items[3].startsWith("class ")) {
							Class<?> clazz = null;
							if (PluginLoader.instance() != null) {
								clazz = PluginLoader.instance().getClass(items[3].substring(6));
							}
							if (clazz == null) {
								clazz = Class.forName(items[3].substring(6));
							}
							ovalue = option.getValueObject(((ClassOption)option).getLegalValueString(clazz));
						} else {
							ovalue = option.getValueObject(items[3]);
						}
					} else {
						ovalue = option.getValueObject(items[3]);
					}
				}
				optionValues.addOptionValue(OptionContainer.SAVEDOPTION, containerIndex, option, ovalue);
			}

			br.close();
		} catch (ClassNotFoundException e) {
			throw new OptionException("The class cannot be found. ", e);
		} catch (NumberFormatException e) {
			throw new OptionException("Option container index isn't an integer value. ", e);
		} catch (IOException e) {
			throw new OptionException("Error when reading the saved options. ", e);
		}
	}
	
	/**
	 * Saves all options that are marked as <code>usage=Option.SAVE</code>
	 * 
	 * @param fileName	The path to the file where the saveOption should by saved.
	 */
	public void saveOptions(String fileName) throws MaltChainedException { 
		try {
			saveOptions(new OutputStreamWriter(new FileOutputStream(fileName), "UTF-8"));
		} catch (FileNotFoundException e) {
			throw new OptionException("The file '"+fileName+"' cannot be created. ", e);
		} catch (UnsupportedEncodingException e) {
			throw new OptionException("The charset 'UTF-8' is unsupported. ", e);
		}
		
	}
	
	/**
	 * Saves all options that are marked as <code>usage=Option.SAVE</code>
	 * 
	 * @param osw	the output stream writer of the saved option file
	 * @throws MaltChainedException
	 */
	public void saveOptions(OutputStreamWriter osw) throws MaltChainedException { 
		try {
			BufferedWriter bw = new BufferedWriter(osw);
			Set<Option> optionToSave = optionDescriptions.getSaveOptionSet();
			
			Object value = null;
			for (Integer index : optionValues.getOptionContainerIndices()) {
				for (Option option : optionToSave) {
					value = optionValues.getOptionValue(index, option);
					if (value == null) {
						value = option.getDefaultValueObject();
					}
					bw.append(index+"\t"+option.getGroup().getName()+"\t"+option.getName()+"\t"+value+"\n");
				}
			}
			bw.flush();
			bw.close();
		} catch (IOException e) {
			throw new OptionException("Error when saving the saved options. ", e);
		} 
	}
	
	/**
	 * Saves all options that are marked as usage=Option.SAVE for a particular option container.
	 * 
	 * @param containerIndex	The index of the option container (0..n). 
	 * @param fileName	The path to the file where the saveOption should by saved.
	 */
	public void saveOptions(int containerIndex, String fileName) throws MaltChainedException { 
		try {
			saveOptions(containerIndex, new OutputStreamWriter(new FileOutputStream(fileName), "UTF-8"));
		} catch (FileNotFoundException e) {
			throw new OptionException("The file '"+fileName+"' cannot be found.", e);
		} catch (UnsupportedEncodingException e) {
			throw new OptionException("The charset 'UTF-8' is unsupported. ", e);
		}
	}

	/**
	 * Saves all options that are marked as usage=Option.SAVE for a particular option container.
	 * 
	 * @param containerIndex The index of the option container (0..n). 
	 * @param osw 	the output stream writer of the saved option file
	 * @throws MaltChainedException
	 */
	public void saveOptions(int containerIndex, OutputStreamWriter osw) throws MaltChainedException { 
		try {
			BufferedWriter bw = new BufferedWriter(osw);
			Set<Option> optionToSave = optionDescriptions.getSaveOptionSet();
			
			Object value = null;
			for (Option option : optionToSave) {
				value = optionValues.getOptionValue(containerIndex, option);
				if (value == null) {
					value = option.getDefaultValueObject();
				}
				bw.append(containerIndex+"\t"+option.getGroup().getName()+"\t"+option.getName()+"\t"+value+"\n");
			}

			bw.flush();
			bw.close();
		} catch (IOException e) {
			throw new OptionException("Error when saving the saved options.", e);
		} 
	}

	/**
	 * Creates several option maps for fast access to individual options.  
	 * 
	 * @throws OptionException
	 */
	public void generateMaps() throws MaltChainedException {
		optionDescriptions.generateMaps();
	}
	
	public boolean parseCommandLine(String argString, int containerIndex) throws MaltChainedException {
		return parseCommandLine(argString.split(" "), containerIndex);
	}
	
	/**
	 * Parses the command line arguments.
	 * 
	 * @param args An array of arguments that are supplied when starting the application. 
	 * @throws OptionException
	 */
	public boolean parseCommandLine(String[] args, int containerIndex) throws MaltChainedException {
		if (args == null  || args.length == 0) {
			return false;
		}
		int i = 0;
		HashMap<String,String> oldFlags = new HashMap<String,String>();
		oldFlags.put("llo", "lo"); oldFlags.put("lso", "lo"); 
		oldFlags.put("lli", "li"); oldFlags.put("lsi", "li");
		oldFlags.put("llx", "lx"); oldFlags.put("lsx", "lx");
		oldFlags.put("llv", "lv"); oldFlags.put("lsv", "lv");
		while (i < args.length) {
			Option option = null;
			String value = null;
			/* Recognizes
			 * --optiongroup-optionname=value
			 * --optionname=value
			 * --optiongroup-optionname (unary option)
			 * --optionname (unary option)
			 */ 
			if (args[i].startsWith("--")) {
				if (args[i].length() == 2) {
					throw new OptionException("The argument contains only '--', please check the user guide to see the correct format. ");
				}
				String optionstring;
				String optiongroup;
				String optionname;
				int indexEqualSign = args[i].indexOf('=');
				if (indexEqualSign != -1) {
					value = args[i].substring(indexEqualSign+1);
					optionstring = args[i].substring(2, indexEqualSign);
				} else {
					value = null;
					optionstring = args[i].substring(2);
				}
				int indexMinusSign = optionstring.indexOf('-');
				if (indexMinusSign != -1) {
					optionname = optionstring.substring(indexMinusSign+1);
					optiongroup = optionstring.substring(0, indexMinusSign);				
				} else {
					optiongroup = null;
					optionname = optionstring;
				}
				
				option = optionDescriptions.getOption(optiongroup, optionname);
				if (option instanceof UnaryOption) {
					value = "used";
				}
				i++;
			} 
			/* Recognizes
			 * -optionflag value
			 * -optionflag (unary option)
			 */
			else if (args[i].startsWith("-")) {
				if (args[i].length() < 2) {
					throw new OptionException("Wrong use of option flag '"+args[i]+"', please check the user guide to see the correct format. ");
				}
				String flag = "";
				if (oldFlags.containsKey(args[i].substring(1))) {
					flag = oldFlags.get(args[i].substring(1));
				} else {
					flag = args[i].substring(1);
				}
				
				// Error message if the old flag '-r' (root handling) is used 
				if (args[i].substring(1).equals("r")) {
					throw new OptionException("The flag -r (root_handling) is replaced with two flags -nr (allow_root) and -ne (allow_reduce) since MaltParser 1.7. Read more about these changes in the user guide.");
				}
				
			    option = optionDescriptions.getOption(flag);

				if (option instanceof UnaryOption) {
					value = "used";
				} else {
					i++;
					if (args.length > i) {
						value = args[i];
					} else {
						throw new OptionException("Could not find the corresponding value for -"+option.getFlag()+". ");
					}
				}
				i++;
			} else {
				throw new OptionException("The option should starts with a minus sign (-), error at argument '"+args[i]+"'");
			}
			Object optionvalue = option.getValueObject(value);
			optionValues.addOptionValue(OptionContainer.COMMANDLINE, containerIndex, option, optionvalue);
		}
		return true;
	}
	
	
	/**
	 * Parses the option file for option values. 
	 * 
	 * @param fileName The option file name (must be a xml file).
	 * @throws OptionException
	 */
	public void parseOptionInstanceXMLfile(String fileName) throws MaltChainedException {
		File file = new File(fileName);
		
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            
    		Element root = db.parse(file).getDocumentElement();
    		NodeList containers = root.getElementsByTagName("optioncontainer");
            Element container;
            for (int i = 0; i < containers.getLength(); i++) {
            	container = (Element)containers.item(i);
            	parseOptionValues(container, i);
            }	
        } catch (IOException e) {
        	throw new OptionException("Can't find the file "+fileName+". ", e);
        } catch (OptionException e) {
        	throw new OptionException("Problem parsing the file "+fileName+". ", e);
        } catch (ParserConfigurationException e) {
        	throw new OptionException("Problem parsing the file "+fileName+". ", e);
        } catch (SAXException e) {
        	throw new OptionException("Problem parsing the file "+fileName+". ", e);
        }
	}
	
	/**
	 * Parses an option container for option values.
	 * 
	 * @param container	a reference to an individual option container in the DOM tree.
	 * @param containerName	the name of this container.
	 * @throws OptionException
	 */
	private void parseOptionValues(Element container, int containerIndex) throws MaltChainedException {
		NodeList optiongroups = container.getElementsByTagName("optiongroup");
        Element optiongroup;
        for (int i = 0; i < optiongroups.getLength(); i++) {
        	optiongroup = (Element)optiongroups.item(i);
        	String groupname = optiongroup.getAttribute("groupname").toLowerCase();
        	if (groupname == null) {
        		throw new OptionException("The option group name is missing. ");
        	}
    		NodeList optionvalues = optiongroup.getElementsByTagName("option");
            Element optionvalue;
            
            for (int j = 0; j < optionvalues.getLength(); j++) {
            	optionvalue = (Element)optionvalues.item(j); 
            	String optionname = optionvalue.getAttribute("name").toLowerCase();
            	String value = optionvalue.getAttribute("value");
            	
            	if (optionname == null) {
            		throw new OptionException("The option name is missing. ");
            	}

            	Option option = optionDescriptions.getOption(groupname, optionname);

            	if (option instanceof UnaryOption) {
					value = "used";
				}
            	if (value == null) {
            		throw new OptionException("The option value is missing. ");
            	}
            	Object ovalue = option.getValueObject(value);
            	optionValues.addOptionValue(OptionContainer.OPTIONFILE, containerIndex, option, ovalue);
            }
        }
	}
	
	/**
	 * Returns a string representation of all option value, except the options in a option group specified
	 * by the excludeGroup argument.
	 * 
	 * @param containerIndex The index of the option container (0..n and -1 is default values). 
	 * @param excludeGroups a set of option group names that should by excluded in the string representation
	 * @return a string representation of all option value
	 * @throws MaltChainedException
	 */
	public String toStringPrettyValues(int containerIndex, Set<String> excludeGroups) throws MaltChainedException {
		int reservedSpaceForOptionName = 30;
		OptionGroup.toStringSetting = OptionGroup.WITHGROUPNAME;
		StringBuilder sb = new StringBuilder();
		if (containerIndex == OptionManager.DEFAULTVALUE) {
			for (String groupname : optionDescriptions.getOptionGroupNameSet()) {
				if (excludeGroups.contains(groupname)) continue;
				sb.append(groupname+"\n");
				for (Option option : optionDescriptions.getOptionGroupList(groupname)) {
					int nSpaces = reservedSpaceForOptionName - option.getName().length();
					if (nSpaces <= 1) {
						nSpaces = 1;
					}
					sb.append(new Formatter().format("  %s (%4s)%"+nSpaces+"s %s\n", option.getName(), "-"+option.getFlag()," ", option.getDefaultValueString()));
				}
			}
		} else {
			for (String groupname : optionDescriptions.getOptionGroupNameSet()) {
				if (excludeGroups.contains(groupname)) continue;
				sb.append(groupname+"\n");
				for (Option option : optionDescriptions.getOptionGroupList(groupname)) {
					String value = optionValues.getOptionValueString(containerIndex, option);
					int nSpaces = reservedSpaceForOptionName - option.getName().length();
					if (nSpaces <= 1) {
						nSpaces = 1;
					}
					
					if (value == null) {
						sb.append(new Formatter().format("  %s (%4s)%"+nSpaces+"s %s\n", option.getName(), "-"+option.getFlag(), " ", option.getDefaultValueString()));
					} else {
						sb.append(new Formatter().format("  %s (%4s)%"+nSpaces+"s %s\n", option.getName(), "-"+option.getFlag(), " ", value));
					}
				}
			}
		}
		return sb.toString();
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(optionDescriptions+"\n");
		sb.append(optionValues+"\n");
		return sb.toString();
	}
}
