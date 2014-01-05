package org.maltparser.core.config;

import org.apache.log4j.Logger;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.symbol.SymbolTableHandler;

/**
*
*
* @author Johan Hall
*/
public interface Configuration {
	public ConfigurationDir getConfigurationDir();
	public void setConfigurationDir(ConfigurationDir configDir);
	public Logger getConfigLogger(); 
	public void setConfigLogger(Logger logger); 

	public SymbolTableHandler getSymbolTables();
	public ConfigurationRegistry getRegistry();
	public Object getOptionValue(String optiongroup, String optionname) throws MaltChainedException;
	public String getOptionValueString(String optiongroup, String optionname) throws MaltChainedException;
}
