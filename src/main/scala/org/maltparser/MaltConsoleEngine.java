package org.maltparser;

import java.util.Date;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.SystemInfo;
import org.maltparser.core.helper.SystemLogger;
import org.maltparser.core.options.OptionManager;

/**
 * MaltConsoleEngine controls the MaltParser system using the console version. 
 * 
 * @author Johan Hall
 * @since 1.0
**/
public class MaltConsoleEngine {
	public static final int OPTION_CONTAINER = 0;
	
	/**
	 * Creates a MaltConsoleEngine object
	 * 
	 */
	public MaltConsoleEngine() {
		try {
			/* Option and Plug-in management */
			OptionManager.instance().loadOptionDescriptionFile();
//			if (SystemInfo.getMaltJarPath() != null) {
//				PluginLoader.instance().loadPlugins(new File(SystemInfo.getMaltJarPath().getParent()+"/plugin"));
//			}
			OptionManager.instance().generateMaps();
		} catch (MaltChainedException e) {
			if (SystemLogger.logger().isDebugEnabled()) {
				SystemLogger.logger().debug("",e);
			} else {
				SystemLogger.logger().error(e.getMessageChain());
			}
			System.exit(1);
		}
	}
	
	/**
	 * Starts the console engine.
	 * 
	 * @param args command-line arguments
	 */
	public void startEngine(String[] args) {
		try {
			final OptionManager om = OptionManager.instance();
			final boolean hasArg = om.parseCommandLine(args,OPTION_CONTAINER);
			/* Update the verbosity level according to the verbosity option */
			String verbosity = null;
			if (hasArg) {
				verbosity = (String)OptionManager.instance().getOptionValue(OPTION_CONTAINER,"system", "verbosity");
			} else {
				verbosity = (String)OptionManager.instance().getOptionDefaultValue("system", "verbosity");
			}
			if (verbosity != null) {
				SystemLogger.instance().setSystemVerbosityLevel(verbosity.toUpperCase());
			}
			/* Help or reading the option file */
			if (!hasArg || om.getNumberOfOptionValues(OPTION_CONTAINER) == 0) {
				SystemLogger.logger().info(SystemInfo.header());
				SystemLogger.logger().info(SystemInfo.shortHelp());
				return;
			} else if (om.getOptionValue(OPTION_CONTAINER,"system", "help") != null) {
				SystemLogger.logger().info(SystemInfo.header());
				SystemLogger.logger().info(om.getOptionDescriptions());
				return;
			} else {
				if (om.getOptionValue(OPTION_CONTAINER,"system", "option_file") != null && om.getOptionValue(0,"system", "option_file").toString().length() > 0) {
					om.parseOptionInstanceXMLfile((String)om.getOptionValue(OPTION_CONTAINER,"system", "option_file"));
				}
			}
			maltParser();
		} catch (MaltChainedException e) {
			if (SystemLogger.logger().isDebugEnabled()) {
				SystemLogger.logger().debug("",e);
			} else {
				SystemLogger.logger().error(e.getMessageChain());
			}
			System.exit(1);
		}
	}
	
	/**
	 * Creates and executes a MaltParser configuration
	 * 
	 * @throws MaltChainedException
	 */
	private void maltParser() throws MaltChainedException {
		if (SystemLogger.logger() != null && SystemLogger.logger().isInfoEnabled()) {
			SystemLogger.logger().info(SystemInfo.header() +"\n");
			SystemLogger.logger().info("Started: " + new Date(System.currentTimeMillis()) +"\n");
		}
		Engine engine = new Engine();
		engine.initialize(OPTION_CONTAINER);
		engine.process(OPTION_CONTAINER);
		engine.terminate(OPTION_CONTAINER);
		if (SystemLogger.logger().isInfoEnabled()) {
			SystemLogger.logger().info("Finished: " + new Date(System.currentTimeMillis())+"\n");
		}
	}
}
