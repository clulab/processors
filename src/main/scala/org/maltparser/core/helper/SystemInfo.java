package org.maltparser.core.helper;

import java.io.IOException;
import java.net.URL;
import java.util.Properties;

import org.maltparser.core.options.OptionManager;

/**
 * 
 * 
 * @author Johan Hall
 */
public class SystemInfo {
	private static SystemInfo uniqueInstance = new SystemInfo();
	private static String version;
	private static String buildDate;

	private SystemInfo() {
		URL url = getClass().getResource("/appdata/release.properties");
		if (url != null) {
			Properties properties = new Properties();
			try {
			   properties.load(url.openStream());
			} catch (IOException e) {
				
			}
			version = properties.getProperty("version", "undef");
			buildDate = properties.getProperty("builddate", "undef");
		} else {
			version = "undef";
			buildDate = "undef";
		}
	}

	/**
	 * Returns a reference to the single instance.
	 */
	public static SystemInfo instance() {
		return uniqueInstance;
	}

	/**
	 * Returns the application header
	 * 
	 * @return the application header
	 */
	public static String header() {
		StringBuilder sb = new StringBuilder();
		sb
				.append("-----------------------------------------------------------------------------\n"
						+ "                          MaltParser "+ version + "                             \n"
						+ "-----------------------------------------------------------------------------\n"
						+ "         MALT (Models and Algorithms for Language Technology) Group          \n"
						+ "             Vaxjo University and Uppsala University                         \n"
						+ "                             Sweden                                          \n"
						+ "-----------------------------------------------------------------------------\n");
		return sb.toString();
	}

	/**
	 * Returns a short version of the help
	 * 
	 * @return a short version of the help
	 */
	public static String shortHelp() {
		StringBuilder sb = new StringBuilder();
		sb.append("\n"
				+ "Usage: \n"
				+ "   java -jar maltparser-"+version+".jar -f <path to option file> <options>\n"
				+ "   java -jar maltparser-"+version+".jar -h for more help and options\n\n"
				+ OptionManager.instance().getOptionDescriptions()
						.toStringOptionGroup("system")
				+ "Documentation: docs/index.html\n");
		return sb.toString();
	}


	/**
	 * Returns the version number as string
	 * 
	 * @return the version number as string
	 */
	public static String getVersion() {
		return version;
	}

	/**
	 * Returns the build date
	 * 
	 * @return the build date
	 */
	public static String getBuildDate() {
		return buildDate;
	}
}
