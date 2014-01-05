package org.maltparser.core.helper;


import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.plugin.Plugin;
import org.maltparser.core.plugin.PluginLoader;

/**
*
*
* @author Johan Hall
*/
public class URLFinder {
	/**
	 * Search for a file according the following priority:
	 * <ol>
	 * <li>The local file system
	 * <li>Specified as an URL (starting with http:, file:, ftp: or jar:
	 * <li>MaltParser distribution file (malt.jar)
	 * <li>MaltParser plugins
	 * </ol>
	 * 
	 * If the file string is found, an URL object is returned, otherwise <b>null</b>
	 * 
	 * @param fileString	the file string to convert into an URL.
	 * @return an URL object, if the file string is found, otherwise <b>null</b>
	 * @throws MaltChainedException
	 */
	public URL findURL(String fileString) throws MaltChainedException {
		File specFile = new File(fileString);

		try {
			if (specFile.exists()) {
				// found the file in the file system
				return new URL("file:///"+specFile.getAbsolutePath());
			} else if (fileString.startsWith("http:") || fileString.startsWith("file:") || fileString.startsWith("ftp:") || fileString.startsWith("jar:")) {
				// the input string is an URL string starting with http, file, ftp or jar
				return new URL(fileString);
			} else {
				return findURLinJars(fileString);
			} 
		} catch (MalformedURLException e) {
			throw new MaltChainedException("Malformed URL: "+fileString, e);
		}
	}
	
	public URL findURLinJars(String fileString) throws MaltChainedException {
		try {
			// search in malt.jar and its plugins
			if (getClass().getResource(fileString) != null) {
				// found the input string in the malt.jar file
				return getClass().getResource(fileString);
			} else { 
				 for (Plugin plugin : PluginLoader.instance()) {
					URL url = null;
					if (!fileString.startsWith("/")) {
						url = new URL("jar:"+plugin.getUrl() + "!/" + fileString);
					} else {
						url = new URL("jar:"+plugin.getUrl() + "!" + fileString);
					}
					
					try { 
						InputStream is = url.openStream();
						is.close();
					} catch (IOException e) {
						continue;
					}
					// found the input string in one of the plugins
					return url;
				} 
				// could not convert the input string into an URL
				return null; 
			}
		} catch (MalformedURLException e) {
			throw new MaltChainedException("Malformed URL: "+fileString, e);
		}
	}
}
