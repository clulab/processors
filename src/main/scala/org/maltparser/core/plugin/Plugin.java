package org.maltparser.core.plugin;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.jar.Attributes;
import java.util.jar.JarFile;

import org.maltparser.core.exception.MaltChainedException;

/**
The class Plugin contains information about a plug-in that comply to the the MaltParser Plugin Standard.


@author Johan Hall

@since 1.0
 */
public class Plugin {
	private JarFile archive;
	private URL url;
	private String pluginName;


	/**
	 * Creates a plug-in container.
	 * 
	 * @param filename	The file name that contains the plugin
	 * @throws MaltChainedException
	 */
	public Plugin(String filename) throws MaltChainedException {
		this(new File(filename));
	}

	/**
	 * Creates a plug-in container.
	 * 
	 * @param file	The jar file that contains the plugin
	 * @throws MaltChainedException
	 */
	public Plugin(File file) throws MaltChainedException {
		try {
			setArchive(new JarFile(file));
			setUrl(new URL("file", null, file.getAbsolutePath()));
			register();
		} catch (FileNotFoundException e) {
			throw new PluginException("The file '"+file.getPath()+File.separator+file.getName()+"' cannot be found. ", e);
		} catch (MalformedURLException e) {
			throw new PluginException("Malformed URL to the jar file '"+archive.getName()+"'. ", e);
		} catch (IOException e) {
			throw new PluginException("The jar file '"+file.getPath()+File.separator+file.getName()+"' cannot be initialized. ", e);
		}
	}

	/**
	 * @throws MaltChainedException
	 */
	private void register() throws MaltChainedException {
		try {
			Attributes atts = archive.getManifest().getMainAttributes();
			pluginName = atts.getValue("Plugin-Name");
			if (pluginName == null) {
				pluginName = archive.getName();
			}
		} catch (IOException e) {
			throw new PluginException("Could not get the 'Plugin-Name' in the manifest for the plugin (jar-file). ", e);
		}
	}
	
	/**
	 * Returns the archive.
	 * 
	 * @return the jar archive.
	 */
	public JarFile getArchive() {
		return archive;
	}
	/**
	 * Sets the archive to set.
	 * 
	 * @param archive The archive to set.
	 */
	public void setArchive(JarFile archive) {
		this.archive = archive;
	}
	/**
	 * Returns the plug-in name.
	 * 
	 * @return the plug-in name.
	 */
	public String getPluginName() {
		return pluginName;
	}
	/**
	 * Sets the plug-in name
	 * 
	 * @param pluginName the plug-in name
	 */
	public void setPluginName(String pluginName) {
		this.pluginName = pluginName;
	}

	/**
	 * Returns the URL
	 * 
	 * @return the URL
	 */
	public URL getUrl() {
		return url;
	}
	/**
	 * Sets the URL. 
	 * 
	 * @param url	 the URL
	 */
	public void setUrl(URL url) {
		this.url = url;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder sb = new StringBuilder();
		
		sb.append(pluginName+" : "+url.toString());
		return sb.toString();
	}
}