package org.maltparser.core.plugin;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeSet;
import org.maltparser.core.exception.MaltChainedException;

/**
Loads MaltParser plug-ins and makes new instances of classes within these plug-ins. 

@author Johan Hall

@since 1.0
 */
public class PluginLoader implements Iterable<Plugin> {
	private HashMap<String, Plugin> plugins;
	private TreeSet<String> pluginNames;
	private File[] directories;
	private JarLoader jarLoader;
	private static PluginLoader uniqueInstance = new PluginLoader();
	
	/**
	 * Creates a PluginLoader
	 * 
	 * @throws PluginException
	 */
	private PluginLoader() {
		pluginNames = new TreeSet<String>();
		plugins = new HashMap<String, Plugin>();
		jarLoader = null;
	}
	
	/**
	* Returns a reference to the single instance.
	*/
	public static PluginLoader instance() {
		return uniqueInstance;
	}
	
	/**
	 * Loads plug-ins from one directory
	 * 
	 * @param pluginDirectory The directory that contains all plug-ins
	 * @throws MaltChainedException
	 */
	public void loadPlugins(File pluginDirectory) throws MaltChainedException {
		this.loadPlugins(new File[] {pluginDirectory});
	}
	
	/**
	 * Loads plug-ins from one or more directories
	 * 
	 * @param pluginDirectories An array of directories that contains all plug-ins
	 * @throws MaltChainedException
	 */
	public void loadPlugins(File[] pluginDirectories) throws MaltChainedException {
		directories = new File[pluginDirectories.length];
		for (int i = 0; i < directories.length; i++) {
			directories[i] = pluginDirectories[i];
		}
		
		try {
			Class<?> self = Class.forName("org.maltparser.core.plugin.PluginLoader");
			jarLoader = new JarLoader(self.getClassLoader());		
		} catch (ClassNotFoundException e) {
			throw new PluginException("The class 'org.maltparser.core.plugin.PluginLoader' not found. ", e);
		}	
		traverseDirectories();
	}
	
	/**
	 * Traverse all the plug-in directories
	 * 
	 * @throws MaltChainedException
	 */
	private void traverseDirectories() throws MaltChainedException {
		for (int i = 0; i < directories.length; i++) {
			traverseDirectory(directories[i]);
		}
	}
	
	/**
	 * Traverse all plug-ins and sub-directories within one plug-in directory.
	 * 
	 * @param directory The directory that contains plug-ins
	 * @throws MaltChainedException
	 */
	private void traverseDirectory(File directory) throws MaltChainedException {
		if (!directory.isDirectory() && directory.getName().endsWith(".jar")) {
			pluginNames.add(directory.getAbsolutePath());
			Plugin plugin = new Plugin(directory);
			plugins.put(directory.getAbsolutePath(), plugin);
			if (jarLoader.readJarFile(plugin.getUrl()) == false) {
				plugins.remove(directory.getAbsolutePath());
			}
		}
        
        if (directory.isDirectory()) {
            String[] children = directory.list();
            for (int i=0; i<children.length; i++) {
            	traverseDirectory(new File(directory, children[i]));
            }
        }
	}
	
	/**
	 * Returns the Class object for the class with the specified name.
	 * 
	 * @param classname the fully qualified name of the desired class
	 * @return the Class object for the class with the specified name.
	 */
	public Class<?> getClass(String classname) {
		if (jarLoader != null) {
			return jarLoader.getClass(classname);
		} else {
			return null;
		}
	}
	
	/**
	 * Creates a new instance of a class within one of the plug-ins
	 * 
	 * @param classname The fully qualified name of the desired class
	 * @param argTypes An array of classes (fully qualified name) that specify the arguments to the constructor 
	 * @param args An array of objects that will be the actual parameters to the constructor (the type should corresponds to the argTypes).
	 * @return a reference to the created instance.
	 * @throws MaltChainedException
	 */
	public Object newInstance(String classname, Class<?>[] argTypes, Object[] args) throws MaltChainedException {
		try {
			if (jarLoader == null) {
				return null;
			}
			Class<?> clazz = jarLoader.getClass(classname);
			Object o = null;
			if (clazz == null)
				return null;
			if (argTypes != null) {
				Constructor<?> constructor = clazz.getConstructor(argTypes);
				o = constructor.newInstance(args);
			} else {
				o = clazz.newInstance();
			}
			return o;
		} catch (NoSuchMethodException e) {
			throw new PluginException("The plugin loader was not able to create an instance of the class '"+classname+"'. ", e);
		} catch (InstantiationException e) {
			throw new PluginException("The plugin loader was not able to create an instance of the class '"+classname+"'. ", e);
		} catch (IllegalAccessException e) {
			throw new PluginException("The plugin loader was not able to create an instance of the class '"+classname+"'. ", e);
		} catch (InvocationTargetException e) {
			throw new PluginException("The plugin loader was not able to create an instance of the class '"+classname+"'. ", e);
		}
	}
	
	public Iterator<Plugin> iterator() {
		return plugins.values().iterator();
	}
	
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (Plugin plugin : this) {
			sb.append(plugin.toString() + "\n");
		}
		return sb.toString();
	}
}
