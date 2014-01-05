package org.maltparser.core.plugin;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.SecureClassLoader;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;
import java.util.regex.PatternSyntaxException;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.HashSet;
import org.maltparser.core.options.OptionManager;


/**
The jar class loader loads the content of a jar file that complies with a MaltParser Plugin.

@author Johan Hall
 */
public class JarLoader extends SecureClassLoader {
	private HashMap<String, byte[]> classByteArrays;
	private HashMap<String, Class<?>> classes;
	
	/**
	 * Creates a new class loader that is specialized for loading jar files.
	 * 
	 * @param parent The parent class loader
	 */
	public JarLoader(ClassLoader parent) {
		super(parent);
		classByteArrays = new HashMap<String, byte[]>();
		classes = new HashMap<String, Class<?>>();
	}
	
	/* (non-Javadoc)
	 * @see java.lang.ClassLoader#findClass(java.lang.String)
	 */
	protected Class<?> findClass(String name) {
		String urlName = name.replace('.', '/');
		byte buf[];

		SecurityManager sm = System.getSecurityManager();
		if (sm != null) {
			int i = name.lastIndexOf('.');
			if (i >= 0) {
				sm.checkPackageDefinition(name.substring(0, i));
			}
		} 

		buf = (byte[]) classByteArrays.get(urlName);
		if (buf != null) {
			return defineClass(null, buf, 0, buf.length);
		}
		return null;
	}

	/**
	 * Loads the content of a jar file that comply with a MaltParser Plugin  
	 * 
	 * @param jarUrl The URL to the jar file
	 * @throws PluginException
	 */
	public boolean readJarFile(URL jarUrl) throws MaltChainedException {
		JarInputStream jis;
		JarEntry je;
		Set<URL> pluginXMLs = new HashSet<URL>();
		
		/*if (logger.isDebugEnabled()) {
			logger.debug("Loading jar " + jarUrl+"\n");
		}*/
		JarFile jarFile;
		try {
			jarFile = new JarFile(jarUrl.getFile());
		} catch (IOException e) {
			throw new PluginException("Could not open jar file " + jarUrl+". ", e);
		}
		try {
	        Manifest manifest = jarFile.getManifest();
	        if (manifest != null) {
		        Attributes manifestAttributes = manifest.getMainAttributes();
		        if (!(manifestAttributes.getValue("MaltParser-Plugin") != null && manifestAttributes.getValue("MaltParser-Plugin").equals("true"))) {
		        	return false;
		        }
		        if (manifestAttributes.getValue("Class-Path") != null) {
		        	String[] classPathItems = manifestAttributes.getValue("Class-Path").split(" ");
		        	for (int i=0; i < classPathItems.length; i++) {
		        		URL u;
		        		try {
		        			u = new URL(jarUrl.getProtocol()+":"+new File(jarFile.getName()).getParentFile().getPath()+"/"+classPathItems[i]);
		        		} catch (MalformedURLException e) {
		        			throw new PluginException("The URL to the plugin jar-class-path '"+jarUrl.getProtocol()+":"+new File(jarFile.getName()).getParentFile().getPath()+"/"+classPathItems[i]+"' is wrong. ", e);
		        		}
			    		URLClassLoader sysloader = (URLClassLoader)ClassLoader.getSystemClassLoader();
			    		Class<?> sysclass = URLClassLoader.class;
		    			Method method = sysclass.getDeclaredMethod("addURL",new Class[]{URL.class});
		    			method.setAccessible(true);
		    			method.invoke(sysloader,new Object[]{u });
		        	}
		        }
	        }
		} catch (PatternSyntaxException e) {
			throw new PluginException("Could not split jar-class-path entries in the jar-file '"+jarFile.getName()+"'. ", e);
		} catch (IOException e) {
			throw new PluginException("Could not read the manifest file in the jar-file '"+jarFile.getName()+"'. ", e);
		} catch (NoSuchMethodException e) {
			throw new PluginException("", e);
		} catch (IllegalAccessException e) {
			throw new PluginException("", e);
		} catch (InvocationTargetException e) {
			throw new PluginException("", e);
		}
		
        try {
			jis = new JarInputStream(jarUrl.openConnection().getInputStream());

			while ((je = jis.getNextJarEntry()) != null) {
				String jarName = je.getName();
				if (jarName.endsWith(".class")) {
					/* if (logger.isDebugEnabled()) {
						logger.debug("  Loading class: " + jarName+"\n");
					}*/
					loadClassBytes(jis, jarName);
					Class<?> clazz = findClass(jarName.substring(0, jarName.length() - 6));
					classes.put(jarName.substring(0, jarName.length() - 6).replace('/','.'), clazz);
					loadClass(jarName.substring(0, jarName.length() - 6).replace('/', '.'));
				}
				if (jarName.endsWith("plugin.xml")) {
					pluginXMLs.add(new URL("jar:"+jarUrl.getProtocol()+":"+jarUrl.getPath()+"!/"+jarName));
				}
				jis.closeEntry();
			}
			for (URL url : pluginXMLs) {
				/* if (logger.isDebugEnabled()) {
					logger.debug("  Loading "+url+"\n");
				}*/
				OptionManager.instance().loadOptionDescriptionFile(url);
			}
		} catch (MalformedURLException e) {
			throw new PluginException("The URL to the plugin.xml is wrong. ", e);
		} catch (IOException e) {
			throw new PluginException("cannot open jar file " + jarUrl+". ", e);
		} catch (ClassNotFoundException e) {
			throw new PluginException("The class "+e.getMessage() +" can't be found. ", e);
		}
		return true;
	}

	/**
	 * Returns the Class object for the class with the specified name.
	 * 
	 * @param classname the fully qualified name of the desired class
	 * @return the Class object for the class with the specified name.
	 */
	public Class<?> getClass(String classname) {
		return (Class<?>)classes.get(classname);
	}
	
	/**
	 * Reads a jar file entry into a byte array.
	 * 
	 * @param jis The jar input stream
	 * @param jarName The name of a jar file entry
	 * @throws PluginException
	 */
	private void loadClassBytes(JarInputStream jis, String jarName) throws MaltChainedException {
		BufferedInputStream jarBuf = new BufferedInputStream(jis);
		ByteArrayOutputStream jarOut = new ByteArrayOutputStream();
		int b;
		try {
			while ((b = jarBuf.read()) != -1) {
				jarOut.write(b);
			}
			classByteArrays.put(jarName.substring(0, jarName.length() - 6), jarOut.toByteArray());
		} catch (IOException e) {
			throw new PluginException("Error reading entry " + jarName+". ", e);
		}
	}

	/**
	 * Checks package access
	 * 
	 * @param name	the package name
	 */
	protected void checkPackageAccess(String name) {
		SecurityManager sm = System.getSecurityManager();
		if (sm != null) {
			sm.checkPackageAccess(name);
		}
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder sb = new StringBuilder();
		
		sb.append("The MaltParser Plugin Loader (JarLoader)\n");
		sb.append("---------------------------------------------------------------------\n");
		for (String entry : new TreeSet<String>(classes.keySet())) {
			sb.append("   "+entry+"\n");
		}
		return sb.toString();
	}
}
