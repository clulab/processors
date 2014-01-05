package org.maltparser.core.config;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
import java.util.jar.JarOutputStream;

import org.maltparser.core.config.version.Versioning;
import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.HashSet;
import org.maltparser.core.helper.SystemInfo;
import org.maltparser.core.helper.SystemLogger;
import org.maltparser.core.helper.URLFinder;
import org.maltparser.core.io.dataformat.DataFormatInstance;
import org.maltparser.core.io.dataformat.DataFormatManager;
import org.maltparser.core.io.dataformat.DataFormatSpecification.DataStructure;
import org.maltparser.core.io.dataformat.DataFormatSpecification.Dependency;
import org.maltparser.core.options.OptionManager;
import org.maltparser.core.symbol.SymbolTableHandler;
import org.maltparser.core.symbol.trie.TrieSymbolTableHandler;


/**
* This class contains methods for handle the configuration directory.
*
* @author Johan Hall
*/
public class ConfigurationDir  {
	protected static final int BUFFER = 4096;
	protected File configDirectory;
	protected String name;
	protected String type;
	protected File workingDirectory;
	protected URL url;
	protected int containerIndex;
	protected BufferedWriter infoFile = null;
	protected String createdByMaltParserVersion;

	private SymbolTableHandler symbolTables;
	private DataFormatManager dataFormatManager;
	private HashMap<String,DataFormatInstance> dataFormatInstances;
	private URL inputFormatURL; 
	private URL outputFormatURL; 
	
	/**
	 * Creates a configuration directory from a mco-file specified by an URL.
	 * 
	 * @param url	an URL to a mco-file
	 * @throws MaltChainedException
	 */
	public ConfigurationDir(URL url) throws MaltChainedException {
		initWorkingDirectory();
		setUrl(url);
		initNameNTypeFromInfoFile(url);
//		initData();
	}
	
	/**
	 * Creates a new configuration directory or a configuration directory from a mco-file
	 * 
	 * @param name	the name of the configuration
	 * @param type	the type of configuration
	 * @param containerIndex	the container index
	 * @throws MaltChainedException
	 */
	public ConfigurationDir(String name, String type, int containerIndex) throws MaltChainedException {
		setContainerIndex(containerIndex);

		initWorkingDirectory();
		if (name != null && name.length() > 0 && type != null && type.length() > 0) {
			setName(name);
			setType(type);
		} else {
			throw new ConfigurationException("The configuration name is not specified. ");
		}

		setConfigDirectory(new File(workingDirectory.getPath()+File.separator+getName()));
		
		String mode = OptionManager.instance().getOptionValue(containerIndex, "config", "flowchart").toString().trim();
		if (mode.equals("parse")) {
			// During parsing also search for the MaltParser configuration file in the class path
			File mcoPath = new File(workingDirectory.getPath()+File.separator+getName()+".mco");
			if (!mcoPath.exists()) {
				String classpath = System.getProperty("java.class.path");
				String[] items = classpath.split(System.getProperty("path.separator"));
				boolean found = false;
				for (String item : items) {
					File candidateDir = new File(item);
					if (candidateDir.exists() && candidateDir.isDirectory()) {
						File candidateConfigFile = new File(candidateDir.getPath()+File.separator+getName()+".mco");
						if (candidateConfigFile.exists()) {
							initWorkingDirectory(candidateDir.getPath());
							setConfigDirectory(new File(workingDirectory.getPath()+File.separator+getName()));
							found = true;
							break;
						}
					}
				}
				if (found == false) {
					throw new ConfigurationException("Couldn't find the MaltParser configuration file: " + getName()+".mco");
				}
			}
	        try {
	        	url = mcoPath.toURI().toURL();
	        } catch (MalformedURLException e) {
	        	// should never happen
	        	throw new ConfigurationException("File path could not be represented as a URL.");
	        }
		}
	}
	
	public void initDataFormat() throws MaltChainedException {
		String inputFormatName = OptionManager.instance().getOptionValue(containerIndex, "input", "format").toString().trim();
		String outputFormatName = OptionManager.instance().getOptionValue(containerIndex, "output", "format").toString().trim();
		final URLFinder f = new URLFinder();

		if (configDirectory != null && configDirectory.exists()) {
			if (outputFormatName.length() == 0 || inputFormatName.equals(outputFormatName)) {
				URL inputFormatURL = f.findURLinJars(inputFormatName);
				if (inputFormatURL != null) {
					outputFormatName = inputFormatName = this.copyToConfig(inputFormatURL);
				} else {
					outputFormatName = inputFormatName = this.copyToConfig(inputFormatName);
				}
			} else {
				URL inputFormatURL = f.findURLinJars(inputFormatName);
				if (inputFormatURL != null) {
					inputFormatName = this.copyToConfig(inputFormatURL);
				} else {
					inputFormatName = this.copyToConfig(inputFormatName);
				}
				URL outputFormatURL = f.findURLinJars(outputFormatName);
				if (inputFormatURL != null) {
					outputFormatName = this.copyToConfig(outputFormatURL);
				} else {
					outputFormatName = this.copyToConfig(outputFormatName);
				}
			}
			OptionManager.instance().overloadOptionValue(containerIndex, "input", "format", inputFormatName);
		} else {
			if (outputFormatName.length() == 0) {
				outputFormatName = inputFormatName;
			}
		}
		dataFormatInstances = new HashMap<String, DataFormatInstance>(3);
		inputFormatURL = findURL(inputFormatName);
		outputFormatURL = findURL(outputFormatName);
		if (outputFormatURL != null) {
			try {
				InputStream is = outputFormatURL.openStream();
			} catch (FileNotFoundException e) {
				outputFormatURL = f.findURL(outputFormatName);
			} catch (IOException e) {
				outputFormatURL = f.findURL(outputFormatName);
			}
		} else {
			outputFormatURL = f.findURL(outputFormatName);
		}
		dataFormatManager = new DataFormatManager(inputFormatURL, outputFormatURL);
		
		String mode = OptionManager.instance().getOptionValue(containerIndex, "config", "flowchart").toString().trim();
		if (mode.equals("parse")) {
			symbolTables = new TrieSymbolTableHandler(TrieSymbolTableHandler.ADD_NEW_TO_TMP_STORAGE);
//			symbolTables = new TrieSymbolTableHandler(TrieSymbolTableHandler.ADD_NEW_TO_TRIE);
		} else {
			symbolTables = new TrieSymbolTableHandler(TrieSymbolTableHandler.ADD_NEW_TO_TRIE);
		}
		if (dataFormatManager.getInputDataFormatSpec().getDataStructure() == DataStructure.PHRASE) {
			if (mode.equals("learn")) {
				Set<Dependency> deps = dataFormatManager.getInputDataFormatSpec().getDependencies();
				for (Dependency dep : deps) {
					URL depFormatURL = f.findURLinJars(dep.getUrlString());
					if (depFormatURL != null) {
						this.copyToConfig(depFormatURL);
					} else {
						this.copyToConfig(dep.getUrlString());
					}
				}
			} 
			else if (mode.equals("parse")) {
				Set<Dependency> deps = dataFormatManager.getInputDataFormatSpec().getDependencies();
				String nullValueStategy = OptionManager.instance().getOptionValue(containerIndex, "singlemalt", "null_value").toString();
				for (Dependency dep : deps) {
//					URL depFormatURL = f.findURLinJars(dep.getUrlString());
					DataFormatInstance dataFormatInstance = dataFormatManager.getDataFormatSpec(dep.getDependentOn()).createDataFormatInstance(symbolTables, nullValueStategy);
					addDataFormatInstance(dataFormatManager.getDataFormatSpec(dep.getDependentOn()).getDataFormatName(), dataFormatInstance);
					dataFormatManager.setInputDataFormatSpec(dataFormatManager.getDataFormatSpec(dep.getDependentOn()));
//					dataFormatManager.setOutputDataFormatSpec(dataFormatManager.getDataFormatSpec(dep.getDependentOn()));
				}
			}
		}
	}
	
	private URL findURL(String specModelFileName) throws MaltChainedException {
		URL url = null;
		File specFile = this.getFile(specModelFileName);
		if (specFile.exists()) {
			try {
				url = new URL("file:///"+specFile.getAbsolutePath());
			} catch (MalformedURLException e) {
				throw new MaltChainedException("Malformed URL: "+specFile, e);
			}
		} else {
			url = this.getConfigFileEntryURL(specModelFileName);
		}
		return url;
	}
	
	/**
	 * Creates an output stream writer, where the corresponding file will be included in the configuration directory
	 * 
	 * @param fileName	a file name
	 * @param charSet	a char set
	 * @return	an output stream writer for writing to a file within the configuration directory
	 * @throws MaltChainedException
	 */
	public OutputStreamWriter getOutputStreamWriter(String fileName, String charSet) throws MaltChainedException {
		try {
			return new OutputStreamWriter(new FileOutputStream(configDirectory.getPath()+File.separator+fileName), charSet);
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("The file '"+fileName+"' cannot be created. ", e);
		} catch (UnsupportedEncodingException e) {
			throw new ConfigurationException("The char set '"+charSet+"' is not supported. ", e);
		}
	}
	
	/**
	 * Creates an output stream writer, where the corresponding file will be included in the 
	 * configuration directory. Uses UTF-8 for character encoding.
	 * 
	 * @param fileName	a file name
	 * @return an output stream writer for writing to a file within the configuration directory
	 * @throws MaltChainedException
	 */
	public OutputStreamWriter getOutputStreamWriter(String fileName) throws MaltChainedException {
		try {
			return new OutputStreamWriter(new FileOutputStream(configDirectory.getPath()+File.separator+fileName, true), "UTF-8");
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("The file '"+fileName+"' cannot be created. ", e);
		} catch (UnsupportedEncodingException e) {
			throw new ConfigurationException("The char set 'UTF-8' is not supported. ", e);
		}
	}
	/**
	 * This method acts the same as getOutputStreamWriter with the difference that the writer append in the file
	 * if it already exists instead of deleting the previous content before starting to write.
	 * 
	 * @param fileName	a file name
	 * @return an output stream writer for writing to a file within the configuration directory
	 * @throws MaltChainedException
	 */
	public OutputStreamWriter getAppendOutputStreamWriter(String fileName) throws MaltChainedException {
		try {
			return new OutputStreamWriter(new FileOutputStream(configDirectory.getPath()+File.separator+fileName, true), "UTF-8");
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("The file '"+fileName+"' cannot be created. ", e);
		} catch (UnsupportedEncodingException e) {
			throw new ConfigurationException("The char set 'UTF-8' is not supported. ", e);
		}
	}
	
	/**
	 * Creates an input stream reader for reading a file within the configuration directory
	 * 
	 * @param fileName	a file name
	 * @param charSet	a char set
	 * @return an input stream reader for reading a file within the configuration directory
	 * @throws MaltChainedException
	 */
	public InputStreamReader getInputStreamReader(String fileName, String charSet) throws MaltChainedException {
		try {
			return new InputStreamReader(new FileInputStream(configDirectory.getPath()+File.separator+fileName), charSet);
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("The file '"+fileName+"' cannot be found. ", e);
		} catch (UnsupportedEncodingException e) {
			throw new ConfigurationException("The char set '"+charSet+"' is not supported. ", e);
		}
	}
	
	/**
	 * Creates an input stream reader for reading a file within the configuration directory.
	 * Uses UTF-8 for character encoding.
	 * 
	 * @param fileName	a file name
	 * @return	an input stream reader for reading a file within the configuration directory
	 * @throws MaltChainedException
	 */
	public InputStreamReader getInputStreamReader(String fileName) throws MaltChainedException {
		return getInputStreamReader(fileName, "UTF-8");
	}
	
	public JarEntry getConfigFileEntry(String fileName) throws MaltChainedException {
		File mcoPath = new File(workingDirectory.getPath()+File.separator+getName()+".mco");
		try {
			JarFile mcoFile = new JarFile(mcoPath.getAbsolutePath());
			JarEntry entry = mcoFile.getJarEntry(getName()+'/'+fileName);
			if (entry == null) {
				entry = mcoFile.getJarEntry(getName()+'\\'+fileName);
			}
			return entry;
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("The file entry '"+fileName+"' in mco-file '"+mcoPath+"' cannot be found. ", e);
		} catch (IOException e) {
			throw new ConfigurationException("The file entry '"+fileName+"' in mco-file '"+mcoPath+"' cannot be found. ", e);
		}
	}
	
	public InputStream getInputStreamFromConfigFileEntry(String fileName) throws MaltChainedException {
		if	(!url.toString().startsWith("jar")) { 
			// New solution 
			try {
				JarURLConnection conn = (JarURLConnection)new URL("jar:" + url.toString() + "!/").openConnection();
				JarFile mcoFile = conn.getJarFile();
				JarEntry entry = mcoFile.getJarEntry(getName()+'/'+fileName);
				if (entry == null) {
					entry = mcoFile.getJarEntry(getName()+'\\'+fileName);
				}
				if (entry == null) {
					throw new FileNotFoundException();
				}
				return mcoFile.getInputStream(entry);

			} catch (IOException e) {
				throw new ConfigurationException("The file entry '"+fileName+"' in the mco file '"+url+"' cannot be loaded. ", e);
			}
		} else {
			// Old solution: Can load files from the mco-file within a jar-file
            File mcoPath = new File(workingDirectory.getPath()+File.separator+getName()+".mco");
            try {
                    JarFile mcoFile = new JarFile(mcoPath.getAbsolutePath());
                    JarEntry entry = mcoFile.getJarEntry(getName()+'/'+fileName);
                    if (entry == null) {
                            entry = mcoFile.getJarEntry(getName()+'\\'+fileName);
                    }
                    if (entry == null) {
                            throw new FileNotFoundException();
                    }
                    return mcoFile.getInputStream(entry);
            } catch (FileNotFoundException e) {
                    throw new ConfigurationException("The file entry '"+fileName+"' in the mco file '"+mcoPath+"' cannot be found. ", e);
            } catch (IOException e) {
                    throw new ConfigurationException("The file entry '"+fileName+"' in the mco file '"+mcoPath+"' cannot be loaded. ", e);
            }
		}
	}
	
	public InputStreamReader getInputStreamReaderFromConfigFileEntry(String fileName, String charSet) throws MaltChainedException {
		if	(!url.toString().startsWith("jar")) { 
			// New solution 
			try {
				JarURLConnection conn = (JarURLConnection)new URL("jar:" + url.toString() + "!/").openConnection();

				JarFile mcoFile = null;
				mcoFile = conn.getJarFile();
				JarEntry entry = mcoFile.getJarEntry(getName() + '/' + fileName);
	
				if (entry == null) {
					entry = mcoFile.getJarEntry(getName() + '\\' + fileName);
				}
				if (entry == null) {
					throw new FileNotFoundException();
				}
				return new InputStreamReader(mcoFile.getInputStream(entry), charSet);
			} catch (UnsupportedEncodingException e) {
				throw new ConfigurationException("The char set '"+charSet+"' is not supported. ", e);
			} catch (IOException e) {
				throw new ConfigurationException("The entry '"+fileName+"' in the mco url '"+this.url+"' cannot be loaded. ", e);
			}
		} else {
			// Old solution: Can load files from the mco-file within a jar-file
			File mcoPath = new File(workingDirectory.getPath()+File.separator+getName()+".mco");
			try {
		        JarFile mcoFile = new JarFile(mcoPath.getAbsolutePath());
	            JarEntry entry = mcoFile.getJarEntry(getName()+'/'+fileName);
	            if (entry == null) {
	                    entry = mcoFile.getJarEntry(getName()+'\\'+fileName);
	            }
		        if (entry == null) {
		        	throw new FileNotFoundException();
		        }
		        return new InputStreamReader(mcoFile.getInputStream(entry),  charSet);
	        } catch (FileNotFoundException e) {
	                throw new ConfigurationException("The file entry '"+fileName+"' in the mco file '"+mcoPath+"' cannot be found. ", e);
	        } catch (UnsupportedEncodingException e) {
	                throw new ConfigurationException("The char set '"+charSet+"' is not supported. ", e);
	        } catch (IOException e) {
	                throw new ConfigurationException("The file entry '"+fileName+"' in the mco file '"+mcoPath+"' cannot be loaded. ", e);
	        }
		}
	}
	
	public InputStreamReader getInputStreamReaderFromConfigFile(String fileName) throws MaltChainedException {
		return getInputStreamReaderFromConfigFileEntry(fileName, "UTF-8");
	}
	
	/**
	 * Returns a file handler object of a file within the configuration directory
	 * 
	 * @param fileName	a file name
	 * @return	a file handler object of a file within the configuration directory
	 * @throws MaltChainedException
	 */
	public File getFile(String fileName) throws MaltChainedException {
		return new File(configDirectory.getPath()+File.separator+fileName);
	}
	
	public URL getConfigFileEntryURL(String fileName) throws MaltChainedException {
		if	(!url.toString().startsWith("jar")) { 
			// New solution 
			try {
				URL url = new URL("jar:"+this.url.toString()+"!/"+getName()+'/'+fileName + "\n");
				try { 
					InputStream is = url.openStream();
					is.close();
				} catch (IOException e) {
					url = new URL("jar:"+this.url.toString()+"!/"+getName()+'\\'+fileName + "\n");
				}
				return url;
			} catch (MalformedURLException e) {
				throw new ConfigurationException("Couldn't find the URL '" +"jar:"+this.url.toString()+"!/"+getName()+'/'+fileName+ "'", e);
			}
		} else {
			// Old solution: Can load files from the mco-file within a jar-file
	        File mcoPath = new File(workingDirectory.getPath()+File.separator+getName()+".mco");
	        try {
	                if (!mcoPath.exists()) {
	                        throw new ConfigurationException("Couldn't find mco-file '" +mcoPath.getAbsolutePath()+ "'");
	                }                                                                                                                                   
	                URL url = new URL("jar:"+new URL("file", null, mcoPath.getAbsolutePath())+"!/"+getName()+'/'+fileName + "\n");
	                try {
	                        InputStream is = url.openStream();
	                        is.close();
	                } catch (IOException e) {
	                        url = new URL("jar:"+new URL("file", null, mcoPath.getAbsolutePath())+"!/"+getName()+'\\'+fileName + "\n");
	                }
	                return url;
	        } catch (MalformedURLException e) {
	                throw new ConfigurationException("Couldn't find the URL '" +"jar:"+mcoPath.getAbsolutePath()+"!/"+getName()+'/'+fileName+ "'", e);
	        }
		}
	}
	
    /**
     * Copies a file into the configuration directory.
     * 
     * @param source	a path to file 
     * @throws MaltChainedException
     */
    public String copyToConfig(File source) throws MaltChainedException {
    	byte[] readBuffer = new byte[BUFFER];
    	String destination = configDirectory.getPath()+File.separator+source.getName();
    	try {
	    	BufferedInputStream bis = new BufferedInputStream(new FileInputStream(source));
	        BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(destination), BUFFER);
	    
	        int n = 0;
		    while ((n = bis.read(readBuffer, 0, BUFFER)) != -1) {
		    	bos.write(readBuffer, 0, n);
		    }
	        bos.flush();
	        bos.close();
	        bis.close();
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("The source file '"+source+"' cannot be found or the destination file '"+destination+"' cannot be created when coping the file. ", e);
		} catch (IOException e) {
			throw new ConfigurationException("The source file '"+source+"' cannot be copied to destination '"+destination+"'. ", e);
		}
		return source.getName();
    }
    

    public String copyToConfig(String fileUrl) throws MaltChainedException {
    	final URLFinder f = new URLFinder();
    	URL url = f.findURL(fileUrl);
    	if (url == null) {
    		throw new ConfigurationException("The file or URL '"+fileUrl+"' could not be found. ");
    	}
    	return copyToConfig(url);
    }
    
    public String copyToConfig(URL url) throws MaltChainedException {
    	if (url == null) {
    		throw new ConfigurationException("URL could not be found. ");
    	}
    	byte[] readBuffer = new byte[BUFFER];
    	String destFileName = url.getPath();
		int indexSlash = destFileName.lastIndexOf('/');
		if (indexSlash == -1) {
			indexSlash = destFileName.lastIndexOf('\\');
		}
    	
		if (indexSlash != -1) {
			destFileName = destFileName.substring(indexSlash+1);
		}
    	
    	String destination = configDirectory.getPath()+File.separator+destFileName;
    	try {
	    	BufferedInputStream bis = new BufferedInputStream(url.openStream());
	        BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(destination), BUFFER);
	    
	        int n = 0;
		    while ((n = bis.read(readBuffer, 0, BUFFER)) != -1) {
		    	bos.write(readBuffer, 0, n);
		    }
	        bos.flush();
	        bos.close();
	        bis.close();
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("The destination file '"+destination+"' cannot be created when coping the file. ", e);
		} catch (IOException e) {
			throw new ConfigurationException("The URL '"+url+"' cannot be copied to destination '"+destination+"'. ", e);
		}
		return destFileName;
    }
 
    
	/**
	 * Removes the configuration directory, if it exists and it contains a .info file. 
	 * 
	 * @throws MaltChainedException
	 */
	public void deleteConfigDirectory() throws MaltChainedException {
		if (!configDirectory.exists()) {
			return;
		}
		File infoFile = new File(configDirectory.getPath()+File.separator+getName()+"_"+getType()+".info");
		if (infoFile.exists()) {
			deleteConfigDirectory(configDirectory);
		} else {
			throw new ConfigurationException("There exists a directory that is not a MaltParser configuration directory. ");
		}
	}
	
	private void deleteConfigDirectory(File directory) throws MaltChainedException {
		if (directory.exists()) {
			File[] files = directory.listFiles();
			for (int i = 0; i < files.length; i++) {
				if (files[i].isDirectory()) {
					deleteConfigDirectory(files[i]);
				} else {
					files[i].delete();
				}
			}
		} else {
			throw new ConfigurationException("The directory '"+directory.getPath()+ "' cannot be found. ");
		}
		directory.delete();
	}
	
	/**
	 * Returns a file handler object for the configuration directory
	 * 
	 * @return a file handler object for the configuration directory
	 */
	public File getConfigDirectory() {
		return configDirectory;
	}

	protected void setConfigDirectory(File dir) {
		this.configDirectory = dir;
	}

	/**
	 * Creates the configuration directory
	 * 
	 * @throws MaltChainedException
	 */
	public void createConfigDirectory() throws MaltChainedException {
		checkConfigDirectory();
		configDirectory.mkdir();
		createInfoFile();
	}
	
	protected void checkConfigDirectory()  throws MaltChainedException {
		if (configDirectory.exists() && !configDirectory.isDirectory()) {
			throw new ConfigurationException("The configuration directory name already exists and is not a directory. ");
		}
		
		if (configDirectory.exists()) {
			deleteConfigDirectory();
		} 
	}
	
	protected void createInfoFile() throws MaltChainedException {
		infoFile = new BufferedWriter(getOutputStreamWriter(getName()+"_"+getType()+".info"));
		try {
			infoFile.write("CONFIGURATION\n");
			infoFile.write("Configuration name:   "+getName()+"\n");
			infoFile.write("Configuration type:   "+getType()+"\n");
			infoFile.write("Created:              "+new Date(System.currentTimeMillis())+"\n");
			
			infoFile.write("\nSYSTEM\n");
			infoFile.write("Operating system architecture: "+System.getProperty("os.arch")+"\n");
			infoFile.write("Operating system name:         "+System.getProperty("os.name")+"\n");
			infoFile.write("JRE vendor name:               "+System.getProperty("java.vendor")+"\n");
			infoFile.write("JRE version number:            "+System.getProperty("java.version")+"\n");
			
			infoFile.write("\nMALTPARSER\n");
			infoFile.write("Version:                       "+SystemInfo.getVersion()+"\n");
			infoFile.write("Build date:                    "+SystemInfo.getBuildDate()+"\n");
			Set<String> excludeGroups = new HashSet<String>();
			excludeGroups.add("system");
			infoFile.write("\nSETTINGS\n");
			infoFile.write(OptionManager.instance().toStringPrettyValues(containerIndex, excludeGroups));
			infoFile.flush();
		} catch (IOException e) {
			throw new ConfigurationException("Could not create the maltparser info file. ");
		}
	}
	
	/**
	 * Returns a writer to the configuration information file
	 * 
	 * @return	a writer to the configuration information file
	 * @throws MaltChainedException
	 */
	public BufferedWriter getInfoFileWriter() throws MaltChainedException {
		return infoFile;
	}
	
	/**
	 * Creates the malt configuration file (.mco). This file is compressed.   
	 * 
	 * @throws MaltChainedException
	 */
	public void createConfigFile() throws MaltChainedException {
		try {
			JarOutputStream jos = new JarOutputStream(new FileOutputStream(workingDirectory.getPath()+File.separator+getName()+".mco"));
//			configLogger.info("Creates configuration file '"+workingDirectory.getPath()+File.separator+getName()+".mco' ...\n");
			createConfigFile(configDirectory.getPath(), jos);
			jos.close();
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("The maltparser configurtation file '"+workingDirectory.getPath()+File.separator+getName()+".mco"+"' cannot be found. ", e);
		} catch (IOException e) {
			throw new ConfigurationException("The maltparser configurtation file '"+workingDirectory.getPath()+File.separator+getName()+".mco"+"' cannot be created. ", e);
		} 
	}

	private void createConfigFile(String directory, JarOutputStream jos) throws MaltChainedException {
    	byte[] readBuffer = new byte[BUFFER];
		try {
			File zipDir = new File(directory);
			String[] dirList = zipDir.list();
			
			int bytesIn = 0;
	
			for (int i = 0; i < dirList.length; i++) {
				File f = new File(zipDir, dirList[i]);
				if (f.isDirectory()) {
					String filePath = f.getPath();
					createConfigFile(filePath, jos);
					continue;
				}
	
				FileInputStream fis = new FileInputStream(f);
				
				String entryPath = f.getPath().substring(workingDirectory.getPath().length()+1);
				entryPath = entryPath.replace('\\', '/');
				JarEntry entry = new JarEntry(entryPath);
				jos.putNextEntry(entry);
	
				while ((bytesIn = fis.read(readBuffer)) != -1) {
					jos.write(readBuffer, 0, bytesIn);
				}
	
				fis.close();
			}
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("The directory '"+directory+"' cannot be found. ", e);
		} catch (IOException e) {
			throw new ConfigurationException("The directory '"+directory+"' cannot be compressed into a mco file. ", e);
		} 
	}
	

	public void copyConfigFile(File in, File out, Versioning versioning) throws MaltChainedException {
		try {
			JarFile jar = new JarFile(in);
			JarOutputStream tempJar = new JarOutputStream(new FileOutputStream(out));
	        byte[] buffer = new byte[BUFFER];
	        int bytesRead;
	        final StringBuilder sb = new StringBuilder();
	        final URLFinder f = new URLFinder();

	        for (Enumeration<JarEntry> entries = jar.entries(); entries.hasMoreElements(); ) {
	            JarEntry inEntry = (JarEntry) entries.nextElement();
	            InputStream entryStream = jar.getInputStream(inEntry);
	            JarEntry outEntry = versioning.getJarEntry(inEntry);
	            
	            if (!versioning.hasChanges(inEntry, outEntry)) {
		            tempJar.putNextEntry(outEntry);
		            while ((bytesRead = entryStream.read(buffer)) != -1) {
		            	tempJar.write(buffer, 0, bytesRead);
		            }
	            } else {
	            	tempJar.putNextEntry(outEntry);
	            	BufferedReader br = new BufferedReader(new InputStreamReader(entryStream));
	            	String line = null;
	            	sb.setLength(0);
	            	while ((line = br.readLine()) != null) {
	            		sb.append(line);
	            		sb.append('\n');
	            	}
	            	String outString = versioning.modifyJarEntry(inEntry, outEntry, sb);
	            	tempJar.write(outString.getBytes());
	            }
	        }
	        if (versioning.getFeatureModelXML() != null && versioning.getFeatureModelXML().startsWith("/appdata")) {
	        	int index = versioning.getFeatureModelXML().lastIndexOf('/');
    	    	BufferedInputStream bis = new BufferedInputStream(f.findURLinJars(versioning.getFeatureModelXML()).openStream());
    	    	tempJar.putNextEntry(new JarEntry(versioning.getNewConfigName()+"/" +versioning.getFeatureModelXML().substring(index+1)));
    	        int n = 0;
    		    while ((n = bis.read(buffer, 0, BUFFER)) != -1) {
    		    	tempJar.write(buffer, 0, n);
    		    }
    	        bis.close();
	        }
	        if (versioning.getInputFormatXML() != null && versioning.getInputFormatXML().startsWith("/appdata")) {
	        	int index = versioning.getInputFormatXML().lastIndexOf('/');
    	    	BufferedInputStream bis = new BufferedInputStream(f.findURLinJars(versioning.getInputFormatXML()).openStream());
    	    	tempJar.putNextEntry(new JarEntry(versioning.getNewConfigName()+"/" +versioning.getInputFormatXML().substring(index+1)));
    	        int n = 0;
    		    while ((n = bis.read(buffer, 0, BUFFER)) != -1) {
    		    	tempJar.write(buffer, 0, n);
    		    }
    	        bis.close();
	        }
	        tempJar.flush();
	        tempJar.close();
	        jar.close();
		} catch (IOException e) {
			throw new ConfigurationException("", e);
		}
	}
	
    protected void initNameNTypeFromInfoFile(URL url) throws MaltChainedException {
		if (url == null) {
			throw new ConfigurationException("The URL cannot be found. ");
		}  	
		try {
			JarEntry je;
			JarInputStream jis = new JarInputStream(url.openConnection().getInputStream());
			while ((je = jis.getNextJarEntry()) != null) {
				String entryName = je.getName();
				if (entryName.endsWith(".info")) {
					int indexUnderScore = entryName.lastIndexOf('_');
					int indexSeparator = entryName.lastIndexOf(File.separator);
					if (indexSeparator == -1) {
						indexSeparator = entryName.lastIndexOf('/');
					}
					if (indexSeparator == -1) {
						indexSeparator = entryName.lastIndexOf('\\');
					}
					int indexDot = entryName.lastIndexOf('.');
					if (indexUnderScore == -1 || indexDot == -1) {
						throw new ConfigurationException("Could not find the configuration name and type from the URL '"+url.toString()+"'. ");
					}
					setName(entryName.substring(indexSeparator+1, indexUnderScore));
					setType(entryName.substring(indexUnderScore+1, indexDot));
					setConfigDirectory(new File(workingDirectory.getPath()+File.separator+getName()));
					jis.close();
					return;
				}
			}
			
		} catch (IOException e) {
			throw new ConfigurationException("Could not find the configuration name and type from the URL '"+url.toString()+"'. ", e);
		}
    }
    
    /**
     * Prints the content of the configuration information file to the system logger
     * 
     * @throws MaltChainedException
     */
    public void echoInfoFile() throws MaltChainedException {
    	checkConfigDirectory();
    	JarInputStream jis;
    	try {
	    	if (url == null) {
	    		jis = new JarInputStream(new FileInputStream(workingDirectory.getPath()+File.separator+getName()+".mco"));
	    	} else {
	    		jis = new JarInputStream(url.openConnection().getInputStream());
	    	}
			JarEntry je;

			while ((je = jis.getNextJarEntry()) != null) {
		        String entryName = je.getName();

		        if (entryName.endsWith(getName()+"_"+getType()+".info")) {
		        	int c;
				    while ((c = jis.read()) != -1) {
				    	SystemLogger.logger().info((char)c);
				    }	
		        }
			}
	    	jis.close();
    	} catch (FileNotFoundException e) {
    		throw new ConfigurationException("Could not print configuration information file. The configuration file '"+workingDirectory.getPath()+File.separator+getName()+".mco"+"' cannot be found. ", e);
    	} catch (IOException e) {
			throw new ConfigurationException("Could not print configuration information file. ", e);
		}

    }
    
    /**
     * Unpacks the malt configuration file (.mco).
     * 
     * @throws MaltChainedException
     */
    public void unpackConfigFile() throws MaltChainedException {
    	checkConfigDirectory();
    	JarInputStream jis;
    	try {
	    	if (url == null) {
	    		jis = new JarInputStream(new FileInputStream(workingDirectory.getPath()+File.separator+getName()+".mco"));
	    	} else {
	    		jis = new JarInputStream(url.openConnection().getInputStream());
	    	}
	    	unpackConfigFile(jis);
	    	jis.close();
    	} catch (FileNotFoundException e) {
    		throw new ConfigurationException("Could not unpack configuration. The configuration file '"+workingDirectory.getPath()+File.separator+getName()+".mco"+"' cannot be found. ", e);
    	} catch (IOException e) {
    		if (configDirectory.exists()) {
    			deleteConfigDirectory();
    		}
			throw new ConfigurationException("Could not unpack configuration. ", e);
		}
    	initCreatedByMaltParserVersionFromInfoFile();
    }

    protected void unpackConfigFile(JarInputStream jis) throws MaltChainedException {
		try {
			JarEntry je;
			byte[] readBuffer = new byte[BUFFER];
	    	SortedSet<String> directoryCache  = new TreeSet<String>();
			while ((je = jis.getNextJarEntry()) != null) {
		        String entryName = je.getName();

		        if (entryName.startsWith("/")) {
		        	entryName = entryName.substring(1);
		        }
		        if (entryName.endsWith(File.separator) || entryName.endsWith("/")) {
		            return;
		        }
		        int index = -1;
	        	if (File.separator.equals("\\")) {
	        		entryName = entryName.replace('/', '\\');
	        		index = entryName.lastIndexOf("\\");
	        	} else if (File.separator.equals("/")) {
	        		entryName = entryName.replace('\\', '/');
	        		index = entryName.lastIndexOf("/");
	        	}
		        if (index > 0) {
		            String dirName = entryName.substring(0, index);
		            if (!directoryCache.contains(dirName)) {
		                File directory = new File(workingDirectory.getPath()+File.separator+dirName);
		                if (!(directory.exists() && directory.isDirectory())) {
		                    if (!directory.mkdirs()) {
		                    	throw new ConfigurationException("Unable to make directory '" + dirName +"'. ");
		                    }
		                    directoryCache.add(dirName);
		                }
		            }
		        }
		       
		        if (new File(workingDirectory.getPath()+File.separator+entryName).isDirectory() && new File(workingDirectory.getPath()+File.separator+entryName).exists()) {
		        	continue;
		        }
		        BufferedOutputStream bos;
		        try {
		        	bos = new BufferedOutputStream(new FileOutputStream(workingDirectory.getPath()+File.separator+entryName), BUFFER);
		    	} catch (FileNotFoundException e) {
					throw new ConfigurationException("Could not unpack configuration. The file '"+workingDirectory.getPath()+File.separator+entryName+"' cannot be unpacked. ", e);
		    	}
				int n = 0;
			    while ((n = jis.read(readBuffer, 0, BUFFER)) != -1) {
			    	bos.write(readBuffer, 0, n);
			    }
			    bos.flush();
			    bos.close();
			}
		} catch (IOException e) {
			throw new ConfigurationException("Could not unpack configuration. ", e);
		}
    }
	    
	/**
	 * Returns the name of the configuration directory
	 * 
	 * @return the name of the configuration directory
	 */
	public String getName() {
		return name;
	}

	protected void setName(String name) {
		this.name = name;
	}

	/**
	 * Returns the type of the configuration directory
	 * 
	 * @return the type of the configuration directory
	 */
	public String getType() {
		return type;
	}

	protected void setType(String type) {
		this.type = type;
	}
	
	/**
	 * Returns a file handler object for the working directory
	 * 
	 * @return a file handler object for the working directory
	 */
	public File getWorkingDirectory() {
		return workingDirectory;
	}

	/**
	 * Initialize the working directory
	 * 
	 * @throws MaltChainedException
	 */
	public void initWorkingDirectory() throws MaltChainedException {
		try {
			initWorkingDirectory(OptionManager.instance().getOptionValue(containerIndex, "config", "workingdir").toString());
		} catch (NullPointerException e) {
			throw new ConfigurationException("The configuration cannot be found.", e);
		}
	}

	/**
	 * Initialize the working directory according to the path. If the path is equals to "user.dir" or current directory, then the current directory
	 *  will be the working directory.
	 * 
	 * @param pathPrefixString	the path to the working directory
	 * @throws MaltChainedException
	 */
	public void initWorkingDirectory(String pathPrefixString) throws MaltChainedException {
		if (pathPrefixString == null || pathPrefixString.equalsIgnoreCase("user.dir") || pathPrefixString.equalsIgnoreCase(".")) {
			workingDirectory = new File(System.getProperty("user.dir"));
		} else {
			workingDirectory = new File(pathPrefixString);
		}

		if (workingDirectory == null || !workingDirectory.isDirectory()) {
			new ConfigurationException("The specified working directory '"+pathPrefixString+"' is not a directory. ");
		}
	}
	
	/**
	 * Returns the URL to the malt configuration file (.mco) 
	 * 
	 * @return the URL to the malt configuration file (.mco)
	 */
	public URL getUrl() {
		return url;
	}

	protected void setUrl(URL url) {
		this.url = url;
	}
	
	/**
	 * Returns the option container index
	 * 
	 * @return the option container index
	 */
	public int getContainerIndex() {
		return containerIndex;
	}

	/**
	 * Sets the option container index
	 * 
	 * @param containerIndex a option container index
	 */
	public void setContainerIndex(int containerIndex) {
		this.containerIndex = containerIndex;
	}

	/**
	 * Returns the version number of MaltParser which created the malt configuration file (.mco)
	 * 
	 * @return the version number of MaltParser which created the malt configuration file (.mco)
	 */
	public String getCreatedByMaltParserVersion() {
		return createdByMaltParserVersion;
	}

	/**
	 * Sets the version number of MaltParser which created the malt configuration file (.mco)
	 * 
	 * @param createdByMaltParserVersion a version number of MaltParser
	 */
	public void setCreatedByMaltParserVersion(String createdByMaltParserVersion) {
		this.createdByMaltParserVersion = createdByMaltParserVersion;
	}
	
	public void initCreatedByMaltParserVersionFromInfoFile() throws MaltChainedException {
		try {
			BufferedReader br = new BufferedReader(getInputStreamReaderFromConfigFileEntry(getName()+"_"+getType()+".info", "UTF-8"));
			String line = null;
			while ((line = br.readLine()) != null) {
				if (line.startsWith("Version:                       ")) {
					setCreatedByMaltParserVersion(line.substring(31));
					break;
				}
			}
			br.close();
		} catch (FileNotFoundException e) {
			throw new ConfigurationException("Could not retrieve the version number of the MaltParser configuration.", e);
		} catch (IOException e) {
			throw new ConfigurationException("Could not retrieve the version number of the MaltParser configuration.", e);
		}
	}
	
	public void versioning() throws MaltChainedException {
		initCreatedByMaltParserVersionFromInfoFile();
		SystemLogger.logger().info("\nCurrent version      : " + SystemInfo.getVersion() + "\n");
		SystemLogger.logger().info("Parser model version : " + createdByMaltParserVersion + "\n");
		if (SystemInfo.getVersion() == null) {
			throw new ConfigurationException("Couln't determine the version of MaltParser");
		} else if (createdByMaltParserVersion == null) {
			throw new ConfigurationException("Couln't determine the version of the parser model");
		} else if (SystemInfo.getVersion().equals(createdByMaltParserVersion)) {
			SystemLogger.logger().info("The parser model "+getName()+".mco has already the same version as the current version of MaltParser. \n");
			return;
		}
		
		File mcoPath = new File(workingDirectory.getPath()+File.separator+getName()+".mco");
		File newMcoPath = new File(workingDirectory.getPath()+File.separator+getName()+"."+SystemInfo.getVersion().trim()+".mco");
		Versioning versioning = new Versioning(name, type, mcoPath, createdByMaltParserVersion);
		if (!versioning.support(createdByMaltParserVersion)) {
			SystemLogger.logger().warn("The parser model '"+ name+ ".mco' is created by MaltParser "+getCreatedByMaltParserVersion()+", which cannot be converted to a MaltParser "+SystemInfo.getVersion()+" parser model.\n");
			SystemLogger.logger().warn("Please retrain the parser model with MaltParser "+SystemInfo.getVersion() +" or download MaltParser "+getCreatedByMaltParserVersion()+" from http://maltparser.org/download.html\n");
			return;
		}
		SystemLogger.logger().info("Converts the parser model '"+ mcoPath.getName()+ "' into '"+newMcoPath.getName()+"'....\n");
		copyConfigFile(mcoPath, newMcoPath, versioning);
	}
	
	protected void checkNConvertConfigVersion() throws MaltChainedException {
		if (createdByMaltParserVersion.startsWith("1.0")) {
			SystemLogger.logger().info("  Converts the MaltParser configuration ");
			SystemLogger.logger().info("1.0");
			SystemLogger.logger().info(" to ");
			SystemLogger.logger().info(SystemInfo.getVersion());
			SystemLogger.logger().info("\n");
			File[] configFiles = configDirectory.listFiles();
			for (int i = 0, n = configFiles.length; i < n; i++) {
				if (configFiles[i].getName().endsWith(".mod")) {
					configFiles[i].renameTo(new File(configDirectory.getPath()+File.separator+"odm0."+configFiles[i].getName()));
				}
				if (configFiles[i].getName().endsWith(getName()+".dsm")) {
					configFiles[i].renameTo(new File(configDirectory.getPath()+File.separator+"odm0.dsm"));
				}
				if (configFiles[i].getName().equals("savedoptions.sop")) {
					configFiles[i].renameTo(new File(configDirectory.getPath()+File.separator+"savedoptions.sop.old"));
				}
				if (configFiles[i].getName().equals("symboltables.sym")) {
					configFiles[i].renameTo(new File(configDirectory.getPath()+File.separator+"symboltables.sym.old"));
				}
			}
			try {
				BufferedReader br = new BufferedReader(new FileReader(configDirectory.getPath()+File.separator+"savedoptions.sop.old"));
				BufferedWriter bw = new BufferedWriter(new FileWriter(configDirectory.getPath()+File.separator+"savedoptions.sop"));
				String line;
				while ((line = br.readLine()) != null) {
					if (line.startsWith("0\tguide\tprediction_strategy")) {
						bw.write("0\tguide\tdecision_settings\tT.TRANS+A.DEPREL\n");
					} else {
						bw.write(line);
						bw.write('\n');
					}
				}
				br.close();
				bw.flush();
				bw.close();
				new File(configDirectory.getPath()+File.separator+"savedoptions.sop.old").delete();
			} catch (FileNotFoundException e) {
				throw new ConfigurationException("Could convert savedoptions.sop version 1.0.4 to version 1.1. ", e);
			}  catch (IOException e) {
				throw new ConfigurationException("Could convert savedoptions.sop version 1.0.4 to version 1.1. ", e);
			}		
			try {
				BufferedReader br = new BufferedReader(new FileReader(configDirectory.getPath()+File.separator+"symboltables.sym.old"));
				BufferedWriter bw = new BufferedWriter(new FileWriter(configDirectory.getPath()+File.separator+"symboltables.sym"));
				String line;
				while ((line = br.readLine()) != null) {
					if (line.startsWith("AllCombinedClassTable")) {
						bw.write("T.TRANS+A.DEPREL\n");
					} else {
						bw.write(line);
						bw.write('\n');
					}
				}
				br.close();
				bw.flush();
				bw.close();
				new File(configDirectory.getPath()+File.separator+"symboltables.sym.old").delete();
			} catch (FileNotFoundException e) {
				throw new ConfigurationException("Could convert symboltables.sym version 1.0.4 to version 1.1. ", e);
			}  catch (IOException e) {
				throw new ConfigurationException("Could convert symboltables.sym version 1.0.4 to version 1.1. ", e);
			}
		}
		if (!createdByMaltParserVersion.startsWith("1.3")) {
			SystemLogger.logger().info("  Converts the MaltParser configuration ");
			SystemLogger.logger().info(createdByMaltParserVersion);
			SystemLogger.logger().info(" to ");
			SystemLogger.logger().info(SystemInfo.getVersion());
			SystemLogger.logger().info("\n");
			

			new File(configDirectory.getPath()+File.separator+"savedoptions.sop").renameTo(new File(configDirectory.getPath()+File.separator+"savedoptions.sop.old"));
			try {
				BufferedReader br = new BufferedReader(new FileReader(configDirectory.getPath()+File.separator+"savedoptions.sop.old"));
				BufferedWriter bw = new BufferedWriter(new FileWriter(configDirectory.getPath()+File.separator+"savedoptions.sop"));
				String line;
				while ((line = br.readLine()) != null) {
					int index = line.indexOf('\t');
					int container = 0;
					if (index > -1) {
						container = Integer.parseInt(line.substring(0,index));
					}
					
					if (line.startsWith(container+"\tnivre\tpost_processing")) {
					} else if (line.startsWith(container+"\tmalt0.4\tbehavior")) {
						if (line.endsWith("true")) {
							SystemLogger.logger().info("MaltParser 1.3 doesn't support MaltParser 0.4 emulation.");
							br.close();
							bw.flush();
							bw.close();
							deleteConfigDirectory();
							System.exit(0);
						}
					} else if (line.startsWith(container+"\tsinglemalt\tparsing_algorithm")) {
						bw.write(container);
						bw.write("\tsinglemalt\tparsing_algorithm\t");
						if (line.endsWith("NivreStandard")) {
							bw.write("class org.maltparser.parser.algorithm.nivre.NivreArcStandardFactory");	
						} else if (line.endsWith("NivreEager")) {
							bw.write("class org.maltparser.parser.algorithm.nivre.NivreArcEagerFactory");
						} else if (line.endsWith("CovingtonNonProjective")) {
							bw.write("class org.maltparser.parser.algorithm.covington.CovingtonNonProjFactory");
						} else if (line.endsWith("CovingtonProjective")) {
							bw.write("class org.maltparser.parser.algorithm.covington.CovingtonProjFactory");
						}
						bw.write('\n');
					} else {
						bw.write(line);
						bw.write('\n');
					}
				}
				br.close();
				bw.flush();
				bw.close();
				new File(configDirectory.getPath()+File.separator+"savedoptions.sop.old").delete();
			} catch (FileNotFoundException e) {
				throw new ConfigurationException("Could convert savedoptions.sop version 1.0.4 to version 1.1. ", e);
			}  catch (IOException e) {
				throw new ConfigurationException("Could convert savedoptions.sop version 1.0.4 to version 1.1. ", e);
			}
		}
	}
	
	/**
	 * Terminates the configuration directory
	 * 
	 * @throws MaltChainedException
	 */
	public void terminate() throws MaltChainedException {
		if (infoFile != null) {
			try {
				infoFile.flush();
				infoFile.close();
			} catch (IOException e) {
				throw new ConfigurationException("Could not close configuration information file. ", e);
			}
		}
		symbolTables = null;
//		configuration = null;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	protected void finalize() throws Throwable {
		try {
			if (infoFile != null) {
				infoFile.flush();
				infoFile.close();
			}
		} finally {
			super.finalize();
		}
	}
	
	public SymbolTableHandler getSymbolTables() {
		return symbolTables;
	}

	public void setSymbolTables(SymbolTableHandler symbolTables) {
		this.symbolTables = symbolTables;
	}

	public DataFormatManager getDataFormatManager() {
		return dataFormatManager;
	}

	public void setDataFormatManager(DataFormatManager dataFormatManager) {
		this.dataFormatManager = dataFormatManager;
	}
		
	public Set<String> getDataFormatInstanceKeys() {
		return dataFormatInstances.keySet();
	}
	
	public boolean addDataFormatInstance(String key, DataFormatInstance dataFormatInstance) {
		if (!dataFormatInstances.containsKey(key)) {
			dataFormatInstances.put(key, dataFormatInstance);
			return true;
		}
		return false;
	}
	
	public DataFormatInstance getDataFormatInstance(String key) {
		return dataFormatInstances.get(key);
	}
	
	public int sizeDataFormatInstance() {
		return dataFormatInstances.size();
	}
	
	public DataFormatInstance getInputDataFormatInstance() {
		return dataFormatInstances.get(dataFormatManager.getInputDataFormatSpec().getDataFormatName());
	}

	public URL getInputFormatURL() {
		return inputFormatURL;
	}

	public URL getOutputFormatURL() {
		return outputFormatURL;
	}
	
	
}
