package org.maltparser.core.config.version;

import java.io.File;
import java.util.Date;
import java.util.jar.JarEntry;

import org.maltparser.core.helper.SystemInfo;
import org.maltparser.core.helper.SystemLogger;

public class Versioning {
	private String maltParserVersion;
	private String parserModelVersion;
	private File mcoPath;
	private String configName;
	private String newConfigName;
	private String configType;
	
	private String featureModelXML;
	private String inputFormatXML;
	
	public static String[] availableVersions = {"1.0.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.2", "1.3", "1.3.1", "1.4", "1.4.1"};
	public static boolean[] supportVersions = {false, false, false, false, false, false, true, true, true};
	
	public Versioning(String configName, String configType, File mcoPath, String parserModelVersion) {
		setConfigName(configName);
		setConfigType(configType);
		setMcoPath(mcoPath);
		setMaltParserVersion(SystemInfo.getVersion());
		setParserModelVersion(parserModelVersion);
		setNewConfigName(configName+"."+maltParserVersion);
	}

	public JarEntry getJarEntry(JarEntry in) {
		if (maltParserVersion.equals(parserModelVersion)) {
			return in;
		}
		String entryName = in.getName().replace(configName+File.separator, newConfigName+File.separator);
		if (entryName.endsWith(".info")) {
			return new JarEntry(entryName.replace(File.separator+configName+"_", File.separator+newConfigName+"_"));
		}
		return new JarEntry(entryName);
	}
	
	public boolean hasChanges(JarEntry in, JarEntry out) {
		if (maltParserVersion.equals(parserModelVersion)) {
			return false;
		}
		if (in.getName().endsWith(".info") || in.getName().endsWith(".sop")) {
			return true;
		}
		return false;
	}
	
	public String modifyJarEntry(JarEntry in, JarEntry out, StringBuilder sb) {
		if (maltParserVersion.equals(parserModelVersion)) {
			return sb.toString();
		}
		if (in.getName().endsWith(".info")) {
			final StringBuilder outString = new StringBuilder();
			String[] lines = sb.toString().split("\\n");
			for (int i = 0; i < lines.length; i++) {
				if (lines[i].startsWith("Configuration name:")) {
					outString.append("Configuration name:   ");
					outString.append(configName);
					outString.append('.');
					outString.append(maltParserVersion);
					outString.append('\n');
				} else if (lines[i].startsWith("Created:")) {
					outString.append(lines[i]);
					outString.append('\n');
					outString.append("Converted:            ");
					outString.append(new Date(System.currentTimeMillis()));
					outString.append('\n');
				} else if (lines[i].startsWith("Version:")) {
					outString.append("Version:                       ");
					outString.append(maltParserVersion);
					outString.append('\n');
					outString.append("Created by:                    ");
					outString.append(parserModelVersion);
					outString.append('\n');
				} else if (lines[i].startsWith("  name (  -c)                           ")) {
					outString.append("  name (  -c)                           ");
					outString.append(newConfigName);
					outString.append('\n');
				} else if (lines[i].startsWith("  format ( -if)                         /appdata/dataformat/")) {
					outString.append("  format ( -if)                         ");
					int index = lines[i].lastIndexOf("/");
					outString.append(lines[i].substring(index + 1));
					outString.append('\n');
				} else if (lines[i].startsWith("  format ( -of)                         /appdata/dataformat/")) {
					outString.append("  format ( -of)                         ");
					int index = lines[i].lastIndexOf("/");
					outString.append(lines[i].substring(index + 1));
					outString.append('\n');
				} else if (lines[i].startsWith("--guide-features (  -F)                 /appdata/features/")) {
					outString.append("--guide-features (  -F)                 ");
					int index = lines[i].lastIndexOf("/");
					outString.append(lines[i].substring(index + 1));
					outString.append('\n');
				} else {
					outString.append(lines[i]);
					outString.append('\n');
				}
			}
			return outString.toString();
		} else if (in.getName().endsWith(".sop")) {
			final StringBuilder outString = new StringBuilder();
			String[] lines = sb.toString().split("\\n");
			for (int i = 0; i < lines.length; i++) {
				int index = lines[i].indexOf('\t');
				int container = 0;
				if (index > -1) {
					container = Integer.parseInt(lines[i].substring(0,index));
				}
				if (lines[i].startsWith(container+"\tguide\tfeatures")) {
					int tabIndex = lines[i].lastIndexOf('\t');
					if (lines[i].substring(tabIndex+1).startsWith("/appdata/features/")) {
						int slashIndex = lines[i].lastIndexOf("/");
						String xmlFile = lines[i].substring(slashIndex+1);
						String path = lines[i].substring(tabIndex+1, slashIndex);
						setFeatureModelXML(path + "/libsvm/" +  xmlFile);
						outString.append(container);
						outString.append("\tguide\tfeatures\t");
						outString.append(xmlFile);
						outString.append('\n');
					} else {
						outString.append(lines[i]);
						outString.append('\n');
					}
				} else if (lines[i].startsWith(container+"\tinput\tformat")) {
					int tabIndex = lines[i].lastIndexOf('\t');
					if (lines[i].substring(tabIndex+1).startsWith("/appdata/dataformat/")) {
						int slashIndex = lines[i].lastIndexOf("/");
						String xmlFile = lines[i].substring(slashIndex+1);
						String path = lines[i].substring(tabIndex+1, slashIndex);
						setInputFormatXML(path + "/" +  xmlFile);
						outString.append(container);
						outString.append("\tinput\tformat\t");
						outString.append(xmlFile);
						outString.append('\n');
					} else {
						outString.append(lines[i]);
						outString.append('\n');
					}
				} else if (earlierVersion("1.3")) {
					if (lines[i].startsWith(container+"\tnivre\tpost_processing")) {
					} else if (lines[i].startsWith(container+"\tmalt0.4\tbehavior")) {
						if (lines[i].endsWith("true")) {
							SystemLogger.logger().info("MaltParser "+maltParserVersion+" doesn't support MaltParser 0.4 emulation.");
						}
					} else if (lines[i].startsWith(container+"\tsinglemalt\tparsing_algorithm")) {
						outString.append(container);
						outString.append("\tsinglemalt\tparsing_algorithm\t");
						if (lines[i].endsWith("NivreStandard")) {
							outString.append("class org.maltparser.parser.algorithm.nivre.NivreArcStandardFactory");	
						} else if (lines[i].endsWith("NivreEager")) {
							outString.append("class org.maltparser.parser.algorithm.nivre.NivreArcEagerFactory");
						} else if (lines[i].endsWith("CovingtonNonProjective")) {
							outString.append("class org.maltparser.parser.algorithm.covington.CovingtonNonProjFactory");
						} else if (lines[i].endsWith("CovingtonProjective")) {
							outString.append("class org.maltparser.parser.algorithm.covington.CovingtonProjFactory");
						}
						outString.append('\n');
					}
				} else {
					outString.append(lines[i]);
					outString.append('\n');
				}
			}
			return outString.toString();
		}
		return sb.toString();
	}
	
	
	public boolean earlierVersion(String version) {
		boolean e = false;
		for (int i = 0; i < availableVersions.length; i++) {
			if (availableVersions[i].equals(version)) {
				break;
			} else if (availableVersions[i].equals(parserModelVersion)) {
				e = true;
			}
		}
		return e;
	}
	
	public boolean support(String version) {
		for (int i = 0; i < availableVersions.length; i++) {
			if (availableVersions[i].equals(version)) {
				return supportVersions[i];
			} 
		}
		return false;
	}
	
	public String getFeatureModelXML() {
		return featureModelXML;
	}

	public void setFeatureModelXML(String featureModelXML) {
		this.featureModelXML = featureModelXML;
	}

	public String getInputFormatXML() {
		return inputFormatXML;
	}

	public void setInputFormatXML(String inputFormatXML) {
		this.inputFormatXML = inputFormatXML;
	}

	public String getNewConfigName() {
		return newConfigName;
	}

	public void setNewConfigName(String newConfigName) {
		this.newConfigName = newConfigName;
	}

	public String getConfigName() {
		return configName;
	}

	public void setConfigName(String configName) {
		this.configName = configName;
	}

	public String getConfigType() {
		return configType;
	}

	public void setConfigType(String configType) {
		this.configType = configType;
	}

	public File getMcoPath() {
		return mcoPath;
	}

	public void setMcoPath(File mcoPath) {
		this.mcoPath = mcoPath;
	}

	public String getMaltParserVersion() {
		return maltParserVersion;
	}

	public void setMaltParserVersion(String maltParserVersion) {
		this.maltParserVersion = maltParserVersion;
	}

	public String getParserModelVersion() {
		return parserModelVersion;
	}

	public void setParserModelVersion(String parserModelVersion) {
		this.parserModelVersion = parserModelVersion;
	}
}
