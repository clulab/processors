package org.maltparser.core.config;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.flow.FlowChartInstance;
import org.maltparser.core.flow.item.ChartItem;
import org.maltparser.core.flow.spec.ChartItemSpecification;
import org.maltparser.core.helper.SystemInfo;
import org.maltparser.core.helper.SystemLogger;
import org.maltparser.core.options.OptionManager;
/**
*
*
* @author Johan Hall
*/
public class ConfigDirChartItem extends ChartItem {
	private String idName;
	private String taskName;
	private String optionFileName;
	private URL configDirURL;
	private String configDirName;
	private ConfigurationDir configDir;
	private String outCharSet;
	private String inCharSet;
	
	public ConfigDirChartItem() {}
	
	public void initialize(FlowChartInstance flowChartinstance, ChartItemSpecification chartItemSpecification) throws MaltChainedException {
		super.initialize(flowChartinstance, chartItemSpecification);
		
		for (String key : chartItemSpecification.getChartItemAttributes().keySet()) {
			if (key.equals("id")) {
				idName = chartItemSpecification.getChartItemAttributes().get(key);
			}  else if (key.equals("task")) {
				taskName = chartItemSpecification.getChartItemAttributes().get(key);
			}
		}
		if (idName == null) {
			idName = getChartElement("configdir").getAttributes().get("id").getDefaultValue();
		} else if (taskName == null) {
			taskName = getChartElement("configdir").getAttributes().get("task").getDefaultValue();
		}
		
		if (OptionManager.instance().getOptionValue(getOptionContainerIndex(), "config", "url") != null && OptionManager.instance().getOptionValue(getOptionContainerIndex(),"config", "url").toString().length() > 0) {
			try {			
				configDirURL = new URL(OptionManager.instance().getOptionValue(getOptionContainerIndex(), "config", "url").toString());
			} catch (MalformedURLException e) {
				throw new ConfigurationException("The URL '"+OptionManager.instance().getOptionValue(getOptionContainerIndex(), "config", "url").toString()+"' is malformed. ", e);
			}
		}
		if (OptionManager.instance().getOptionValue(getOptionContainerIndex(), "config", "name").toString().endsWith(".mco")) {
			int index = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "config", "name").toString().lastIndexOf('.');
			configDirName = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "config", "name").toString().substring(0,index);
		} else {
			configDirName = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "config", "name").toString();
		}
		
		try {
			if (OptionManager.instance().getOptionValue(getOptionContainerIndex(), "system", "option_file") != null) {
				optionFileName = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "system", "option_file").toString();
			}
		} catch (ConfigurationException e) {
			throw new ConfigurationException("The option file '"+optionFileName+"' could not be copied. ",e);
		}
		if (OptionManager.instance().getOptionValue(getOptionContainerIndex(), "output", "charset") != null) {
			outCharSet = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "output", "charset").toString();
		} else {
			outCharSet = "UTF-8";
		}
		
		if (OptionManager.instance().getOptionValue(getOptionContainerIndex(), "input", "charset") != null) {
			inCharSet = OptionManager.instance().getOptionValue(getOptionContainerIndex(), "input", "charset").toString();
		} else {
			inCharSet = "UTF-8";
		}
		
		configDir = (ConfigurationDir)flowChartinstance.getFlowChartRegistry(org.maltparser.core.config.ConfigurationDir.class, idName);
		if (configDir == null) {
			if (configDirURL != null) {
				configDir = new ConfigurationDir(configDirURL);
			} else {
				configDir = new ConfigurationDir(configDirName, idName, getOptionContainerIndex());
			}
			
			flowChartinstance.addFlowChartRegistry(org.maltparser.core.config.ConfigurationDir.class, idName, configDir);
		}
		if (taskName.equals("versioning")) {
			configDir.versioning();
		} else if (taskName.equals("loadsavedoptions")) {
			configDir.initCreatedByMaltParserVersionFromInfoFile();
			if (configDir.getCreatedByMaltParserVersion() == null) {
				SystemLogger.logger().warn("Couln't determine which version of MaltParser that created the parser model: " + configDirName+ ".mco\n MaltParser will terminate\n");
				System.exit(1);
			} else if (!configDir.getCreatedByMaltParserVersion().substring(0,3).equals(SystemInfo.getVersion().substring(0,3))) {
				SystemLogger.logger().error("The parser model '"+ configDirName+ ".mco' is created by MaltParser "+configDir.getCreatedByMaltParserVersion()+".\n");
				SystemLogger.logger().error("You have to re-train the parser model to be able to parse with current version of MaltParser.\n");
				System.exit(1);
			}
			OptionManager.instance().loadOptions(getOptionContainerIndex(), configDir.getInputStreamReaderFromConfigFile("savedoptions.sop"));
			configDir.initDataFormat();
		} else if (taskName.equals("createdir")) {
			configDir.setCreatedByMaltParserVersion(SystemInfo.getVersion());
			configDir.createConfigDirectory();
			if (optionFileName != null && optionFileName.length() > 0) {
				configDir.copyToConfig(new File(optionFileName));
			}
			configDir.initDataFormat();
		}
	}
	
	public int preprocess(int signal) throws MaltChainedException {
		if (taskName.equals("unpack")) {
			SystemLogger.logger().info("Unpacking the parser model '"+ configDirName+ ".mco' ...\n");
			configDir.unpackConfigFile();
		} else if (taskName.equals("info")) {
			configDir.echoInfoFile();
		} else if (taskName.equals("loadsymboltables")) {
			configDir.getSymbolTables().load(configDir.getInputStreamReaderFromConfigFileEntry("symboltables.sym",inCharSet));
		}
		return signal;
	}
	
	
	public int process(int signal) throws MaltChainedException {
		return signal;
	}
	
	public int postprocess(int signal) throws MaltChainedException {
		if (taskName.equals("createfile")) {
			OptionManager.instance().saveOptions(getOptionContainerIndex(), configDir.getOutputStreamWriter("savedoptions.sop"));
			configDir.createConfigFile();
		} else if (taskName.equals("deletedir")) {
			configDir.terminate();
			configDir.deleteConfigDirectory();
		} else if (taskName.equals("savesymboltables")) {
			configDir.getSymbolTables().save(configDir.getOutputStreamWriter("symboltables.sym", outCharSet));
		}
		return signal;
	}
	
	public void terminate() throws MaltChainedException { }
	
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return obj.toString().equals(this.toString());
	}
	
	public int hashCode() {
		return 217 + (null == toString() ? 0 : toString().hashCode());
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append("    configdir ");
		sb.append("id:");sb.append(idName);
		sb.append(' ');
		sb.append("task:");sb.append(taskName);
		
		return sb.toString();
	}

}
