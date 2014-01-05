package org.maltparser.ml.lib;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;

import java.io.OutputStreamWriter;
import java.util.ArrayList;

import org.apache.log4j.Logger;

import java.util.LinkedHashMap;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;


import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.core.feature.function.FeatureFunction;
import org.maltparser.core.feature.value.FeatureValue;
import org.maltparser.core.feature.value.MultipleFeatureValue;
import org.maltparser.core.feature.value.SingleFeatureValue;
import org.maltparser.core.syntaxgraph.DependencyStructure;
import org.maltparser.ml.LearningMethod;
import org.maltparser.parser.DependencyParserConfig;
import org.maltparser.parser.guide.instance.InstanceModel;
import org.maltparser.parser.history.action.SingleDecision;

public abstract class Lib implements LearningMethod {
	protected Verbostity verbosity;
	public enum Verbostity {
		SILENT, ERROR, ALL
	}
	protected InstanceModel owner;
	protected int learnerMode;
	protected String name;
	protected int numberOfInstances;
	protected boolean saveInstanceFiles;
	protected boolean excludeNullValues;
	protected BufferedWriter instanceOutput = null; 
	protected FeatureMap featureMap;
	protected String paramString;
	protected String pathExternalTrain;
	protected LinkedHashMap<String, String> libOptions;
	protected String allowedLibOptionFlags;
	protected Logger configLogger;
	protected final Pattern tabPattern = Pattern.compile("\t");
	protected final Pattern pipePattern = Pattern.compile("\\|");	
	private final StringBuilder sb = new StringBuilder();
	protected MaltLibModel model = null;
	/**
	 * Constructs a Lib learner.
	 * 
	 * @param owner the guide model owner
	 * @param learnerMode the mode of the learner BATCH or CLASSIFY
	 */
	public Lib(InstanceModel owner, Integer learnerMode, String learningMethodName) throws MaltChainedException {
		setOwner(owner);
		setLearnerMode(learnerMode.intValue());
		setNumberOfInstances(0);
		setLearningMethodName(learningMethodName);
		verbosity = Verbostity.SILENT;
		configLogger = owner.getGuide().getConfiguration().getConfigLogger();
		initLibOptions();
		initAllowedLibOptionFlags();
		parseParameters(getConfiguration().getOptionValue("lib", "options").toString());
		initSpecialParameters();
		
		if (learnerMode == BATCH) {
			featureMap = new FeatureMap();
			instanceOutput = new BufferedWriter(getInstanceOutputStreamWriter(".ins"));
		} else if (learnerMode == CLASSIFY) {
			featureMap = loadFeatureMap(getInputStreamFromConfigFileEntry(".map"));
		}
	}
	
	
	public void addInstance(SingleDecision decision, FeatureVector featureVector) throws MaltChainedException {
		if (featureVector == null) {
			throw new LibException("The feature vector cannot be found");
		} else if (decision == null) {
			throw new LibException("The decision cannot be found");
		}	
		
		try {
			sb.append(decision.getDecisionCode()+"\t");
			final int n = featureVector.size();
			for (int i = 0; i < n; i++) {
				FeatureValue featureValue = featureVector.getFeatureValue(i);
				if (featureValue == null || (excludeNullValues == true && featureValue.isNullValue())) {
					sb.append("-1");
				} else {
					if (!featureValue.isMultiple()) {
						SingleFeatureValue singleFeatureValue = (SingleFeatureValue)featureValue;
						if (singleFeatureValue.getValue() == 1) {
							sb.append(singleFeatureValue.getIndexCode());
						} else if (singleFeatureValue.getValue() == 0) {
							sb.append("-1");
						} else {
							sb.append(singleFeatureValue.getIndexCode());
							sb.append(":");
							sb.append(singleFeatureValue.getValue());
						}
					} else { //if (featureValue instanceof MultipleFeatureValue) {
						Set<Integer> values = ((MultipleFeatureValue)featureValue).getCodes();
						int j=0;
						for (Integer value : values) {
							sb.append(value.toString());
							if (j != values.size()-1) {
								sb.append("|");
							}
							j++;
						}
					}
//					else {
//						throw new LibException("Don't recognize the type of feature value: "+featureValue.getClass());
//					}
				}
				sb.append('\t');
			}
			sb.append('\n');
			instanceOutput.write(sb.toString());
			instanceOutput.flush();
			increaseNumberOfInstances();
			sb.setLength(0);
		} catch (IOException e) {
			throw new LibException("The learner cannot write to the instance file. ", e);
		}
	}

	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException { }

	public void moveAllInstances(LearningMethod method,
			FeatureFunction divideFeature,
			ArrayList<Integer> divideFeatureIndexVector)
			throws MaltChainedException { 
		if (method == null) {
			throw new LibException("The learning method cannot be found. ");
		} else if (divideFeature == null) {
			throw new LibException("The divide feature cannot be found. ");
		} 
		
		try {
			final BufferedReader in = new BufferedReader(getInstanceInputStreamReader(".ins"));
			final BufferedWriter out = method.getInstanceWriter();
			final StringBuilder sb = new StringBuilder(6);
			int l = in.read();
			char c;
			int j = 0;
	
			while(true) {
				if (l == -1) {
					sb.setLength(0);
					break;
				}
				c = (char)l; 
				l = in.read();
				if (c == '\t') {
					if (divideFeatureIndexVector.contains(j-1)) {
						out.write(Integer.toString(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode()));
						out.write('\t');
					}
					out.write(sb.toString());
					j++;
					out.write('\t');
					sb.setLength(0);
				} else if (c == '\n') {
					out.write(sb.toString());
					if (divideFeatureIndexVector.contains(j-1)) {
						out.write('\t');
						out.write(Integer.toString(((SingleFeatureValue)divideFeature.getFeatureValue()).getIndexCode()));
					}
					out.write('\n');
					sb.setLength(0);
					method.increaseNumberOfInstances();
					this.decreaseNumberOfInstances();
					j = 0;
				} else {
					sb.append(c);
				}
			}	
			in.close();
			getFile(".ins").delete();
			out.flush();
		} catch (SecurityException e) {
			throw new LibException("The learner cannot remove the instance file. ", e);
		} catch (NullPointerException  e) {
			throw new LibException("The instance file cannot be found. ", e);
		} catch (FileNotFoundException e) {
			throw new LibException("The instance file cannot be found. ", e);
		} catch (IOException e) {
			throw new LibException("The learner read from the instance file. ", e);
		}
	}

	public void noMoreInstances() throws MaltChainedException { 
		closeInstanceWriter();
	}

	public boolean predict(FeatureVector featureVector, SingleDecision decision) throws MaltChainedException {
//		if (featureVector == null) {
//			throw new LibException("The learner cannot predict the next class, because the feature vector cannot be found. ");
//		}
		final FeatureList featureList = new FeatureList();
		final int size = featureVector.size();
		for (int i = 1; i <= size; i++) {
			final FeatureValue featureValue = featureVector.getFeatureValue(i-1);	
			if (featureValue != null && !(excludeNullValues == true && featureValue.isNullValue())) {
				if (!featureValue.isMultiple()) {
					SingleFeatureValue singleFeatureValue = (SingleFeatureValue)featureValue;
					final int index = featureMap.getIndex(i, singleFeatureValue.getIndexCode());
					if (index != -1 && singleFeatureValue.getValue() != 0) {
						featureList.add(index,singleFeatureValue.getValue());
					}
				} 
				else { //if (featureValue instanceof MultipleFeatureValue) {
					for (Integer value : ((MultipleFeatureValue)featureValue).getCodes()) {
						final int v = featureMap.getIndex(i, value);
						if (v != -1) {
							featureList.add(v,1);
						}
					}
				} 
			}
		}
		try {
			decision.getKBestList().addList(model.predict(featureList.toArray()));
		} catch (OutOfMemoryError e) {
			throw new LibException("Out of memory. Please increase the Java heap size (-Xmx<size>). ", e);
		}
		return true;
	}
		
//	protected abstract int[] prediction(FeatureList featureList) throws MaltChainedException;
	
	public void train(FeatureVector featureVector) throws MaltChainedException { 
		if (featureVector == null) {
			throw new LibException("The feature vector cannot be found. ");
		} else if (owner == null) {
			throw new LibException("The parent guide model cannot be found. ");
		}
		long startTime = System.currentTimeMillis();
		
//		if (configLogger.isInfoEnabled()) {
//			configLogger.info("\nStart training\n");
//		}
		if (pathExternalTrain != null) {
			trainExternal(featureVector);
		} else {
			trainInternal(featureVector);
		}
//		long elapsed = System.currentTimeMillis() - startTime;
//		if (configLogger.isInfoEnabled()) {
//			configLogger.info("Time 1: " +new Formatter().format("%02d:%02d:%02d", elapsed/3600000, elapsed%3600000/60000, elapsed%60000/1000)+" ("+elapsed+" ms)\n");
//		}
		try {
//			if (configLogger.isInfoEnabled()) {
//				configLogger.info("\nSaving feature map "+getFile(".map").getName()+"\n");
//			}
			saveFeatureMap(new BufferedOutputStream(new FileOutputStream(getFile(".map").getAbsolutePath())), featureMap);
		} catch (FileNotFoundException e) {
			throw new LibException("The learner cannot save the feature map file '"+getFile(".map").getAbsolutePath()+"'. ", e);
		}
//		elapsed = System.currentTimeMillis() - startTime;
//		if (configLogger.isInfoEnabled()) {
//			configLogger.info("Time 2: " +new Formatter().format("%02d:%02d:%02d", elapsed/3600000, elapsed%3600000/60000, elapsed%60000/1000)+" ("+elapsed+" ms)\n");
//		}
	}
	protected abstract void trainExternal(FeatureVector featureVector) throws MaltChainedException;
	protected abstract void trainInternal(FeatureVector featureVector) throws MaltChainedException;
	
	public void terminate() throws MaltChainedException { 
		closeInstanceWriter();
		owner = null;
		model = null;
	}

	public BufferedWriter getInstanceWriter() {
		return instanceOutput;
	}
	
	protected void closeInstanceWriter() throws MaltChainedException {
		try {
			if (instanceOutput != null) {
				instanceOutput.flush();
				instanceOutput.close();
				instanceOutput = null;
			}
		} catch (IOException e) {
			throw new LibException("The learner cannot close the instance file. ", e);
		}
	}
	
	
	/**
	 * Returns the parameter string used for configure the learner
	 * 
	 * @return the parameter string used for configure the learner
	 */
	public String getParamString() {
		return paramString;
	}
	
	public InstanceModel getOwner() {
		return owner;
	}

	protected void setOwner(InstanceModel owner) {
		this.owner = owner;
	}
	
	public int getLearnerMode() {
		return learnerMode;
	}

	public void setLearnerMode(int learnerMode) throws MaltChainedException {
		this.learnerMode = learnerMode;
	}
	
	public String getLearningMethodName() {
		return name;
	}
	
	/**
	 * Returns the current configuration
	 * 
	 * @return the current configuration
	 * @throws MaltChainedException
	 */
	public DependencyParserConfig getConfiguration() throws MaltChainedException {
		return owner.getGuide().getConfiguration();
	}
	
	public int getNumberOfInstances() throws MaltChainedException {
		if(numberOfInstances!=0)
			return numberOfInstances;
		else{
			BufferedReader reader = new BufferedReader( getInstanceInputStreamReader(".ins"));
			try {
				while(reader.readLine()!=null){
					numberOfInstances++;
					owner.increaseFrequency();
				}
				reader.close();
			} catch (IOException e) {
				throw new MaltChainedException("No instances found in file",e);
			}
			return numberOfInstances;
		}
	}

	public void increaseNumberOfInstances() {
		numberOfInstances++;
		owner.increaseFrequency();
	}
	
	public void decreaseNumberOfInstances() {
		numberOfInstances--;
		owner.decreaseFrequency();
	}
	
	protected void setNumberOfInstances(int numberOfInstances) {
		this.numberOfInstances = 0;
	}

	protected void setLearningMethodName(String name) {
		this.name = name;
	}
	
	public String getPathExternalTrain() {
		return pathExternalTrain;
	}


	public void setPathExternalTrain(String pathExternalTrain) {
		this.pathExternalTrain = pathExternalTrain;
	}

	protected OutputStreamWriter getInstanceOutputStreamWriter(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getAppendOutputStreamWriter(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected InputStreamReader getInstanceInputStreamReader(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getInputStreamReader(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected InputStreamReader getInstanceInputStreamReaderFromConfigFile(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getInputStreamReaderFromConfigFile(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected InputStream getInputStreamFromConfigFileEntry(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getInputStreamFromConfigFileEntry(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	
	protected File getFile(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getFile(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected JarEntry getConfigFileEntry(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getConfigFileEntry(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected void initSpecialParameters() throws MaltChainedException {
		if (getConfiguration().getOptionValue("singlemalt", "null_value") != null && getConfiguration().getOptionValue("singlemalt", "null_value").toString().equalsIgnoreCase("none")) {
			excludeNullValues = true;
		} else {
			excludeNullValues = false;
		}
		saveInstanceFiles = ((Boolean)getConfiguration().getOptionValue("lib", "save_instance_files")).booleanValue();
		if (!getConfiguration().getOptionValue("lib", "external").toString().equals("")) {
			String path = getConfiguration().getOptionValue("lib", "external").toString(); 
			try {
				if (!new File(path).exists()) {
					throw new LibException("The path to the external  trainer 'svm-train' is wrong.");
				}
				if (new File(path).isDirectory()) {
					throw new LibException("The option --lib-external points to a directory, the path should point at the 'train' file or the 'train.exe' file in the libsvm or the liblinear package");
				}
				if (!(path.endsWith("train") ||path.endsWith("train.exe"))) {
					throw new LibException("The option --lib-external does not specify the path to 'train' file or the 'train.exe' file in the libsvm or the liblinear package. ");
				}
				setPathExternalTrain(path);
			} catch (SecurityException e) {
				throw new LibException("Access denied to the file specified by the option --lib-external. ", e);
			}
		}
		if (getConfiguration().getOptionValue("lib", "verbosity") != null) {
			verbosity = Verbostity.valueOf(getConfiguration().getOptionValue("lib", "verbosity").toString().toUpperCase());
		}
	}
	
	public String getLibOptions() {
		final StringBuilder sb = new StringBuilder();
		for (String key : libOptions.keySet()) {
			sb.append('-');
			sb.append(key);
			sb.append(' ');
			sb.append(libOptions.get(key));
			sb.append(' ');
		}
		return sb.toString();
	}
	
	public String[] getLibParamStringArray() {
		final ArrayList<String> params = new ArrayList<String>();

		for (String key : libOptions.keySet()) {
			params.add("-"+key); params.add(libOptions.get(key));
		}
		return params.toArray(new String[params.size()]);
	}
	
	public abstract void initLibOptions();
	public abstract void initAllowedLibOptionFlags();
	
	public void parseParameters(String paramstring) throws MaltChainedException {
		if (paramstring == null) {
			return;
		}
		final String[] argv;
		try {
			argv = paramstring.split("[_\\p{Blank}]");
		} catch (PatternSyntaxException e) {
			throw new LibException("Could not split the parameter string '"+paramstring+"'. ", e);
		}
		for (int i=0; i < argv.length-1; i++) {
			if(argv[i].charAt(0) != '-') {
				throw new LibException("The argument flag should start with the following character '-', not with "+argv[i].charAt(0));
			}
			if(++i>=argv.length) {
				throw new LibException("The last argument does not have any value. ");
			}
			try {
				int index = allowedLibOptionFlags.indexOf(argv[i-1].charAt(1));
				if (index != -1) {
					libOptions.put(Character.toString(argv[i-1].charAt(1)), argv[i]);
				} else {
					throw new LibException("Unknown learner parameter: '"+argv[i-1]+"' with value '"+argv[i]+"'. ");		
				}
			} catch (ArrayIndexOutOfBoundsException e) {
				throw new LibException("The learner parameter '"+argv[i-1]+"' could not convert the string value '"+argv[i]+"' into a correct numeric value. ", e);
			} catch (NumberFormatException e) {
				throw new LibException("The learner parameter '"+argv[i-1]+"' could not convert the string value '"+argv[i]+"' into a correct numeric value. ", e);	
			} catch (NullPointerException e) {
				throw new LibException("The learner parameter '"+argv[i-1]+"' could not convert the string value '"+argv[i]+"' into a correct numeric value. ", e);	
			}
		}
	}
	
	protected void finalize() throws Throwable {
		try {
			closeInstanceWriter();
		} finally {
			super.finalize();
		}
	}
	
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("\n"+getLearningMethodName()+" INTERFACE\n");
		sb.append(getLibOptions());
		return sb.toString();
	}

	protected int binariesInstance(String line, FeatureList featureList) throws MaltChainedException {
		int y = -1; 
		featureList.clear();
		try {	
			String[] columns = tabPattern.split(line);

			if (columns.length == 0) {
				return -1;
			}
			try {
				y = Integer.parseInt(columns[0]);
			} catch (NumberFormatException e) {
				throw new LibException("The instance file contain a non-integer value '"+columns[0]+"'", e);
			}
			for(int j = 1; j < columns.length; j++) {
				final String[] items = pipePattern.split(columns[j]);
				for (int k = 0; k < items.length; k++) {
					try {
						int colon = items[k].indexOf(':');
						if (colon == -1) {
							if (Integer.parseInt(items[k]) != -1) {
								int v = featureMap.addIndex(j, Integer.parseInt(items[k]));
								if (v != -1) {
									featureList.add(v,1);
								}
							}
						} else {
							int index = featureMap.addIndex(j, Integer.parseInt(items[k].substring(0,colon)));
							double value;
							if (items[k].substring(colon+1).indexOf('.') != -1) {
								value = Double.parseDouble(items[k].substring(colon+1));
							} else {
								value = Integer.parseInt(items[k].substring(colon+1));
							}
							featureList.add(index,value);
						}
					} catch (NumberFormatException e) {
						throw new LibException("The instance file contain a non-numeric value '"+items[k]+"'", e);
					}
				}
			}
		} catch (ArrayIndexOutOfBoundsException e) {
			throw new LibException("Couln't read from the instance file. ", e);
		}
		return y;
	}

	protected void binariesInstances2SVMFileFormat(InputStreamReader isr, OutputStreamWriter osw) throws MaltChainedException {
		try {
			final BufferedReader in = new BufferedReader(isr);
			final BufferedWriter out = new BufferedWriter(osw);
			final FeatureList featureSet = new FeatureList();
			while(true) {
				String line = in.readLine();
				if(line == null) break;
				int y = binariesInstance(line, featureSet);
				if (y == -1) {
					continue;
				}
				out.write(Integer.toString(y));
				
		        for (int k=0; k < featureSet.size(); k++) {
		        	MaltFeatureNode x = featureSet.get(k);
					out.write(' ');
					out.write(Integer.toString(x.getIndex()));
					out.write(':');
					out.write(Double.toString(x.getValue()));         
				}
				out.write('\n');
			}			
			in.close();	
			out.close();
		} catch (NumberFormatException e) {
			throw new LibException("The instance file contain a non-numeric value", e);
		} catch (IOException e) {
			throw new LibException("Couln't read from the instance file, when converting the Malt instances into LIBSV/LIBLINEAR format. ", e);
		}
	}
	
	protected void saveFeatureMap(OutputStream os, FeatureMap map) throws MaltChainedException {
		try {
		    ObjectOutputStream output = new ObjectOutputStream(os);
	        try{
	          output.writeObject(map);
	        }
	        finally{
	          output.close();
	        }
		} catch (IOException e) {
			throw new LibException("Save feature map error", e);
		}
	}

	protected FeatureMap loadFeatureMap(InputStream is) throws MaltChainedException {
		FeatureMap map = new FeatureMap();
		try {
		    ObjectInputStream input = new ObjectInputStream(is);
		    try {
		    	map = (FeatureMap)input.readObject();
		    } finally {
		    	input.close();
		    }
		} catch (ClassNotFoundException e) {
			throw new LibException("Load feature map error", e);
		} catch (IOException e) {
			throw new LibException("Load feature map error", e);
		}
		return map;
	}
}
