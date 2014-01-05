package org.maltparser.ml.libsvm;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import libsvm.svm;
import libsvm.svm_model;
import libsvm.svm_node;
import libsvm.svm_parameter;
import libsvm.svm_problem;

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
import org.maltparser.parser.history.kbest.KBestList;
import org.maltparser.parser.history.kbest.ScoredKBestList;

/**
Implements an interface to the LIBSVM learner (currently the LIBSVM 2.91 is used). More information
about LIBSVM can be found at 
<a href="http://www.csie.ntu.edu.tw/~cjlin/libsvm/" target="_blank">LIBSVM -- A Library for Support Vector Machines</a>.

@author Johan Hall
@since 1.0
*/
public class Libsvm implements LearningMethod {
	public final static String LIBSVM_VERSION = "2.91";
	public enum Verbostity {
		SILENT, ERROR, ALL
	}
	protected InstanceModel owner;
	protected int learnerMode;
	protected String name;
	protected int numberOfInstances;
	protected boolean saveInstanceFiles;
	protected boolean excludeNullValues;
	protected String pathExternalSVMTrain = null;
//	private int[] cardinalities;

	/**
	 * Instance output stream writer 
	 */
	private BufferedWriter instanceOutput = null; 
	/**
	 * LIBSVM svm_model object, only used during classification.
	 */
	private svm_model model = null;
	
	/**
	 * LIBSVM svm_parameter object
	 */
	private svm_parameter svmParam;
	/**
	 * Parameter string
	 */
	private String paramString;
	/**
	 * An array of LIBSVM svm_node objects, only used during classification.
	 */
	private ArrayList<svm_node> xlist = null;

	private Verbostity verbosity;
	/**
	 * Constructs a LIBSVM learner.
	 * 
	 * @param owner the guide model owner
	 * @param learnerMode the mode of the learner TRAIN or CLASSIFY
	 */
	public Libsvm(InstanceModel owner, Integer learnerMode) throws MaltChainedException {
		setOwner(owner);
		setLearningMethodName("libsvm");
		setLearnerMode(learnerMode.intValue());
		setNumberOfInstances(0);
		verbosity = Verbostity.SILENT;
		initSvmParam(getConfiguration().getOptionValue("libsvm", "libsvm_options").toString());
		initSpecialParameters();
		if (learnerMode == BATCH) {
//			if (owner.getGuide().getConfiguration().getConfigLogger().isInfoEnabled()) {
//				if (pathExternalSVMTrain != null) {
//					owner.getGuide().getConfiguration().getConfigLogger().info("  Learner              : LIBSVM external "+ getParamString() + "\n");
//				} else {
//					owner.getGuide().getConfiguration().getConfigLogger().info("  Learner              : LIBSVM "+LIBSVM_VERSION+" "+ getParamString() + "\n");
//				}
//			}
			instanceOutput = new BufferedWriter(getInstanceOutputStreamWriter(".ins"));
		} 
//		else {
//			if (owner.getGuide().getConfiguration().getConfigLogger().isInfoEnabled()) {
//				owner.getGuide().getConfiguration().getConfigLogger().info("  Classifier           : LIBSVM "+LIBSVM_VERSION+" "+ getParamString()+ "\n");
//			}
//		}
	}
	
	
	public void addInstance(SingleDecision decision, FeatureVector featureVector) throws MaltChainedException {
		if (featureVector == null) {
			throw new LibsvmException("The feature vector cannot be found");
		} else if (decision == null) {
			throw new LibsvmException("The decision cannot be found");
		}	
		try {
			instanceOutput.write(decision.getDecisionCode()+"\t");
			for (int i = 0; i < featureVector.size(); i++) {
				FeatureValue featureValue = featureVector.get(i).getFeatureValue();
				if (excludeNullValues == true && featureValue.isNullValue()) {
					instanceOutput.write("-1");
				} else {
					if (featureValue instanceof SingleFeatureValue) {
						instanceOutput.write(((SingleFeatureValue)featureValue).getIndexCode()+"");
					} else if (featureValue instanceof MultipleFeatureValue) {
						Set<Integer> values = ((MultipleFeatureValue)featureValue).getCodes();
						int j=0;
						for (Integer value : values) {
							instanceOutput.write(value.toString());
							if (j != values.size()-1) {
								instanceOutput.write("|");
							}
							j++;
						}
					}
				}
				if (i != featureVector.size()) {
					instanceOutput.write('\t');
				}
			}

			instanceOutput.write('\n');
			instanceOutput.flush();
			increaseNumberOfInstances();
		} catch (IOException e) {
			throw new LibsvmException("The LIBSVM learner cannot write to the instance file. ", e);
		}
	}
	
	public void finalizeSentence(DependencyStructure dependencyGraph) throws MaltChainedException { }
	
	/* (non-Javadoc)
	 * @see org.maltparser.ml.LearningMethod#noMoreInstances()
	 */
	public void noMoreInstances() throws MaltChainedException {
		closeInstanceWriter();
	}


	/* (non-Javadoc)
	 * @see org.maltparser.ml.LearningMethod#train(org.maltparser.parser.guide.feature.FeatureVector)
	 */
	public void train(FeatureVector featureVector) throws MaltChainedException {
		if (featureVector == null) {
			throw new LibsvmException("The feature vector cannot be found. ");
		} else if (owner == null) {
			throw new LibsvmException("The parent guide model cannot be found. ");
		}
//		cardinalities = getCardinalities(featureVector);
//		if (pathExternalSVMTrain == null) {
//			try {
//				final svm_problem prob = readProblemMaltSVMFormat(getInstanceInputStreamReader(".ins"), cardinalities, svmParam);
//				if(svm.svm_check_parameter(prob, svmParam) != null) {
//					throw new LibsvmException(svm.svm_check_parameter(prob, svmParam));
//				}
//				owner.getGuide().getConfiguration().getConfigLogger().info("Creating LIBSVM model "+getFile(".mod").getName()+"\n");
//				final PrintStream out = System.out;
//				final PrintStream err = System.err;
//				System.setOut(NoPrintStream.NO_PRINTSTREAM);
//				System.setErr(NoPrintStream.NO_PRINTSTREAM);
//				
//				svm.svm_save_model(getFile(".mod").getAbsolutePath(), svm.svm_train(prob, svmParam));
//				System.setOut(err);
//				System.setOut(out);
//				if (!saveInstanceFiles) {
//					getFile(".ins").delete();
//				}
//			} catch (OutOfMemoryError e) {
//				throw new LibsvmException("Out of memory. Please increase the Java heap size (-Xmx<size>). ", e);
//			} catch (IllegalArgumentException e) {
//				throw new LibsvmException("The LIBSVM learner was not able to redirect Standard Error stream. ", e);
//			} catch (SecurityException e) {
//				throw new LibsvmException("The LIBSVM learner cannot remove the instance file. ", e);
//			} catch (IOException e) {
//				throw new LibsvmException("The LIBSVM learner cannot save the model file '"+getFile(".mod").getAbsolutePath()+"'. ", e);
//			}
//		} else {
//			trainExternal(featureVector);
//		}
//		saveCardinalities(getInstanceOutputStreamWriter(".car"), cardinalities);
	}
	
	

	private void trainExternal(FeatureVector featureVector) throws MaltChainedException {
		try {		
//			maltSVMFormat2OriginalSVMFormat(getInstanceInputStreamReader(".ins"), getInstanceOutputStreamWriter(".ins.tmp"), cardinalities);
			owner.getGuide().getConfiguration().getConfigLogger().info("Creating LIBSVM model (svm-train) "+getFile(".mod").getName());

			final ArrayList<String> commands = new ArrayList<String>();
			commands.add(pathExternalSVMTrain);
			final String[] params = getSVMParamStringArray(svmParam);
			for (int i=0; i < params.length; i++) {
				commands.add(params[i]);
			}
			commands.add(getFile(".ins.tmp").getAbsolutePath());
			commands.add(getFile(".mod").getAbsolutePath());
			String[] arrayCommands =  commands.toArray(new String[commands.size()]);
			
	        if (verbosity == Verbostity.ALL) {
	        	owner.getGuide().getConfiguration().getConfigLogger().info('\n');
	        }
			final Process child = Runtime.getRuntime().exec(arrayCommands);
	        final InputStream in = child.getInputStream();
	        final InputStream err = child.getErrorStream();
	        int c;
	        while ((c = in.read()) != -1){
	        	if (verbosity == Verbostity.ALL) {
	        		owner.getGuide().getConfiguration().getConfigLogger().info((char)c);
	        	}
	        }
	        while ((c = err.read()) != -1){
	        	if (verbosity == Verbostity.ALL || verbosity == Verbostity.ERROR) {
	        		owner.getGuide().getConfiguration().getConfigLogger().info((char)c);
	        	}
	        }
            if (child.waitFor() != 0) {
            	owner.getGuide().getConfiguration().getConfigLogger().info(" FAILED ("+child.exitValue()+")");
            }
	        in.close();
	        err.close();
	        if (!saveInstanceFiles) {
				getFile(".ins").delete();
				getFile(".ins.tmp").delete();
	        }
	        owner.getGuide().getConfiguration().getConfigLogger().info('\n');
		} catch (InterruptedException e) {
			 throw new LibsvmException("SVM-trainer is interrupted. ", e);
		} catch (IllegalArgumentException e) {
			throw new LibsvmException("The LIBSVM learner was not able to redirect Standard Error stream. ", e);
		} catch (SecurityException e) {
			throw new LibsvmException("The LIBSVM learner cannot remove the instance file. ", e);
		} catch (IOException e) {
			throw new LibsvmException("The LIBSVM learner cannot save the model file '"+getFile(".mod").getAbsolutePath()+"'. ", e);
		} catch (OutOfMemoryError e) {
			throw new LibsvmException("Out of memory. Please increase the Java heap size (-Xmx<size>). ", e);
		}
	}
	
//	private int[] getCardinalities(FeatureVector featureVector) {
//		int[] cardinalities = new int[featureVector.size()];
//		int i = 0;
//		for (FeatureFunction feature : featureVector) {
//			cardinalities[i++] = feature.getFeatureValue().getCardinality();
//		}
//		return cardinalities;
//	}
//	
//	private void saveCardinalities(OutputStreamWriter osw, int[] cardinalities) throws MaltChainedException {
//		final BufferedWriter out = new BufferedWriter(osw);
//		try {
//			for (int i = 0, n = cardinalities.length; i < n; i++) {
//				out.write(Integer.toString(cardinalities[i]));
//				if (i < n - 1) {
//					out.write(',');
//				}
//			}
//			out.write('\n');
//			out.close();
//		} catch (IOException e) {
//			throw new LibsvmException("Couldn't save the cardinalities to file. ", e);
//		}
//	}
//	
//	private int[] loadCardinalities(InputStreamReader isr) throws MaltChainedException {
//		int[] cardinalities = null;
//		try {
//			final BufferedReader in = new BufferedReader(isr); 
//			String line;
//			if ((line = in.readLine()) != null) {
//				String[] items = line.split(",");
//				cardinalities = new int[items.length];
//				for (int i = 0; i < items.length; i++) {
//					cardinalities[i] = Integer.parseInt(items[i]);
//				}
// 			}
//			in.close();
//		} catch (IOException e) {
//			throw new LibsvmException("The cardinalities cannot be read because wrongly formatted. ", e);
//		} catch (NumberFormatException e) {
//			throw new LibsvmException("Couldn't load the cardinalities from file. ", e);
//		}
//		return cardinalities;
//	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.ml.LearningMethod#moveAllInstances(org.maltparser.ml.LearningMethod, org.maltparser.core.feature.function.FeatureFunction, java.util.ArrayList)
	 */
	public void moveAllInstances(LearningMethod method, FeatureFunction divideFeature, ArrayList<Integer> divideFeatureIndexVector) throws MaltChainedException {
		if (method == null) {
			throw new LibsvmException("The learning method cannot be found. ");
		} else if (divideFeature == null) {
			throw new LibsvmException("The divide feature cannot be found. ");
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
					if (sb.length() > 0) { 
						out.write(sb.toString());
					}
					if (divideFeatureIndexVector.contains(j-1)) {
						if (sb.length() > 0) { 
							out.write('\t');
						}
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
		} catch (SecurityException e) {
			throw new LibsvmException("The LIBSVM learner cannot remove the instance file. ", e);
		} catch (NullPointerException  e) {
			throw new LibsvmException("The instance file cannot be found. ", e);
		} catch (FileNotFoundException e) {
			throw new LibsvmException("The instance file cannot be found. ", e);
		} catch (IOException e) {
			throw new LibsvmException("The LIBSVM learner read from the instance file. ", e);
		}
	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.ml.LearningMethod#predict(org.maltparser.parser.guide.feature.FeatureVector, org.maltparser.ml.KBestList)
	 */
	public boolean predict(FeatureVector featureVector, SingleDecision decision) throws MaltChainedException {
		if (model == null) {
			try {
				model = svm.svm_load_model(new BufferedReader(getInstanceInputStreamReaderFromConfigFile(".mod")));
			} catch (IOException e) {
				throw new LibsvmException("The model cannot be loaded. ", e);
			}
		}
//		if (cardinalities == null) {
//			if (getConfigFileEntry(".car") != null) {
//				cardinalities = loadCardinalities(getInstanceInputStreamReaderFromConfigFile(".car"));
//			} else {
//				cardinalities = getCardinalities(featureVector);
//			}
//		}
		if (xlist == null) {
			xlist = new ArrayList<svm_node>(featureVector.size()); 
		}
		if (model == null) { 
			throw new LibsvmException("The LIBSVM learner cannot predict the next class, because the learning model cannot be found. ");
		} else if (featureVector == null) {
			throw new LibsvmException("The LIBSVM learner cannot predict the next class, because the feature vector cannot be found. ");
		}
//		int j = 0;
//		int offset = 0;
//		int i = 0;
//		for (FeatureFunction feature : featureVector) {
//			final FeatureValue featureValue = feature.getFeatureValue();
//			if (!(excludeNullValues == true && featureValue.isNullValue())) {
//				if (featureValue instanceof SingleFeatureValue) {
//					if (((SingleFeatureValue)featureValue).getCode() < cardinalities[i]) {
//						if (j >= xlist.size()) {
//							svm_node x =  new svm_node();
//							x.value = 1;
//							xlist.add(j,x);
//						}
//						xlist.get(j++).index = ((SingleFeatureValue)featureValue).getCode() + offset;
//					}
//				} else if (featureValue instanceof MultipleFeatureValue) {
//					for (Integer value : ((MultipleFeatureValue)featureValue).getCodes()) {
//						if (value < cardinalities[i]) {
////						if (((MultipleFeatureValue)featureValue).isKnown(value)) {
//							if (j >= xlist.size()) {
//								svm_node x =  new svm_node();
//								x.value = 1;
//								xlist.add(j,x);
//							}
//							xlist.get(j++).index = value + offset;
//						}
//					}
//				}
//			}
//			offset += cardinalities[i];
//			i++;
//		}
//
//		svm_node[] xarray = new svm_node[j];
//		for (int k = 0; k < j; k++) {
//			xarray[k] = xlist.get(k);
//		}
//		try {
//			if (decision.getKBestList().getK() == 1 || svm.svm_get_svm_type(model) == svm_parameter.ONE_CLASS ||
//					svm.svm_get_svm_type(model) == svm_parameter.EPSILON_SVR ||
//					svm.svm_get_svm_type(model) == svm_parameter.NU_SVR) {
//				decision.getKBestList().add((int)svm.svm_predict(model, xarray));
//			} else {
//				svm_predict_with_kbestlist(model, xarray, decision.getKBestList());
//			}
//
//		} catch (OutOfMemoryError e) {
//				throw new LibsvmException("Out of memory. Please increase the Java heap size (-Xmx<size>). ", e);
//		}

		return true;
	}
	

	public void terminate() throws MaltChainedException { 
		closeInstanceWriter();
		model = null;
		svmParam = null;
		xlist = null;
		owner = null;
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
			throw new LibsvmException("The LIBSVM learner cannot close the instance file. ", e);
		}
	}
	
	/**
	 * Initialize the LIBSVM according to the parameter string
	 * 
	 * @param paramString the parameter string to configure the LIBSVM learner.
	 * @throws MaltChainedException
	 */
	protected void initSvmParam(String paramString) throws MaltChainedException {
		this.paramString = paramString;
		svmParam = new svm_parameter();
		initParameters(svmParam);
		parseParameters(paramString, svmParam);
	}
	
	/**
	 * Returns the parameter string for used for configure LIBSVM
	 * 
	 * @return the parameter string for used for configure LIBSVM
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
			//Do a line count of the instance file and return that
			
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
	
	protected OutputStreamWriter getInstanceOutputStreamWriter(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getAppendOutputStreamWriter(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected InputStreamReader getInstanceInputStreamReader(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getInputStreamReader(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected InputStreamReader getInstanceInputStreamReaderFromConfigFile(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getInputStreamReaderFromConfigFile(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected File getFile(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getFile(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected JarEntry getConfigFileEntry(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getConfigFileEntry(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	/**
	 * Reads an instance file into a svm_problem object according to the Malt-SVM format, which is column fixed format (tab-separated).
	 * 
	 * @param isr	the instance stream reader for the instance file
	 * @param cardinalities	a array containing the number of distinct values for a particular column.
	 * @param param	a svm_parameter object
	 * @throws LibsvmException
	 */
	public final svm_problem readProblemMaltSVMFormat(InputStreamReader isr, int[] cardinalities, svm_parameter param) throws MaltChainedException {
		final svm_problem prob = new svm_problem();
		try {
			final BufferedReader fp = new BufferedReader(isr);
			int max_index = 0;
			if (xlist == null) {
				xlist = new ArrayList<svm_node>(); 
			}
			prob.l = getNumberOfInstances();
			prob.x = new svm_node[prob.l][];
			prob.y = new double[prob.l];
			int i = 0;
			final Pattern tabPattern = Pattern.compile("\t");
			final Pattern pipePattern = Pattern.compile("\\|");
			while(true) {
				String line = fp.readLine();
				if(line == null) break;
				String[] columns = tabPattern.split(line);

				if (columns.length == 0) {
					continue;
				}
				
				int offset = 0; 
				int j = 0;
				try {
					prob.y[i] = (double)Integer.parseInt(columns[j]);
					int p = 0;
					for(j = 1; j < columns.length; j++) {
						final String[] items = pipePattern.split(columns[j]);	
						for (int k = 0; k < items.length; k++) {
							try {
								if (Integer.parseInt(items[k]) != -1) {
									xlist.add(p, new svm_node());
									xlist.get(p).value = 1;
									xlist.get(p).index = Integer.parseInt(items[k])+offset;
									p++;
								}
							} catch (NumberFormatException e) {
								throw new LibsvmException("The instance file contain a non-integer value '"+items[k]+"'", e);
							}
						}
						offset += cardinalities[j-1];
					}
					prob.x[i] = xlist.subList(0, p).toArray(new svm_node[0]);
					if(columns.length > 1) {
						max_index = Math.max(max_index, xlist.get(p-1).index);
					}
					i++;
					xlist.clear();
				} catch (ArrayIndexOutOfBoundsException e) {
					throw new LibsvmException("Cannot read from the instance file. ", e);
				}
			}
			fp.close();	
			if (param.gamma == 0) {
				param.gamma = 1.0/max_index;
			}
			xlist = null;
		} catch (IOException e) {
			throw new LibsvmException("Cannot read from the instance file. ", e);
		}
		return prob;
	}
	
	protected void initSpecialParameters() throws MaltChainedException {
		if (getConfiguration().getOptionValue("singlemalt", "null_value") != null && getConfiguration().getOptionValue("singlemalt", "null_value").toString().equalsIgnoreCase("none")) {
			excludeNullValues = true;
		} else {
			excludeNullValues = false;
		}
		saveInstanceFiles = ((Boolean)getConfiguration().getOptionValue("libsvm", "save_instance_files")).booleanValue();
			
		if (!getConfiguration().getOptionValue("libsvm", "libsvm_external").toString().equals("")) {
			try {
				if (!new File(getConfiguration().getOptionValue("libsvm", "libsvm_external").toString()).exists()) {
					throw new LibsvmException("The path to the external LIBSVM trainer 'svm-train' is wrong.");
				}
				if (new File(getConfiguration().getOptionValue("libsvm", "libsvm_external").toString()).isDirectory()) {
					throw new LibsvmException("The option --libsvm-libsvm_external points to a directory, the path should point at the 'svm-train' file or the 'svm-train.exe' file");
				}
				if (!(getConfiguration().getOptionValue("libsvm", "libsvm_external").toString().endsWith("svm-train") || getConfiguration().getOptionValue("libsvm", "libsvm_external").toString().endsWith("svm-train.exe"))) {
					throw new LibsvmException("The option --libsvm-libsvm_external does not specify the path to 'svm-train' file or the 'svm-train.exe' file. ");
				}
				pathExternalSVMTrain = getConfiguration().getOptionValue("libsvm", "libsvm_external").toString();
			} catch (SecurityException e) {
				throw new LibsvmException("Access denied to the file specified by the option --libsvm-libsvm_external. ", e);
			}
		}
		if (getConfiguration().getOptionValue("libsvm", "verbosity") != null) {
			verbosity = Verbostity.valueOf(getConfiguration().getOptionValue("libsvm", "verbosity").toString().toUpperCase());
		}
	}
	
	/**
	 * Assign a default value to all svm parameters
	 * 
	 * @param param	a svm_parameter object
	 */
	protected void initParameters(svm_parameter param) throws MaltChainedException {
		if (param == null) {
			throw new LibsvmException("Svm-parameters cannot be found. ");
		}
		param.svm_type = svm_parameter.C_SVC;
		param.kernel_type = svm_parameter.POLY;
		param.degree = 2;
		param.gamma = 0.2;	// 1/k
		param.coef0 = 0;
		param.nu = 0.5;
		param.cache_size = 100; 
		param.C = 1; 
		param.eps = 1.0; 
		param.p = 0.1;
		param.shrinking = 1;
		param.probability = 0;
		param.nr_weight = 0;
		param.weight_label = new int[0];
		param.weight = new double[0];
	}
	
	/**
	 * Returns a string containing all svm-parameters of interest
	 * 
	 * @param param a svm_parameter object
	 * @return a string containing all svm-parameters of interest
	 */
	public String toStringParameters(svm_parameter param)  {
		if (param == null) {
			throw new IllegalArgumentException("Svm-parameters cannot be found. ");
		}
		final StringBuffer sb = new StringBuffer();
		
		final String[] svmtypes = {"C_SVC", "NU_SVC","ONE_CLASS","EPSILON_SVR","NU_SVR"};
		final String[] kerneltypes = {"LINEAR", "POLY","RBF","SIGMOID","PRECOMPUTED"};
		final DecimalFormat dform = new DecimalFormat("#0.0#"); 
		final DecimalFormatSymbols sym = new DecimalFormatSymbols();
		sym.setDecimalSeparator('.');
		dform.setDecimalFormatSymbols(sym);
		sb.append("LIBSVM SETTINGS\n");
		sb.append("  SVM type      : " + svmtypes[param.svm_type] + " (" + param.svm_type + ")\n");
		sb.append("  Kernel        : " + kerneltypes[param.kernel_type] + " (" + param.kernel_type + ")\n");
		if (param.kernel_type == svm_parameter.POLY) {
			sb.append("  Degree        : " + param.degree + "\n");
		}
		if (param.kernel_type == svm_parameter.POLY || param.kernel_type == svm_parameter.RBF || param.kernel_type == svm_parameter.SIGMOID) {
			sb.append("  Gamma         : " + dform.format(param.gamma) + "\n");
			if (param.kernel_type == svm_parameter.POLY || param.kernel_type == svm_parameter.SIGMOID) {
				sb.append("  Coef0         : " + dform.format(param.coef0) + "\n");
			}
		}
		if (param.svm_type == svm_parameter.NU_SVC || param.svm_type == svm_parameter.NU_SVR || param.svm_type == svm_parameter.ONE_CLASS) {
			sb.append("  Nu            : " + dform.format(param.nu) + "\n");
		}
		sb.append("  Cache Size    : " + dform.format(param.cache_size) + " MB\n");
		if (param.svm_type == svm_parameter.C_SVC || param.svm_type == svm_parameter.NU_SVR || param.svm_type == svm_parameter.EPSILON_SVR) {
			sb.append("  C             : " + dform.format(param.C) + "\n");
		}
		sb.append("  Eps           : " + dform.format(param.eps) + "\n");
		if (param.svm_type == svm_parameter.EPSILON_SVR) {
			sb.append("  P             : " + dform.format(param.p) + "\n");
		}
		sb.append("  Shrinking     : " + param.shrinking + "\n");
		sb.append("  Probability   : " + param.probability + "\n");
		if (param.svm_type == svm_parameter.C_SVC) {
			sb.append("  #Weight       : " + param.nr_weight + "\n");
			if (param.nr_weight > 0) {
				sb.append("  Weight labels : ");
				for (int i = 0; i < param.nr_weight; i++) {
					sb.append(param.weight_label[i]);
					if (i != param.nr_weight-1) {
						sb.append(", ");
					}
				}
				sb.append("\n");
				for (int i = 0; i < param.nr_weight; i++) {
					sb.append(dform.format(param.weight));
					if (i != param.nr_weight-1) {
						sb.append(", ");
					}
				}
				sb.append("\n");
			}
		}
		return sb.toString();
	}
	
	public String[] getSVMParamStringArray(svm_parameter param) {
		final ArrayList<String> params = new ArrayList<String>();

		if (param.svm_type != 0) {
			params.add("-s"); params.add(new Integer(param.svm_type).toString());
		}
		if (param.kernel_type != 2) {
			params.add("-t"); params.add(new Integer(param.kernel_type).toString());
		}
		if (param.degree != 3) {
			params.add("-d"); params.add(new Integer(param.degree).toString());
		}
		params.add("-g"); params.add(new Double(param.gamma).toString());
		if (param.coef0 != 0) {
			params.add("-r"); params.add(new Double(param.coef0).toString());
		}
		if (param.nu != 0.5) {
			params.add("-n"); params.add(new Double(param.nu).toString());
		}
		if (param.cache_size != 100) {
			params.add("-m"); params.add(new Double(param.cache_size).toString());
		}
		if (param.C != 1) {
			params.add("-c"); params.add(new Double(param.C).toString());
		}
		if (param.eps != 0.001) {
			params.add("-e"); params.add(new Double(param.eps).toString());
		}
		if (param.p != 0.1) {
			params.add("-p"); params.add(new Double(param.p).toString());
		}
		if (param.shrinking != 1) {
			params.add("-h"); params.add(new Integer(param.shrinking).toString());
		}
		if (param.probability != 0) {
			params.add("-b"); params.add(new Integer(param.probability).toString());
		}

		return params.toArray(new String[params.size()]);
	}
	
	/**
	 * Parses the parameter string. The parameter string must contain parameter and value pairs, which are separated by a blank 
	 * or a underscore. The parameter begins with a character '-' followed by a one-character flag and the value must comply with
	 * the parameters data type. Some examples:
	 * 
	 * -s 0 -t 1 -d 2 -g 0.4 -e 0.1
	 * -s_0_-t_1_-d_2_-g_0.4_-e_0.1
	 * 
	 * @param paramstring	the parameter string 
	 * @param param	a svm_parameter object
	 * @throws LibsvmException
	 */
	public void parseParameters(String paramstring, svm_parameter param) throws MaltChainedException {
		if (param == null) {
			throw new LibsvmException("Svm-parameters cannot be found. ");
		}
		if (paramstring == null) {
			return;
		}
		final String[] argv;
		try {
			argv = paramstring.split("[_\\p{Blank}]");
		} catch (PatternSyntaxException e) {
			throw new LibsvmException("Could not split the svm-parameter string '"+paramstring+"'. ", e);
		}
		for (int i=0; i < argv.length-1; i++) {
			if(argv[i].charAt(0) != '-') {
				throw new LibsvmException("The argument flag should start with the following character '-', not with "+argv[i].charAt(0));
			}
			if(++i>=argv.length) {
				throw new LibsvmException("The last argument does not have any value. ");
			}
			try {
				switch(argv[i-1].charAt(1)) {
				case 's':
					param.svm_type = Integer.parseInt(argv[i]);
					break;
				case 't':
					param.kernel_type = Integer.parseInt(argv[i]);
					break;
				case 'd':
					param.degree = Integer.parseInt(argv[i]);
					break;
				case 'g':
					param.gamma = Double.valueOf(argv[i]).doubleValue();
					break;
				case 'r':
					param.coef0 = Double.valueOf(argv[i]).doubleValue();
					break;
				case 'n':
					param.nu = Double.valueOf(argv[i]).doubleValue();
					break;
				case 'm':
					param.cache_size = Double.valueOf(argv[i]).doubleValue();
					break;
				case 'c':
					param.C = Double.valueOf(argv[i]).doubleValue();
					break;
				case 'e':
					param.eps = Double.valueOf(argv[i]).doubleValue();
					break;
				case 'p':
					param.p = Double.valueOf(argv[i]).doubleValue();
					break;
				case 'h':
					param.shrinking = Integer.parseInt(argv[i]);
					break;
			    case 'b':
					param.probability = Integer.parseInt(argv[i]);
					break;
				case 'w':
					++param.nr_weight;
					{
						int[] old = param.weight_label;
						param.weight_label = new int[param.nr_weight];
						System.arraycopy(old,0,param.weight_label,0,param.nr_weight-1);
					}
	
					{
						double[] old = param.weight;
						param.weight = new double[param.nr_weight];
						System.arraycopy(old,0,param.weight,0,param.nr_weight-1);
					}
	
					param.weight_label[param.nr_weight-1] = Integer.parseInt(argv[i].substring(2));
					param.weight[param.nr_weight-1] = Double.valueOf(argv[i]).doubleValue();
					break;
				case 'Y':
				case 'V':
				case 'S':
				case 'F':
				case 'T':
				case 'M':
				case 'N':
					break;
				default:
					throw new LibsvmException("Unknown svm parameter: '"+argv[i-1]+"' with value '"+argv[i]+"'. ");		
				}
			} catch (ArrayIndexOutOfBoundsException e) {
				throw new LibsvmException("The svm-parameter '"+argv[i-1]+"' could not convert the string value '"+argv[i]+"' into a correct numeric value. ", e);
			} catch (NumberFormatException e) {
				throw new LibsvmException("The svm-parameter '"+argv[i-1]+"' could not convert the string value '"+argv[i]+"' into a correct numeric value. ", e);	
			} catch (NullPointerException e) {
				throw new LibsvmException("The svm-parameter '"+argv[i-1]+"' could not convert the string value '"+argv[i]+"' into a correct numeric value. ", e);	
			}
		}
	}

	public void svm_predict_with_kbestlist(svm_model model, svm_node[] x, KBestList kBestList) throws MaltChainedException {
		int i;
		final int nr_class = svm.svm_get_nr_class(model);
		final double[] dec_values = new double[nr_class*(nr_class-1)/2];
		svm.svm_predict_values(model, x, dec_values);

		final int[] vote = new int[nr_class];
		final double[] score = new double[nr_class];
		final int[] voteindex = new int[nr_class];
		for(i=0;i<nr_class;i++) {
			vote[i] = 0;
			score[i] = 0.0;
			voteindex[i] = i;
		}
		int pos=0;
		for(i=0;i<nr_class;i++) {
			for(int j=i+1;j<nr_class;j++) {
				if(dec_values[pos] > 0) {
					vote[i]++;
				} else {
					vote[j]++;
				}
				score[i] += dec_values[pos];
				score[j] += dec_values[pos];
				pos++;
			}
		}
		for(i=0;i<nr_class;i++) {
			score[i] = score[i]/nr_class;
		}
		int lagest, tmpint;
		double tmpdouble;
		for (i=0;i<nr_class-1;i++) {
			lagest = i;
			for (int j=i;j<nr_class;j++) {
				if (vote[j] > vote[lagest]) {
					lagest = j;
				}
			}
			tmpint = vote[lagest];
			vote[lagest] = vote[i];
			vote[i] = tmpint;
			tmpdouble = score[lagest];
			score[lagest] = score[i];
			score[i] = tmpdouble;
			tmpint = voteindex[lagest];
			voteindex[lagest] = voteindex[i];
			voteindex[i] = tmpint;
		}
		final int[] labels = new int[nr_class];
		svm.svm_get_labels(model, labels);
		int k = nr_class-1;
		if (kBestList.getK() != -1) {
			k = kBestList.getK() - 1;
		}
		
		for (i=0; i<nr_class && k >= 0; i++, k--) {
			if (vote[i] > 0 || i == 0) {
				if (kBestList instanceof ScoredKBestList) {
					((ScoredKBestList)kBestList).add(labels[voteindex[i]], (float)vote[i]/(float)(nr_class*(nr_class-1)/2));
				} else {
					kBestList.add(labels[voteindex[i]]);
				}
			}
		}
	}
	/**
	 * Converts the instance file (Malt's own SVM format) into the LIBSVM (SVMLight) format. The input instance file is removed (replaced)
	 * by the instance file in the LIBSVM (SVMLight) format. If a column contains -1, the value will be removed in destination file. 
	 * 
	 * @param isr the input stream reader for the source instance file
	 * @param osw	the output stream writer for the destination instance file
	 * @param cardinalities a vector containing the number of distinct values for a particular column
	 * @throws LibsvmException
	 */
	public static void maltSVMFormat2OriginalSVMFormat(InputStreamReader isr, OutputStreamWriter osw, int[] cardinalities) throws MaltChainedException {
		try {
			final BufferedReader in = new BufferedReader(isr);
			final BufferedWriter out = new BufferedWriter(osw);

			int c;
			int j = 0;
			int offset = 0;
			int code = 0;
			while(true) {
				c = in.read();
				if (c == -1) {
					break;
				}
				
				if (c == '\t' || c == '|') {
					if (j == 0) {
						out.write(Integer.toString(code));
						j++;
					} else {
						if (code != -1) {
							out.write(' ');
							out.write(Integer.toString(code+offset));
							out.write(":1");
						}
						if (c == '\t') {
							offset += cardinalities[j-1];
							j++;
						}
					}
					code = 0;
				} else if (c == '\n') {
					j = 0;
					offset = 0;
					out.write('\n');
					code = 0;
				} else if (c == '-') {
					code = -1;
				} else if (code != -1) {
					if (c > 47 && c < 58) {
						code = code * 10 + (c-48);
					} else {
						throw new LibsvmException("The instance file contain a non-integer value, when converting the Malt SVM format into LIBSVM format.");
					}
				}	
			}			
			in.close();	
			out.close();
		} catch (IOException e) {
			throw new LibsvmException("Cannot read from the instance file, when converting the Malt SVM format into LIBSVM format. ", e);
		}
	}
	
	protected void finalize() throws Throwable {
		try {
			closeInstanceWriter();
		} finally {
			super.finalize();
		}
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("\nLIBSVM INTERFACE\n"); 
		sb.append("  LIBSVM version: "+LIBSVM_VERSION+"\n");
		sb.append("  SVM-param string: "+paramString+"\n");
		
		sb.append(toStringParameters(svmParam));
		return sb.toString();
	}
}
