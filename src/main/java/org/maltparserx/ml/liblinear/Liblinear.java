package org.maltparserx.ml.liblinear;

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
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.jar.JarEntry;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import de.bwaldvogel.liblinear.FeatureNode;
import de.bwaldvogel.liblinear.Linear;
import de.bwaldvogel.liblinear.Model;
import de.bwaldvogel.liblinear.Parameter;
import de.bwaldvogel.liblinear.Problem;
import de.bwaldvogel.liblinear.SolverType;



import org.maltparserx.core.exception.MaltChainedException;
import org.maltparserx.core.feature.FeatureVector;
import org.maltparserx.core.feature.function.FeatureFunction;
import org.maltparserx.core.feature.value.FeatureValue;
import org.maltparserx.core.feature.value.MultipleFeatureValue;
import org.maltparserx.core.feature.value.SingleFeatureValue;
import org.maltparserx.core.helper.NoPrintStream;
import org.maltparserx.core.syntaxgraph.DependencyStructure;
import org.maltparserx.ml.LearningMethod;
import org.maltparserx.parser.DependencyParserConfig;
import org.maltparserx.parser.guide.instance.InstanceModel;
import org.maltparserx.parser.history.action.SingleDecision;
import org.maltparserx.parser.history.kbest.KBestList;
import org.maltparserx.parser.history.kbest.ScoredKBestList;



public class Liblinear implements LearningMethod {
	public final static String LIBLINEAR_VERSION = "1.51";
	public enum Verbostity {
		SILENT, ERROR, ALL
	}
	private LinkedHashMap<String, String> liblinearOptions;
	 
	protected InstanceModel owner;
	protected int learnerMode;
	protected String name;
	protected int numberOfInstances;
	protected boolean saveInstanceFiles;
	protected boolean excludeNullValues;
	protected String pathExternalLiblinearTrain = null;
//	private int[] cardinalities;
	/**
	 * Instance output stream writer 
	 */
	private BufferedWriter instanceOutput = null; 
	/**
	 * Liblinear model object, only used during classification.
	 */
	private Model model = null;
	
	/**
	 * Parameter string
	 */
	private String paramString;

	private ArrayList<FeatureNode> xlist = null;

	private Verbostity verbosity;
	
	private HashMap<Long,Integer> featureMap;
	private int featureCounter = 1;
	private boolean featurePruning = false;
	private TreeSet<XNode> featureSet;
	
	/**
	 * Constructs a Liblinear learner.
	 * 
	 * @param owner the guide model owner
	 * @param learnerMode the mode of the learner TRAIN or CLASSIFY
	 */
	public Liblinear(InstanceModel owner, Integer learnerMode) throws MaltChainedException {
		setOwner(owner);
		setLearningMethodName("liblinear");
		setLearnerMode(learnerMode.intValue());
		setNumberOfInstances(0);
		verbosity = Verbostity.SILENT;

		liblinearOptions = new LinkedHashMap<String, String>();
		initLiblinearOptions();
		parseParameters(getConfiguration().getOptionValue("liblinear", "liblinear_options").toString());
		initSpecialParameters();
		
		if (learnerMode == BATCH) {
			if (featurePruning) {
				featureMap = new HashMap<Long, Integer>();
			}
			instanceOutput = new BufferedWriter(getInstanceOutputStreamWriter(".ins"));
		} 
		if (featurePruning) {
			featureSet = new TreeSet<XNode>();
		}
	}
	
	private int addFeatureMapValue(int featurePosition, int code) {
		long key = ((((long)featurePosition) << 48) | (long)code);
		if (featureMap.containsKey(key)) {
			return featureMap.get(key);
		}
		int value = featureCounter++;
		featureMap.put(key, value);
		return value;
	}
	
	private int getFeatureMapValue(int featurePosition, int code) {
		long key = ((((long)featurePosition) << 48) | (long)code);
		if (featureMap.containsKey(key)) {
			return featureMap.get(key);
		}
		return -1;
	}
	
	private void saveFeatureMap(OutputStream os, HashMap<Long,Integer> map) throws MaltChainedException {
		try {
		    ObjectOutputStream obj_out_stream = new ObjectOutputStream (os);
		    obj_out_stream.writeObject(map);
		    obj_out_stream.close();
		} catch (IOException e) {
			throw new LiblinearException("Save feature map error", e);
		}
	}
	
	private HashMap<Long,Integer> loadFeatureMap(InputStream is) throws MaltChainedException {
		HashMap<Long,Integer> map = new HashMap<Long,Integer>();
		try {
		    ObjectInputStream obj_in_stream = new ObjectInputStream(is);
		    map = (HashMap<Long,Integer>)obj_in_stream.readObject();
		    obj_in_stream.close();
		} catch (ClassNotFoundException e) {
			throw new LiblinearException("Load feature map error", e);
		} catch (IOException e) {
			throw new LiblinearException("Load feature map error", e);
		}
		return map;
	}
	
	public void addInstance(SingleDecision decision, FeatureVector featureVector) throws MaltChainedException {
		if (featureVector == null) {
			throw new LiblinearException("The feature vector cannot be found");
		} else if (decision == null) {
			throw new LiblinearException("The decision cannot be found");
		}	
		
		StringBuilder sb = new StringBuilder();
		try {
			sb.append(decision.getDecisionCode()+"\t");
			int n = featureVector.size();
			for (int i = 0; i < n; i++) {
				FeatureValue featureValue = featureVector.get(i).getFeatureValue();
				if (excludeNullValues == true && featureValue.isNullValue()) {
					sb.append("-1");
				} else {
					if (featureValue instanceof SingleFeatureValue) {
						sb.append(((SingleFeatureValue)featureValue).getIndexCode()+"");
					} else if (featureValue instanceof MultipleFeatureValue) {
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
				}
				sb.append('\t');
			}
			sb.append('\n');
			instanceOutput.write(sb.toString());
			instanceOutput.flush();
			increaseNumberOfInstances();
		} catch (IOException e) {
			throw new LiblinearException("The Liblinear learner cannot write to the instance file. ", e);
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
			throw new LiblinearException("The feature vector cannot be found. ");
		} else if (owner == null) {
			throw new LiblinearException("The parent guide model cannot be found. ");
		}
//		cardinalities = getCardinalities(featureVector);
		if (pathExternalLiblinearTrain == null) {
			try {
				Problem problem = null;
				if (featurePruning) {
					problem = readLibLinearProblemWithFeaturePruning(getInstanceInputStreamReader(".ins"));
				} else {
//					problem = readLibLinearProblem(getInstanceInputStreamReader(".ins"), cardinalities);
				}
				
				if (owner.getGuide().getConfiguration().getConfigLogger().isInfoEnabled()) {
					owner.getGuide().getConfiguration().getConfigLogger().info("Creating Liblinear model "+getFile(".mod").getName()+"\n");
				}
				final PrintStream out = System.out;
				final PrintStream err = System.err;
				System.setOut(NoPrintStream.NO_PRINTSTREAM);
				System.setErr(NoPrintStream.NO_PRINTSTREAM);
				Linear.saveModel(new File(getFile(".mod").getAbsolutePath()), Linear.train(problem, getLiblinearParameters()));
				System.setOut(err);
				System.setOut(out);
				if (!saveInstanceFiles) {
					getFile(".ins").delete();
				}
			} catch (OutOfMemoryError e) {
				throw new LiblinearException("Out of memory. Please increase the Java heap size (-Xmx<size>). ", e);
			} catch (IllegalArgumentException e) {
				throw new LiblinearException("The Liblinear learner was not able to redirect Standard Error stream. ", e);
			} catch (SecurityException e) {
				throw new LiblinearException("The Liblinear learner cannot remove the instance file. ", e);
			} catch (IOException e) {
				throw new LiblinearException("The Liblinear learner cannot save the model file '"+getFile(".mod").getAbsolutePath()+"'. ", e);
			}
		} else {
			trainExternal(featureVector);
		}
		
		if (featurePruning) {
			try {
				saveFeatureMap(new FileOutputStream(getFile(".map").getAbsolutePath()), featureMap);
			} catch (FileNotFoundException e) {
				throw new LiblinearException("The Liblinear learner cannot save the feature map file '"+getFile(".map").getAbsolutePath()+"'. ", e);
			}
		} else {
//			saveCardinalities(getInstanceOutputStreamWriter(".car"), cardinalities);
		}
	}
	
	private void trainExternal(FeatureVector featureVector) throws MaltChainedException {
		try {		
//			maltSVMFormat2OriginalSVMFormat(getInstanceInputStreamReader(".ins"), getInstanceOutputStreamWriter(".ins.tmp"), cardinalities);
			owner.getGuide().getConfiguration().getConfigLogger().info("Creating Liblinear model (external) "+getFile(".mod").getName());

			final String[] params = getLibLinearParamStringArray();
			String[] arrayCommands = new String[params.length+3];
			int i = 0;
			arrayCommands[i++] = pathExternalLiblinearTrain;
			for (; i <= params.length; i++) {
				arrayCommands[i] = params[i-1];
			}
			arrayCommands[i++] = getFile(".ins.tmp").getAbsolutePath();
			arrayCommands[i++] = getFile(".mod").getAbsolutePath();
			
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
			 throw new LiblinearException("Liblinear is interrupted. ", e);
		} catch (IllegalArgumentException e) {
			throw new LiblinearException("The Liblinear learner was not able to redirect Standard Error stream. ", e);
		} catch (SecurityException e) {
			throw new LiblinearException("The Liblinear learner cannot remove the instance file. ", e);
		} catch (IOException e) {
			throw new LiblinearException("The Liblinear learner cannot save the model file '"+getFile(".mod").getAbsolutePath()+"'. ", e);
		} catch (OutOfMemoryError e) {
			throw new LiblinearException("Out of memory. Please increase the Java heap size (-Xmx<size>). ", e);
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
//			throw new LiblinearException("", e);
//		}
//	}
	
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
//			throw new LiblinearException("", e);
//		} catch (NumberFormatException e) {
//			throw new LiblinearException("", e);
//		}
//		return cardinalities;
//	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.ml.LearningMethod#moveAllInstances(org.maltparser.ml.LearningMethod, org.maltparser.core.feature.function.FeatureFunction, java.util.ArrayList)
	 */
	public void moveAllInstances(LearningMethod method, FeatureFunction divideFeature, ArrayList<Integer> divideFeatureIndexVector) throws MaltChainedException {
		if (method == null) {
			throw new LiblinearException("The learning method cannot be found. ");
		} else if (divideFeature == null) {
			throw new LiblinearException("The divide feature cannot be found. ");
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
			throw new LiblinearException("The Liblinear learner cannot remove the instance file. ", e);
		} catch (NullPointerException  e) {
			throw new LiblinearException("The instance file cannot be found. ", e);
		} catch (FileNotFoundException e) {
			throw new LiblinearException("The instance file cannot be found. ", e);
		} catch (IOException e) {
			throw new LiblinearException("The Liblinear learner read from the instance file. ", e);
		}

	}
	
	/* (non-Javadoc)
	 * @see org.maltparser.ml.LearningMethod#predict(org.maltparser.parser.guide.feature.FeatureVector, org.maltparser.ml.KBestList)
	 */
	public boolean predict(FeatureVector featureVector, SingleDecision decision) throws MaltChainedException {
		if (model == null) {
			try {
				model = Linear.loadModel(new BufferedReader(getInstanceInputStreamReaderFromConfigFile(".mod")));
			} catch (IOException e) {
				throw new LiblinearException("The model cannot be loaded. ", e);
			}
		}
		if (model == null) { 
			throw new LiblinearException("The Liblinear learner cannot predict the next class, because the learning model cannot be found. ");
		} else if (featureVector == null) {
			throw new LiblinearException("The Liblinear learner cannot predict the next class, because the feature vector cannot be found. ");
		}
		if (featurePruning) {
			return predictWithFeaturePruning(featureVector, decision);
		}
		
//		if (cardinalities == null) {
//			if (getConfigFileEntry(".car") != null) {
//				cardinalities = loadCardinalities(getInstanceInputStreamReaderFromConfigFile(".car"));
//			} else {
//				cardinalities = getCardinalities(featureVector);
//			}
//		}

		//System.out.println("METHOD PREDICT CARDINALITIES SIZE" + cardinalities.length + " FEATURE VECTOR SIZE " +featureVector.size());
		if (xlist == null) {
			xlist = new ArrayList<FeatureNode>(featureVector.size()); 
		}


//		int offset = 1;
//		int i = 0;
//		for (FeatureFunction feature : featureVector) {
//			final FeatureValue featureValue = feature.getFeatureValue();
//			if (!(excludeNullValues == true && featureValue.isNullValue())) {
//				if (featureValue instanceof SingleFeatureValue) {
//					if (((SingleFeatureValue)featureValue).getCode() < cardinalities[i]) {
//						xlist.add(new FeatureNode(((SingleFeatureValue)featureValue).getCode() + offset, 1));
//					}
//				} else if (featureValue instanceof MultipleFeatureValue) {
//					for (Integer value : ((MultipleFeatureValue)featureValue).getCodes()) {
//						if (value < cardinalities[i]) {
//							xlist.add(new FeatureNode(value + offset, 1));
//						}
//					}
//				}
//			}
//			offset += cardinalities[i];
//			i++;
//		}
		FeatureNode[] xarray = new FeatureNode[xlist.size()];
		for (int k = 0; k < xlist.size(); k++) {
			xarray[k] = xlist.get(k);
		}

		if (decision.getKBestList().getK() == 1) {
			decision.getKBestList().add((int) Linear.predict(model, xarray));
		} else {
			liblinear_predict_with_kbestlist(model, xarray, decision.getKBestList());
		}
		
		xlist.clear();

		return true;
	}
	
	public boolean predictWithFeaturePruning(FeatureVector featureVector, SingleDecision decision) throws MaltChainedException {
		if (featureMap == null) {
			featureMap = loadFeatureMap(getInputStreamFromConfigFileEntry(".map"));
		}
		int i = 1;
		for (FeatureFunction feature : featureVector) {
			final FeatureValue featureValue = feature.getFeatureValue();
			if (!(excludeNullValues == true && featureValue.isNullValue())) {
				if (featureValue instanceof SingleFeatureValue) {
					int v = getFeatureMapValue(i, ((SingleFeatureValue)featureValue).getIndexCode());
					if (v != -1) {
						featureSet.add(new XNode(v,1));
					}
				} else if (featureValue instanceof MultipleFeatureValue) {
					for (Integer value : ((MultipleFeatureValue)featureValue).getCodes()) {
						int v = getFeatureMapValue(i, value);
						if (v != -1) {
							featureSet.add(new XNode(v,1));
						}
					}
				}
			}
			i++;
		}
		FeatureNode[] xarray = new FeatureNode[featureSet.size()];
		int k = 0;
		for (XNode x : featureSet) {
			xarray[k++] = new FeatureNode(x.getIndex(), x.getValue());
		}


		if (decision.getKBestList().getK() == 1) {
			decision.getKBestList().add((int) Linear.predict(model, xarray));
		} else {
			liblinear_predict_with_kbestlist(model, xarray, decision.getKBestList());
		}
		featureSet.clear();

		return true;
	}
	
	public void terminate() throws MaltChainedException { 
		closeInstanceWriter();
		model = null;
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
			throw new LiblinearException("The Liblinear learner cannot close the instance file. ", e);
		}
	}
	
	
	/**
	 * Returns the parameter string for used for configure Liblinear
	 * 
	 * @return the parameter string for used for configure Liblinear
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
	
	protected InputStream getInputStreamFromConfigFileEntry(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getInputStreamFromConfigFileEntry(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	
	protected File getFile(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getFile(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	protected JarEntry getConfigFileEntry(String suffix) throws MaltChainedException {
		return getConfiguration().getConfigurationDir().getConfigFileEntry(owner.getModelName()+getLearningMethodName()+suffix);
	}
	
	
	public Problem readLibLinearProblemWithFeaturePruning(InputStreamReader isr) throws MaltChainedException {
		Problem problem = new Problem();
		
		try {
			final BufferedReader fp = new BufferedReader(isr);
			
			problem.bias = -1;
			problem.l = getNumberOfInstances();
			problem.x = new FeatureNode[problem.l][];
			problem.y = new double[problem.l];
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
				int j = 0;
				try {
					problem.y[i] = Integer.parseInt(columns[j]);					
					for(j = 1; j < columns.length; j++) {
						final String[] items = pipePattern.split(columns[j]);
						for (int k = 0; k < items.length; k++) {
							try {
								int colon = items[k].indexOf(':');
								if (colon == -1) {
									if (Integer.parseInt(items[k]) != -1) {
										int v = addFeatureMapValue(j, Integer.parseInt(items[k]));
										if (v != -1) {
											featureSet.add(new XNode(v,1));
										}
									}
								} else {
									int index = addFeatureMapValue(j, Integer.parseInt(items[k].substring(0,colon)));
									double value;
									if (items[k].substring(colon+1).indexOf('.') != -1) {
										value = Double.parseDouble(items[k].substring(colon+1));
									} else {
										value = Integer.parseInt(items[k].substring(colon+1));
									}
									featureSet.add(new XNode(index,value));
								}
							} catch (NumberFormatException e) {
								throw new LiblinearException("The instance file contain a non-integer value '"+items[k]+"'", e);
							}
						}
					}
					problem.x[i] = new FeatureNode[featureSet.size()];
					int p = 0;
					for (XNode x : featureSet) {
						problem.x[i][p++] = new FeatureNode(x.getIndex(), x.getValue());
					}
					featureSet.clear();
					i++;
				} catch (ArrayIndexOutOfBoundsException e) {
					throw new LiblinearException("Cannot read from the instance file. ", e);
				}
			}
			fp.close();	
			featureSet = null;
			problem.n = featureMap.size();
			System.out.println("Number of features: "+problem.n);
		} catch (IOException e) {
			throw new LiblinearException("Cannot read from the instance file. ", e);
		}
		return problem;
	}
	/**
	 * Reads an instance file into a problem object according to the Malt-SVM format, which is column fixed format (tab-separated).
	 * 
	 * @param isr	the instance stream reader for the instance file
	 * @param cardinalities	a array containing the number of distinct values for a particular column.
	 * @throws LiblinearException
	 */
	public Problem readLibLinearProblem(InputStreamReader isr, int[] cardinalities) throws MaltChainedException {
		Problem problem = new Problem();

		try {
			final BufferedReader fp = new BufferedReader(isr);
			int max_index = 0;
			if (xlist == null) {
				xlist = new ArrayList<FeatureNode>(); 
			}
			problem.bias = -1; //getBias();
			problem.l = getNumberOfInstances();
			problem.x = new FeatureNode[problem.l][];
			problem.y = new double[problem.l];
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
				
				int offset = 1; 
				int j = 0;
				try {
					problem.y[i] = 
						Integer.parseInt(columns[j]);
					int p = 0;
					for(j = 1; j < columns.length; j++) {
						final String[] items = pipePattern.split(columns[j]);	
						for (int k = 0; k < items.length; k++) {
							try {
								if (Integer.parseInt(items[k]) != -1) {
									xlist.add(p, new FeatureNode(Integer.parseInt(items[k])+offset, 1));
									p++;
								}
							} catch (NumberFormatException e) {
								throw new LiblinearException("The instance file contain a non-integer value '"+items[k]+"'", e);
							}
						}
						offset += cardinalities[j-1];
					}
					problem.x[i] = xlist.subList(0, p).toArray(new FeatureNode[0]);
					if(columns.length > 1) {
						max_index = Math.max(max_index, problem.x[i][p-1].getIndex());
					}
					i++;
					xlist.clear();
				} catch (ArrayIndexOutOfBoundsException e) {
					throw new LiblinearException("Cannot read from the instance file. ", e);
				}
			}
			fp.close();	
			problem.n = max_index;
			System.out.println("Number of features: "+problem.n);
//			if ( problem.bias >= 0 ) {
//				problem.n++;
//			}
			xlist = null;
		} catch (IOException e) {
			throw new LiblinearException("Cannot read from the instance file. ", e);
		}
		return problem;
	}
	
	protected void initSpecialParameters() throws MaltChainedException {
		if (getConfiguration().getOptionValue("singlemalt", "null_value") != null && getConfiguration().getOptionValue("singlemalt", "null_value").toString().equalsIgnoreCase("none")) {
			excludeNullValues = true;
		} else {
			excludeNullValues = false;
		}
		saveInstanceFiles = ((Boolean)getConfiguration().getOptionValue("liblinear", "save_instance_files")).booleanValue();
//		featurePruning = ((Boolean)getConfiguration().getOptionValue("liblinear", "feature_pruning")).booleanValue();
		featurePruning = true;
		if (!getConfiguration().getOptionValue("liblinear", "liblinear_external").toString().equals("")) {
			try {
				if (!new File(getConfiguration().getOptionValue("liblinear", "liblinear_external").toString()).exists()) {
					throw new LiblinearException("The path to the external Liblinear trainer 'svm-train' is wrong.");
				}
				if (new File(getConfiguration().getOptionValue("liblinear", "liblinear_external").toString()).isDirectory()) {
					throw new LiblinearException("The option --liblinear-liblinear_external points to a directory, the path should point at the 'train' file or the 'train.exe' file");
				}
				if (!(getConfiguration().getOptionValue("liblinear", "liblinear_external").toString().endsWith("train") || getConfiguration().getOptionValue("liblinear", "liblinear_external").toString().endsWith("train.exe"))) {
					throw new LiblinearException("The option --liblinear-liblinear_external does not specify the path to 'train' file or the 'train.exe' file. ");
				}
				pathExternalLiblinearTrain = getConfiguration().getOptionValue("liblinear", "liblinear_external").toString();
			} catch (SecurityException e) {
				throw new LiblinearException("Access denied to the file specified by the option --liblinear-liblinear_external. ", e);
			}
		}
		if (getConfiguration().getOptionValue("liblinear", "verbosity") != null) {
			verbosity = Verbostity.valueOf(getConfiguration().getOptionValue("liblinear", "verbosity").toString().toUpperCase());
		}
	}
	
	public String getLibLinearOptions() {
		StringBuilder sb = new StringBuilder();
		for (String key : liblinearOptions.keySet()) {
			sb.append('-');
			sb.append(key);
			sb.append(' ');
			sb.append(liblinearOptions.get(key));
			sb.append(' ');
		}
		return sb.toString();
	}
	
	public void parseParameters(String paramstring) throws MaltChainedException {
		if (paramstring == null) {
			return;
		}
		final String[] argv;
		String allowedFlags = "sceB";
		try {
			argv = paramstring.split("[_\\p{Blank}]");
		} catch (PatternSyntaxException e) {
			throw new LiblinearException("Could not split the liblinear-parameter string '"+paramstring+"'. ", e);
		}
		for (int i=0; i < argv.length-1; i++) {
			if(argv[i].charAt(0) != '-') {
				throw new LiblinearException("The argument flag should start with the following character '-', not with "+argv[i].charAt(0));
			}
			if(++i>=argv.length) {
				throw new LiblinearException("The last argument does not have any value. ");
			}
			try {
				int index = allowedFlags.indexOf(argv[i-1].charAt(1));
				if (index != -1) {
					liblinearOptions.put(Character.toString(argv[i-1].charAt(1)), argv[i]);
				} else {
					throw new LiblinearException("Unknown liblinear parameter: '"+argv[i-1]+"' with value '"+argv[i]+"'. ");		
				}
			} catch (ArrayIndexOutOfBoundsException e) {
				throw new LiblinearException("The liblinear parameter '"+argv[i-1]+"' could not convert the string value '"+argv[i]+"' into a correct numeric value. ", e);
			} catch (NumberFormatException e) {
				throw new LiblinearException("The liblinear parameter '"+argv[i-1]+"' could not convert the string value '"+argv[i]+"' into a correct numeric value. ", e);	
			} catch (NullPointerException e) {
				throw new LiblinearException("The liblinear parameter '"+argv[i-1]+"' could not convert the string value '"+argv[i]+"' into a correct numeric value. ", e);	
			}
		}
	}
	
	public double getBias() throws MaltChainedException {
		try {
			return Double.valueOf(liblinearOptions.get("B")).doubleValue();
		} catch (NumberFormatException e) {
			throw new LiblinearException("The liblinear bias value is not numerical value. ", e);
		}
	}

	public Parameter getLiblinearParameters() throws MaltChainedException {
		Parameter param = new Parameter(SolverType.MCSVM_CS, 0.1, 0.1);
		String type = liblinearOptions.get("s");
		
		if (type.equals("0")) {
			param.setSolverType(SolverType.L2R_LR);
		} else if (type.equals("1")) {
			param.setSolverType(SolverType.L2R_L2LOSS_SVC_DUAL);
		} else if (type.equals("2")) {
			param.setSolverType(SolverType.L2R_L2LOSS_SVC);
		} else if (type.equals("3")) {
			param.setSolverType(SolverType.L2R_L1LOSS_SVC_DUAL);
		} else if (type.equals("4")) {
			param.setSolverType(SolverType.MCSVM_CS);
		} else if (type.equals("5")) {
			param.setSolverType(SolverType.L1R_L2LOSS_SVC);	
		} else if (type.equals("6")) {
			param.setSolverType(SolverType.L1R_LR);	
		} else {
			throw new LiblinearException("The liblinear type (-s) is not an integer value between 0 and 4. ");
		}
		try {
			param.setC(Double.valueOf(liblinearOptions.get("c")).doubleValue());
		} catch (NumberFormatException e) {
			throw new LiblinearException("The liblinear cost (-c) value is not numerical value. ", e);
		}
		try {
			param.setEps(Double.valueOf(liblinearOptions.get("e")).doubleValue());
		} catch (NumberFormatException e) {
			throw new LiblinearException("The liblinear epsilon (-e) value is not numerical value. ", e);
		}
		return param;
	}

	public void initLiblinearOptions() {
		liblinearOptions.put("s", "4"); // type = SolverType.L2LOSS_SVM_DUAL (default)
		liblinearOptions.put("c", "0.1"); // cost = 1 (default)
		liblinearOptions.put("e", "0.1"); // epsilon = 0.1 (default)
		liblinearOptions.put("B", "-1"); // bias = -1 (default)
	}

	public String[] getLibLinearParamStringArray() {
		final ArrayList<String> params = new ArrayList<String>();

		for (String key : liblinearOptions.keySet()) {
			params.add("-"+key); params.add(liblinearOptions.get(key));
		}
		return params.toArray(new String[params.size()]);
	}
	
	
	public void liblinear_predict_with_kbestlist(Model model, FeatureNode[] x, KBestList kBestList) throws MaltChainedException {
		int i;
		final int nr_class = model.getNrClass();
		final double[] dec_values = new double[nr_class];

		Linear.predictValues(model, x, dec_values);
		final int[] labels = model.getLabels();
		int[] predictionList = new int[nr_class];
		for(i=0;i<nr_class;i++) {
			predictionList[i] = labels[i];
		}

		double tmpDec;
		int tmpObj;
		int lagest;
		for (i=0;i<nr_class-1;i++) {
			lagest = i;
			for (int j=i;j<nr_class;j++) {
				if (dec_values[j] > dec_values[lagest]) {
					lagest = j;
				}
			}
			tmpDec = dec_values[lagest];
			dec_values[lagest] = dec_values[i];
			dec_values[i] = tmpDec;
			tmpObj = predictionList[lagest];
			predictionList[lagest] = predictionList[i];
			predictionList[i] = tmpObj;
		}
		
		int k = nr_class-1;
		if (kBestList.getK() != -1) {
			k = kBestList.getK() - 1;
		}
		
		for (i=0; i<nr_class && k >= 0; i++, k--) {
			if (kBestList instanceof ScoredKBestList) {
				((ScoredKBestList)kBestList).add(predictionList[i], (float)dec_values[i]);
			} else {
				kBestList.add(predictionList[i]);
			}

		}
	}
	
	/**
	 * Converts the instance file (Malt's own SVM format) into the Liblinear (SVMLight) format. The input instance file is removed (replaced)
	 * by the instance file in the Liblinear (SVMLight) format. If a column contains -1, the value will be removed in destination file. 
	 * 
	 * @param isr the input stream reader for the source instance file
	 * @param osw	the output stream writer for the destination instance file
	 * @param cardinalities a vector containing the number of distinct values for a particular column
	 * @throws LiblinearException
	 */
	public static void maltSVMFormat2OriginalSVMFormat(InputStreamReader isr, OutputStreamWriter osw, int[] cardinalities) throws MaltChainedException {
		try {
			final BufferedReader in = new BufferedReader(isr);
			final BufferedWriter out = new BufferedWriter(osw);

			int c;
			int j = 0;
			int offset = 1;
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
					offset = 1;
					out.write('\n');
					code = 0;
				} else if (c == '-') {
					code = -1;
				} else if (code != -1) {
					if (c > 47 && c < 58) {
						code = code * 10 + (c-48);
					} else {
						throw new LiblinearException("The instance file contain a non-integer value, when converting the Malt SVM format into Liblinear format.");
					}
				}	
			}			
			in.close();	
			out.close();
		} catch (IOException e) {
			throw new LiblinearException("Cannot read from the instance file, when converting the Malt SVM format into Liblinear format. ", e);
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
		sb.append("\nLiblinear INTERFACE\n");
		sb.append("  Liblinear version: "+LIBLINEAR_VERSION+"\n");
		sb.append("  Liblinear string: "+paramString+"\n");
		
		sb.append(getLibLinearOptions());
		return sb.toString();
	}
}
