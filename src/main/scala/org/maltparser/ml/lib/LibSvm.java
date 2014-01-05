package org.maltparser.ml.lib;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintStream;

import java.util.LinkedHashMap;

import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.feature.FeatureVector;
import org.maltparser.core.helper.NoPrintStream;
import org.maltparser.parser.guide.instance.InstanceModel;


import libsvm.svm;
import libsvm.svm_model;
import libsvm.svm_node;
import libsvm.svm_parameter;
import libsvm.svm_problem;

public class LibSvm extends Lib {

	public LibSvm(InstanceModel owner, Integer learnerMode) throws MaltChainedException {
		super(owner, learnerMode, "libsvm");
		if (learnerMode == CLASSIFY) {
			try {
			    ObjectInputStream input = new ObjectInputStream(getInputStreamFromConfigFileEntry(".moo"));
			    try {
			    	model = (MaltLibModel)input.readObject();
			    } finally {
			    	input.close();
			    }
			} catch (ClassNotFoundException e) {
				throw new LibException("Couldn't load the liblinear model", e);
			} catch (Exception e) {
				throw new LibException("Couldn't load the liblinear model", e);
			}
		}
	}
	
	protected void trainInternal(FeatureVector featureVector) throws MaltChainedException {
		try {
			final svm_problem prob = readProblem(getInstanceInputStreamReader(".ins"));
			final svm_parameter param = getLibSvmParameters();
			if(svm.svm_check_parameter(prob, param) != null) {
				throw new LibException(svm.svm_check_parameter(prob, param));
			}
			owner.getGuide().getConfiguration().getConfigLogger().info("Creating LIBSVM model "+getFile(".moo").getName()+"\n");
			final PrintStream out = System.out;
			final PrintStream err = System.err;
			System.setOut(NoPrintStream.NO_PRINTSTREAM);
			System.setErr(NoPrintStream.NO_PRINTSTREAM);
			svm_model model = svm.svm_train(prob, param);
			System.setOut(err);
			System.setOut(out);
		    ObjectOutputStream output = new ObjectOutputStream (new BufferedOutputStream(new FileOutputStream(getFile(".moo").getAbsolutePath())));
	        try{
	          output.writeObject(new MaltLibsvmModel(model, prob));
	        } finally {
	          output.close();
	        }
			if (!saveInstanceFiles) {
				getFile(".ins").delete();
			}
		} catch (OutOfMemoryError e) {
			throw new LibException("Out of memory. Please increase the Java heap size (-Xmx<size>). ", e);
		} catch (IllegalArgumentException e) {
			throw new LibException("The LIBSVM learner was not able to redirect Standard Error stream. ", e);
		} catch (SecurityException e) {
			throw new LibException("The LIBSVM learner cannot remove the instance file. ", e);
		} catch (IOException e) {
			throw new LibException("The LIBSVM learner cannot save the model file '"+getFile(".mod").getAbsolutePath()+"'. ", e);
		}
	}
	
	protected void trainExternal(FeatureVector featureVector) throws MaltChainedException {
		try {		
			binariesInstances2SVMFileFormat(getInstanceInputStreamReader(".ins"), getInstanceOutputStreamWriter(".ins.tmp"));
			owner.getGuide().getConfiguration().getConfigLogger().info("Creating learner model (external) "+getFile(".mod").getName());
			final svm_problem prob = readProblem(getInstanceInputStreamReader(".ins"));
			final String[] params = getLibParamStringArray();
			String[] arrayCommands = new String[params.length+3];
			int i = 0;
			arrayCommands[i++] = pathExternalTrain;
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
	        svm_model model = svm.svm_load_model(getFile(".mod").getAbsolutePath());
	        MaltLibsvmModel xmodel = new MaltLibsvmModel(model, prob);
	        ObjectOutputStream output = new ObjectOutputStream (new BufferedOutputStream(new FileOutputStream(getFile(".moo").getAbsolutePath())));
	        try {
	        	output.writeObject(xmodel);
		    } finally {
		    	output.close();
		    }
	        if (!saveInstanceFiles) {
				getFile(".ins").delete();
				getFile(".mod").delete();
				getFile(".ins.tmp").delete();
	        }
	        owner.getGuide().getConfiguration().getConfigLogger().info('\n');
		} catch (InterruptedException e) {
			 throw new LibException("Learner is interrupted. ", e);
		} catch (IllegalArgumentException e) {
			throw new LibException("The learner was not able to redirect Standard Error stream. ", e);
		} catch (SecurityException e) {
			throw new LibException("The learner cannot remove the instance file. ", e);
		} catch (IOException e) {
			throw new LibException("The learner cannot save the model file '"+getFile(".mod").getAbsolutePath()+"'. ", e);
		} catch (OutOfMemoryError e) {
			throw new LibException("Out of memory. Please increase the Java heap size (-Xmx<size>). ", e);
		}
	}
	
	public void terminate() throws MaltChainedException { 
		super.terminate();
	}
	
	public void initLibOptions() {
		libOptions = new LinkedHashMap<String, String>();
		libOptions.put("s", Integer.toString(svm_parameter.C_SVC));
		libOptions.put("t", Integer.toString(svm_parameter.POLY));
		libOptions.put("d", Integer.toString(2));
		libOptions.put("g", Double.toString(0.2));
		libOptions.put("r", Double.toString(0));
		libOptions.put("n", Double.toString(0.5));
		libOptions.put("m", Integer.toString(100));
		libOptions.put("c", Double.toString(1));
		libOptions.put("e", Double.toString(1.0));
		libOptions.put("p", Double.toString(0.1));
		libOptions.put("h", Integer.toString(1));
		libOptions.put("b", Integer.toString(0));
	}
	
	public void initAllowedLibOptionFlags() {
		allowedLibOptionFlags = "stdgrnmcepb";
	}
	
	private svm_parameter getLibSvmParameters() throws MaltChainedException {
		svm_parameter param = new svm_parameter();
	
		param.svm_type = Integer.parseInt(libOptions.get("s"));
		param.kernel_type = Integer.parseInt(libOptions.get("t"));
		param.degree = Integer.parseInt(libOptions.get("d"));
		param.gamma = Double.valueOf(libOptions.get("g")).doubleValue();
		param.coef0 = Double.valueOf(libOptions.get("r")).doubleValue();
		param.nu = Double.valueOf(libOptions.get("n")).doubleValue();
		param.cache_size = Double.valueOf(libOptions.get("m")).doubleValue();
		param.C = Double.valueOf(libOptions.get("c")).doubleValue();
		param.eps = Double.valueOf(libOptions.get("e")).doubleValue();
		param.p = Double.valueOf(libOptions.get("p")).doubleValue();
		param.shrinking = Integer.parseInt(libOptions.get("h"));
		param.probability = Integer.parseInt(libOptions.get("b"));
		param.nr_weight = 0;
		param.weight_label = new int[0];
		param.weight = new double[0];
		return param;
	}
	
	private svm_problem readProblem(InputStreamReader isr) throws MaltChainedException {
		final svm_problem problem = new svm_problem();
		final svm_parameter param = getLibSvmParameters();
		final FeatureList featureList = new FeatureList();
		try {
			final BufferedReader fp = new BufferedReader(isr);
			
			problem.l = getNumberOfInstances();
			problem.x = new svm_node[problem.l][];
			problem.y = new double[problem.l];
			int i = 0;
			
			while(true) {
				String line = fp.readLine();
				if(line == null) break;
				int y = binariesInstance(line, featureList);
				if (y == -1) {
					continue;
				}
				try {
					problem.y[i] = y;
					problem.x[i] = new svm_node[featureList.size()];
					int p = 0;
			        for (int k=0; k < featureList.size(); k++) {
			        	MaltFeatureNode x = featureList.get(k);
						problem.x[i][p] = new svm_node();
						problem.x[i][p].value = x.getValue();
						problem.x[i][p].index = x.getIndex();          
						p++;
					}
					i++;
				} catch (ArrayIndexOutOfBoundsException e) {
					throw new LibException("Couldn't read libsvm problem from the instance file. ", e);
				}
			}
			fp.close();	
			if (param.gamma == 0) {
				param.gamma = 1.0/featureMap.getFeatureCounter();
			}
		} catch (IOException e) {
			throw new LibException("Couldn't read libsvm problem from the instance file. ", e);
		}
		return problem;
	}
}
