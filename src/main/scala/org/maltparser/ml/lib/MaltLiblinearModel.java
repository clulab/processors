package org.maltparser.ml.lib;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Reader;
import java.io.Serializable;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.regex.Pattern;

import org.maltparser.core.helper.Util;

import de.bwaldvogel.liblinear.SolverType;

/**
 * <p>This class borrows code from liblinear.Model.java of the Java implementation of the liblinear package.
 * MaltLiblinearModel stores the model obtained from the training procedure. In addition to the original code the model is more integrated to
 * MaltParser. Instead of moving features from MaltParser's internal data structures to liblinear's data structure it uses MaltParser's data 
 * structure directly on the model. </p> 
 * 
 * @author Johan Hall
 *
 */
public class MaltLiblinearModel implements Serializable, MaltLibModel {
	private static final long serialVersionUID = 7526471155622776147L;
	private static final Charset FILE_CHARSET = Charset.forName("ISO-8859-1");
	private double bias;
	/** label of each class */
	private int[] labels;
	private int nr_class;
	private int nr_feature;
	private SolverType solverType;
	/** feature weight array */
	private double[][] w;

    public MaltLiblinearModel(int[] labels, int nr_class, int nr_feature, double[][] w, SolverType solverType) {
    	this.labels = labels;
    	this.nr_class = nr_class;
    	this.nr_feature = nr_feature;
    	this.w = w;
    	this.solverType = solverType;	
    }
    
    public MaltLiblinearModel(Reader inputReader) throws IOException {
    	loadModel(inputReader);
    }
    
    public MaltLiblinearModel(File modelFile) throws IOException {
        BufferedReader inputReader = new BufferedReader(new InputStreamReader(new FileInputStream(modelFile), FILE_CHARSET));
        loadModel(inputReader);
    }
    
    /**
    * @return number of classes
    */
    public int getNrClass() {
        return nr_class;
    }

    /**
    * @return number of features
    */
    public int getNrFeature() {
        return nr_feature;
    }

    public int[] getLabels() {
        return Util.copyOf(labels, nr_class);
    }

    /**
    * The nr_feature*nr_class array w gives feature weights. We use one
    * against the rest for multi-class classification, so each feature
    * index corresponds to nr_class weight values. Weights are
    * organized in the following way
    *
    * <pre>
    * +------------------+------------------+------------+
    * | nr_class weights | nr_class weights | ...
    * | for 1st feature | for 2nd feature |
    * +------------------+------------------+------------+
    * </pre>
    *
    * If bias &gt;= 0, x becomes [x; bias]. The number of features is
    * increased by one, so w is a (nr_feature+1)*nr_class array. The
    * value of bias is stored in the variable bias.
    * @see #getBias()
    * @return a <b>copy of</b> the feature weight array as described
    */
//    public double[] getFeatureWeights() {
//        return Util.copyOf(w, w.length);
//    }

    /**
    * @return true for logistic regression solvers
    */
    public boolean isProbabilityModel() {
        return (solverType == SolverType.L2R_LR || solverType == SolverType.L2R_LR_DUAL || solverType == SolverType.L1R_LR);
    }
    
    public double getBias() {
        return bias;
    }
        
    public int[] predict(MaltFeatureNode[] x) { 
		final double[] dec_values = new double[nr_class];
		final int[] predictionList = Util.copyOf(labels, nr_class); 
        final int n = (bias >= 0)?nr_feature + 1:nr_feature;
//        final int nr_w = (nr_class == 2 && solverType != SolverType.MCSVM_CS)?1:nr_class;
        final int xlen = x.length;
//        int i;
//        for (i = 0; i < nr_w; i++) {
//            dec_values[i] = 0;   
//        }
        
        for (int i=0; i < xlen; i++) {
            if (x[i].index <= n) {
            	final int t = (x[i].index - 1);
            	if (w[t] != null) {
	                for (int j = 0; j < w[t].length; j++) {
	                    dec_values[j] += w[t][j] * x[i].value;
	                }
            	}
            }
        }

		
		double tmpDec;
		int tmpObj;
		int lagest;
		final int nc =  nr_class-1;
		for (int i=0; i < nc; i++) {
			lagest = i;
			for (int j=i; j < nr_class; j++) {
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
		return predictionList;
	}
	
	private void readObject(ObjectInputStream is) throws ClassNotFoundException, IOException {
		is.defaultReadObject();
	}

	private void writeObject(ObjectOutputStream os) throws IOException {
		os.defaultWriteObject();
	}
	
	private void loadModel(Reader inputReader) throws IOException {
		labels = null;
		Pattern whitespace = Pattern.compile("\\s+");
        BufferedReader reader = null;
        if (inputReader instanceof BufferedReader) {
            reader = (BufferedReader)inputReader;
        } else {
            reader = new BufferedReader(inputReader);
        }

        try {
            String line = null;
            while ((line = reader.readLine()) != null) {
                String[] split = whitespace.split(line);
                if (split[0].equals("solver_type")) {
                    SolverType solver = SolverType.valueOf(split[1]);
                    if (solver == null) {
                        throw new RuntimeException("unknown solver type");
                    }
                    solverType = solver;
                } else if (split[0].equals("nr_class")) {
                    nr_class = Util.atoi(split[1]);
                    Integer.parseInt(split[1]);
                } else if (split[0].equals("nr_feature")) {
                    nr_feature = Util.atoi(split[1]);
                } else if (split[0].equals("bias")) {
                    bias = Util.atof(split[1]);
                } else if (split[0].equals("w")) {
                    break;
                } else if (split[0].equals("label")) {
                    labels = new int[nr_class];
                    for (int i = 0; i < nr_class; i++) {
                        labels[i] = Util.atoi(split[i + 1]);
                    }
                } else {
                    throw new RuntimeException("unknown text in model file: [" + line + "]");
                }
            }

            int w_size = nr_feature;
            if (bias >= 0) w_size++;

            int nr_w = nr_class;
            if (nr_class == 2 && solverType != SolverType.MCSVM_CS) nr_w = 1;
            w = new double[w_size][nr_w];
            int[] buffer = new int[128];

            for (int i = 0; i < w_size; i++) {
                for (int j = 0; j < nr_w; j++) {
                    int b = 0;
                    while (true) {
                        int ch = reader.read();
                        if (ch == -1) {
                            throw new EOFException("unexpected EOF");
                        }
                        if (ch == ' ') {
                        	w[i][j] = Util.atof(new String(buffer, 0, b));
                            break;
                        } else {
                            buffer[b++] = ch;
                        }
                    }
                }
            }
        }
        finally {
            Util.closeQuietly(reader);
        }
	}

    public int hashCode() {
        final int prime = 31;
        long temp = Double.doubleToLongBits(bias);
        int result = prime * 1 + (int)(temp ^ (temp >>> 32));
        result = prime * result + Arrays.hashCode(labels);
        result = prime * result + nr_class;
        result = prime * result + nr_feature;
        result = prime * result + ((solverType == null) ? 0 : solverType.hashCode());
        for (int i = 0; i < w.length; i++) {
        	result = prime * result + Arrays.hashCode(w[i]);
        }
        return result;
    }

    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        MaltLiblinearModel other = (MaltLiblinearModel)obj;
        if (Double.doubleToLongBits(bias) != Double.doubleToLongBits(other.bias)) return false;
        if (!Arrays.equals(labels, other.labels)) return false;
        if (nr_class != other.nr_class) return false;
        if (nr_feature != other.nr_feature) return false;
        if (solverType == null) {
            if (other.solverType != null) return false;
        } else if (!solverType.equals(other.solverType)) return false;
        for (int i = 0; i < w.length; i++) {
        	if (other.w.length <= i) return false;
        	if (!Util.equals(w[i], other.w[i])) return false;
        }    
        return true;
    }
    
    public String toString() {
        final StringBuilder sb = new StringBuilder("Model");
        sb.append(" bias=").append(bias);
        sb.append(" nr_class=").append(nr_class);
        sb.append(" nr_feature=").append(nr_feature);
        sb.append(" solverType=").append(solverType);
        return sb.toString();
    }
}
