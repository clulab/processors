package org.maltparserx.ml.lib;

public interface MaltLibModel {
	public int[] predict(MaltFeatureNode[] x);
}
