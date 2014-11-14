/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.uci.jforestsx.input;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.dataset.NumericArrayFactory;
import edu.uci.jforestsx.dataset.NumericArrayFactory.NumericArrayType;
import edu.uci.jforestsx.util.Util;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class BinaryFileReader {

	BufferedInputStream input;
	private int version;

	private Feature[] features;
	private double[] targets;
	private int[] featureSizes;
	private NumericArrayType[] featureTypes;

	public BinaryFileReader(InputStream in) {
		try {
			input = new BufferedInputStream(in);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void read(byte[] buf, int offset, int len) {
		try {
			int toRead = len;
			// We have to read bytes in a loop.
			// Some file systems like s3n don't return the needed files
			// even with Buffered
			while (toRead > 0) {
				int ret = input.read(buf, offset, toRead);
				if (ret < 0) {
					throw new IOException("Premeture EOF from inputStream");
				}
				toRead -= ret;
				offset += ret;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void read(double[] arr) {
		for (int i = 0; i < arr.length; i++) {
			arr[i] = readDouble();
		}
	}

	protected int readInt() {
		byte[] buf = new byte[4];
		read(buf, 0, 4);
		return Util.toInt(buf, 0);
	}

	protected double readDouble() {
		byte[] buf = new byte[8];
		read(buf, 0, 8);
		return Util.toDouble(buf, 0);
	}

	private byte readByte() {
		byte[] buf = new byte[1];
		read(buf, 0, 1);
		return buf[0];
	}

	private Feature readFeature(int featureSize, NumericArrayType type) throws Exception {
		byte[] buf = new byte[featureSize];
		read(buf, 0, featureSize);
		Feature feature = new Feature();
		feature.bins = NumericArrayFactory.createNumericArray(type, targets.length);
		feature.loadFromByteArray(buf, 0);
		return feature;
	}

	public void close() {
		try {
			input.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	protected void readHeader() throws Exception {
		version = readInt();
		if (version != BinaryFileWriter.VERSION) {
			throw new Exception("Expected bin file version: " + BinaryFileWriter.VERSION + ", found: " + version);
		}
		features = new Feature[readInt()];
		targets = new double[readInt()];
		featureSizes = new int[features.length];
		for (int f = 0; f < features.length; f++) {
			featureSizes[f] = readInt();
		}
		featureTypes = new NumericArrayType[features.length];
		for (int f = 0; f < features.length; f++) {
			featureTypes[f] = NumericArrayType.getFromOrdinal(readByte());
		}
	}

	public void read() throws Exception {
		readHeader();
		read(targets);
		for (int f = 0; f < features.length; f++) {
			features[f] = readFeature(featureSizes[f], featureTypes[f]);
		}
	}

	public int getVersion() {
		return version;
	}

	public Feature[] getFeatures() {
		return features;
	}

	public double[] getTargets() {
		return targets;
	}
}
