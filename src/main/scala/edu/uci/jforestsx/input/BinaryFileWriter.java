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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import edu.uci.jforestsx.dataset.Feature;
import edu.uci.jforestsx.util.Util;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class BinaryFileWriter {

	/*
	 * This version in incremented by 1, each time the structure of the bin file
	 * is changed. This is to ensure that we don't try to read bin files with old
	 * structures.
	 */
	public final static int VERSION = 3;

	BufferedOutputStream output;
	
	private Feature[] features;
	private double[] targets;

	public BinaryFileWriter(String binFileName, Feature[] features, double[] targets) {
		try {
			File file = new File(binFileName);
            file.delete();
            output = new BufferedOutputStream(new FileOutputStream(file));
            
            this.features = features;
            this.targets = targets;
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private void write(byte[] arr) {
		try {
			output.write(arr);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private void write(byte b) {
		try {
			output.write(b);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	protected void writeInt(int value) {
		write(Util.toByteArray(value));
	}
	
	private void write(double[] arr) {
		byte[] buf = new byte[8];			
		for (int i = 0; i<arr.length; i++) {
			Util.putDoubleInByteArray(arr[i], buf, 0);
			write(buf);
		}	
	}
	
	private void writeFeature(Feature feature) {
		byte[] buf = new byte[feature.getSizeInBytes()];
		feature.toByteArray(buf, 0);
		write(buf);
	}
	
	public void close() {
		try {
			output.close();
		} catch (IOException e) {			
			e.printStackTrace();
		}
	}
	
	protected void writeHeader() {
		writeInt(VERSION);
		writeInt(features.length);
		writeInt(targets.length);
		for (int f = 0; f < features.length; f++) {
			writeInt(features[f].getSizeInBytes());
		}
		for (int f = 0; f < features.length; f++) {
			write((byte) features[f].getType().ordinal());
		}
	}
	
	public void write() {
		writeHeader();
		write(targets);
		for (int f = 0; f < features.length; f++) {
			writeFeature(features[f]);
		}		
	}
}
