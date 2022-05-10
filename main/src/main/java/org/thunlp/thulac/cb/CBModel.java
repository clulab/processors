package org.thunlp.thulac.cb;

import org.thunlp.thulac.util.BufferUtils;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.nio.channels.FileChannel;

public class CBModel {
	// TODO: add documentation

	public int l_size; // size of the labels
	public int f_size; // size of the features

	public int[] ll_weights; // weights of (label, label)
	public int[] fl_weights; // weights of (feature, label)

	public CBModel(String filename) throws IOException {
		FileInputStream in = new FileInputStream(filename);
		FileChannel channel = in.getChannel();

		ByteBuffer header = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN);
		header.clear();
		channel.read(header);
		header.flip();
		IntBuffer intHeader = header.asIntBuffer();
		this.l_size = intHeader.get();
		this.f_size = intHeader.get();

		int llSize = this.l_size * this.l_size, flSize = this.l_size * this.f_size;
		this.ll_weights = new int[llSize];
		this.fl_weights = new int[flSize];
		ByteBuffer buf = ByteBuffer.allocate(64 * 1024).order(ByteOrder.LITTLE_ENDIAN);
		buf.clear();
		BufferUtils.readInts(channel, buf, this.ll_weights, this.fl_weights);

		channel.close();
	}
}
