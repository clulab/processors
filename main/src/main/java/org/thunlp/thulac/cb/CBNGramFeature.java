package org.thunlp.thulac.cb;

import org.thunlp.thulac.data.Dat;

import java.util.Vector;

public class CBNGramFeature {
	// TODO: add documentation

	private static final int SENTENCE_BOUNDARY = '#';

	private int separator;
	private int maxLength;
	private int[] uniBases;
	private int[] biBases;
	private int[] values;
	private int datSize;
	private int[] dat;
	private CBModel model;

	public CBNGramFeature(Dat myDat, CBModel model, int[] values) {
		this.separator = ' ';
		this.datSize = myDat.datSize;
		this.dat = myDat.dat;
		this.model = model;
		this.maxLength = 20000;
		this.uniBases = new int[this.maxLength + 2];
		this.biBases = new int[this.maxLength + 4];
		this.values = values;
	}

	private void addValues(int valueOffset, int base, int del) {
		int ind = this.dat[base << 1] + del;
		if (ind >= this.datSize || this.dat[(ind << 1) + 1] != base) return;
		int offset = this.dat[ind << 1];
		int weightOffset = offset * this.model.l_size;
		if (this.model.l_size == 4) {
			this.values[valueOffset] += this.model.fl_weights[weightOffset];
			this.values[valueOffset + 1] += this.model.fl_weights[weightOffset + 1];
			this.values[valueOffset + 2] += this.model.fl_weights[weightOffset + 2];
			this.values[valueOffset + 3] += this.model.fl_weights[weightOffset + 3];
		} else for (int i = 0; i < this.model.l_size; i++) {
			this.values[valueOffset + i] += this.model.fl_weights[weightOffset + i];
		}
	}

	private Vector<Integer> findBases(int datSize, int ch1, int ch2) {
		Vector<Integer> result = new Vector<>();
		int uniBase;
		int biBase;
		if (ch1 > 32 && ch1 < 128) ch1 += 65248;
		if (ch2 > 32 && ch2 < 128) ch2 += 65248;
		if (ch1 >= datSize || this.dat[(ch1 << 1) + 1] != 0) {
			uniBase = -1;
			biBase = -1;
			result.clear();
			result.add(uniBase);
			result.add(biBase);
			return result;
		}
		uniBase = this.dat[ch1 << 1] + this.separator;
		int ind = this.dat[ch1 << 1] + ch2;
		if (ind >= datSize || this.dat[(ind << 1) + 1] != ch1) {
			biBase = -1;
			result.clear();
			result.add(uniBase);
			result.add(biBase);
			return result;
		}
		biBase = this.dat[ind << 1] + this.separator;
		result.clear();
		result.add(uniBase);
		result.add(biBase);
		return result;
	}

	public int putValues(String sequence, int len) {
		if (len >= this.maxLength) {
			System.err.println("Length larger than maxLength.");
			return 1;
		}

		Vector<Integer> result = this.findBases(this.datSize, SENTENCE_BOUNDARY,
				SENTENCE_BOUNDARY);
		this.uniBases[0] = result.get(0);
		this.biBases[0] = result.get(1);

		result = this.findBases(this.datSize, SENTENCE_BOUNDARY, sequence.charAt(0));
		this.uniBases[0] = result.get(0);
		this.biBases[1] = result.get(1);
		for (int i = 0; i + 1 < len; i++) {
			result = this.findBases(this.datSize, sequence.charAt(i),
					sequence.charAt(i + 1));
			this.uniBases[i + 1] = result.get(0);
			this.biBases[i + 2] = result.get(1);
		}

		result = this.findBases(this.datSize, (int) sequence.charAt(len - 1),
				SENTENCE_BOUNDARY);
		this.uniBases[len] = result.get(0);
		this.biBases[len + 1] = result.get(1);

		result = this.findBases(this.datSize, SENTENCE_BOUNDARY, SENTENCE_BOUNDARY);
		this.uniBases[len + 1] = result.get(0);
		this.biBases[len + 2] = result.get(1);

		int base;
		for (int i = 0; i < len; i++) {
			int valueOffset = i * this.model.l_size;
			if ((base = this.uniBases[i + 1]) != -1) {
				this.addValues(valueOffset, base, 49);
			}
			if ((base = this.uniBases[i]) != -1) {
				this.addValues(valueOffset, base, 50);
			}
			if ((base = this.uniBases[i + 2]) != -1) {
				this.addValues(valueOffset, base, 51);
			}
			if ((base = this.biBases[i + 1]) != -1) {
				this.addValues(valueOffset, base, 49);
			}
			if ((base = this.biBases[i + 2]) != -1) {
				this.addValues(valueOffset, base, 50);
			}
			if ((base = this.biBases[i]) != -1) {
				this.addValues(valueOffset, base, 51);
			}
			if ((base = this.biBases[i + 3]) != -1) {
				this.addValues(valueOffset, base, 52);
			}
		}
		return 0;
	}
}
