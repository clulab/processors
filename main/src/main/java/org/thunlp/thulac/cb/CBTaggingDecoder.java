package org.thunlp.thulac.cb;

import org.thunlp.thulac.data.Dat;
import org.thunlp.thulac.data.POCGraph;
import org.thunlp.thulac.data.TaggedWord;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Vector;

public class CBTaggingDecoder {
	// TODO: add documentation

	private int maxLength;
	private int len;
	private String sequence;
	private int[][] allowedLabelLists;
	private int[][] pocsToTags;

	private CBNGramFeature nGramFeature;
	private Dat dat;

	private CBModel model;

	private Node[] nodes;
	private int[] values;
	private AlphaBeta[] alphas;
	private int[] result;

	private String[] labelInfo;

	private int[][] labelTransPre;
	private int[][] labelTransPost;

	public int threshold;

	public CBTaggingDecoder() {
		this.maxLength = 20000;
		this.len = 0;
		this.sequence = "";
		this.allowedLabelLists = new int[this.maxLength][];

		this.pocsToTags = null;
		this.nGramFeature = null;
		this.dat = null;
		this.nodes = new Node[this.maxLength];
		this.labelTransPre = null;
		this.labelTransPost = null;
		this.threshold = 0;

		this.model = null;
		this.alphas = null;
	}

	public void loadFiles(String modelFile, String datFile, String labelFile) throws
			IOException {
		this.model = new CBModel(modelFile);

		this.values = new int[this.maxLength * this.model.l_size];
		this.alphas = new AlphaBeta[this.maxLength * this.model.l_size];
		this.result = new int[this.maxLength * this.model.l_size];

		for (int i = 0; i < this.maxLength; i++) {
			this.nodes[i] = new Node();

			int[] pre = new int[2];
			pre[0] = i - 1;
			pre[1] = -1;
			this.nodes[i].predecessors = pre;

			pre = new int[2];
			pre[0] = i + 1;
			pre[1] = -1;
			this.nodes[i].successors = pre;
		}

		this.dat = new Dat(datFile);
		this.nGramFeature = new CBNGramFeature(this.dat, this.model, this.values);

		this.labelInfo = new String[10000];
		Vector<Vector<Integer>> pocTags = new Vector<>();
		for (int i = 0; i < 16; i++) pocTags.add(new Vector<>());
		BufferedReader in = new BufferedReader(
				new InputStreamReader(new FileInputStream(labelFile)));
		String line;
		int ind = 0;
		while ((line = in.readLine()) != null) {
			this.labelInfo[ind] = line;
			int segInd = line.charAt(0) - '0';
			for (int j = 0; j < 16; j++)
				if (((1 << segInd) & j) != 0) pocTags.get(j).add(ind);
			ind++;
		}
		in.close();

		this.pocsToTags = new int[16][];
		for (int j = 1; j < 16; j++) {
			this.pocsToTags[j] = new int[pocTags.get(j).size() + 1];
			for (int k = 0; k < pocTags.get(j).size(); k++)
				this.pocsToTags[j][k] = pocTags.get(j).get(k);
			this.pocsToTags[j][pocTags.get(j).size()] = -1;
		}

		int[][] labelLookingFor = new int[this.model.l_size][];
		for (int i = 0; i < this.model.l_size; i++) labelLookingFor[i] = null;
		for (int i = 0; i < this.model.l_size; i++) {
			if ("30".indexOf(this.labelInfo[i].charAt(0)) != -1) continue;
			for (int j = 0; j <= i; j++) {
				if ((this.labelInfo[i].substring(1).equals(
						this.labelInfo[j].substring(1))) && (this.labelInfo[j].charAt(
						0) == '0')) {
					if (labelLookingFor[j] == null) {
						labelLookingFor[j] = new int[2];
						labelLookingFor[j][0] = -1;
						labelLookingFor[j][1] = -1;
					}
					labelLookingFor[j][this.labelInfo[i].charAt(0) - '1'] = i;
					break;
				}
			}
		}


		for (int i = 0; i < this.maxLength; i++) this.allowedLabelLists[i] = null;
	}

	public void dp() {
		if (this.allowedLabelLists[0] == null)
			this.allowedLabelLists[0] = this.pocsToTags[9];
		if (this.allowedLabelLists[this.len - 1] == null)
			this.allowedLabelLists[this.len - 1] = this.pocsToTags[12];
		AlphaBeta.dbDecode(this.model.l_size, this.model.ll_weights,
				this.len, this.nodes, this.values, this.alphas, this.result,
				this.labelTransPre, this.allowedLabelLists);
		this.allowedLabelLists[0] = null;
		this.allowedLabelLists[this.len - 1] = null;
	}

	public void setLabelTrans() {
		int lSize = this.model.l_size;
		Vector<Vector<Integer>> preLabels = new Vector<>();
		Vector<Vector<Integer>> postLabels = new Vector<>();
		for (int i = 0; i < lSize; i++) {
			preLabels.add(new Vector<>());
			postLabels.add(new Vector<>());
		}
		for (int i = 0; i < lSize; i++) {
			for (int j = 0; j < lSize; j++) {
				int ni = this.labelInfo[i].charAt(0) - '0';
				int nj = this.labelInfo[j].charAt(0) - '0';
				boolean iIsEnd = ((ni == 2) || (ni == 3));
				boolean jIsBegin = ((nj == 0) || (nj == 3));
				boolean sameTag = this.labelInfo[i].substring(1)
						.equals(this.labelInfo[j].substring(1));
				if (sameTag) {
					if ((ni == 0 && nj == 1) ||
							(ni == 0 && nj == 2) ||
							(ni == 1 && nj == 2) ||
							(ni == 1 && nj == 1) ||
							(ni == 2 && nj == 0) ||
							(ni == 2 && nj == 3) ||
							(ni == 3 && nj == 3) ||
							(ni == 3 && nj == 0)) {
						preLabels.get(j).add(i);
						postLabels.get(i).add(j);
					}
				} else if (iIsEnd && jIsBegin) {
					preLabels.get(j).add(i);
					postLabels.get(i).add(j);
				}
			}
		}
		this.labelTransPre = new int[lSize][];
		for (int i = 0; i < lSize; i++) {
			this.labelTransPre[i] = new int[preLabels.get(i).size() + 1];
			for (int j = 0; j < preLabels.get(i).size(); j++) {
				this.labelTransPre[i][j] = preLabels.get(i).get(j);
			}
			this.labelTransPre[i][preLabels.get(i).size()] = -1;
		}

		this.labelTransPost = new int[lSize][];
		for (int i = 0; i < lSize; i++) {
			this.labelTransPost[i] = new int[postLabels.get(i).size() + 1];
			for (int j = 0; j < postLabels.get(i).size(); j++)
				this.labelTransPost[i][j] = postLabels.get(i).get(j);
			this.labelTransPost[i][postLabels.get(i).size()] = -1;
		}
	}

	public void putValues() {
		if (this.len == 0) return;
		for (int i = 0; i < this.len; i++) this.nodes[i].type = 0;
		this.nodes[0].type += 1;
		this.nodes[this.len - 1].type += 2;

		int size = this.len * this.model.l_size;
		for (int i = 0; i < size; i++) this.values[i] = 0;
		this.nGramFeature.putValues(this.sequence, this.len);
	}

	public boolean segment(String raw, POCGraph graph, List<TaggedWord> ts) {
		if (raw.length() == 0) return false;

		for (int i = 0; i < raw.length(); i++)
			this.allowedLabelLists[i] = this.pocsToTags[
					graph.get(i) == 0 ? 15 : graph.get(i)];
		this.sequence = "";
		for (int i = 0; i < raw.length(); i++) this.sequence += raw.charAt(i);
		this.len = raw.length();
		this.putValues(); // calculate eigenvalue and initialize and store them in values
		this.dp(); // DP search for the best answer and store it in result

		for (int i = 0; i < raw.length(); i++) this.allowedLabelLists[i] = null;
		int offset = 0;
		ts.clear();
		for (int i = 0; i < this.len; i++) {
			if ((i == this.len - 1) || (this.labelInfo[this.result[i]].charAt(
					0) == '2') || (this.labelInfo[this.result[i]].charAt(0) == '3')) {
				ts.add(new TaggedWord());
				for (int j = offset; j < i + 1; j++) {
					ts.get(ts.size() - 1).word += (this.sequence.charAt(j));
				}
				offset = i + 1; // output tags
				ts.get(ts.size() - 1).tag = this.labelInfo[this.result[i]].substring(
						1);
			}
		}
		return true;
	}
}
