package org.thunlp.thulac.cb;


// a structure for alphas and betas
public class AlphaBeta {
	// TODO: add documentation

	public int value;
	public int nodeId;
	public int labelId;

	public AlphaBeta() {
		super();
		this.value = 0;
		this.nodeId = -2;
		this.labelId = 0;
	}

	public AlphaBeta(int value, int nodeId, int labelId) {
		super();
		this.value = value;
		this.nodeId = nodeId;
		this.labelId = labelId;
	}


	public static int dbDecode(
			int l_size, int[] llWeights, int nodeCount, Node[] nodes, int[] values,
			AlphaBeta[] alphas,
			int[] result, int[][] preLabels, int[][] allowedLabelLists) {
		int nodeId;
		int[] pNodeId;
		int[] pPreLabel;
		int[] pAllowedLabel;
		int k;
		int j;
		AlphaBeta tmp;
		AlphaBeta best = new AlphaBeta();
		best.nodeId = -1;
		AlphaBeta preAlpha;

		int score;
		int index = 0;
		int index2 = 0;
		int index3 = 0;

		for (int i = 0; i < nodeCount * l_size; i++) {
			alphas[i] = new AlphaBeta();
			alphas[i].nodeId = -2;
		}
		for (int i = 0; i < nodeCount; i++) {
			pAllowedLabel = allowedLabelLists != null ? allowedLabelLists[i] : null;
			j = -1;
			int maxValue = 0;
			boolean hasMaxValue = false;
			if (pAllowedLabel != null) {
				index = 0;
				while ((j = pAllowedLabel[index]) != -1) {
					index++;
					if (!hasMaxValue || (maxValue < values[i * l_size + j])) {
						hasMaxValue = true;
						maxValue = values[i * l_size + j];
					}
				}
				index = 0;
				j = -1;
				while ((j = pAllowedLabel[index]) != -1) {
					index++;
					tmp = alphas[i * l_size + j];
					tmp.value = 0;
					pNodeId = nodes[i].predecessors;
					pPreLabel = preLabels != null ? preLabels[j] : null;
					index2 = 0;
					while ((nodeId = pNodeId[index2]) >= 0) {
						index2++;
						k = -1;
						if (pPreLabel != null) {
							index3 = 0;
							while ((k = pPreLabel[index3]) != -1) {
								index3++;
								preAlpha = alphas[nodeId * l_size + k];
								if (preAlpha.nodeId == -2) continue;
								score = preAlpha.value + llWeights[k * l_size + j];
								if ((tmp.nodeId < 0) || (score > tmp.value)) {
									tmp.value = score;
									tmp.nodeId = nodeId;
									tmp.labelId = k;
								}
							}
						} else {
							k++;
							while (k != l_size) {
								preAlpha = alphas[nodeId * l_size + k];
								if (preAlpha.nodeId == -2) continue;
								score = preAlpha.value + llWeights[k * l_size + j];
								if ((tmp.nodeId < 0) || (score > tmp.value)) {
									tmp.value = score;
									tmp.nodeId = nodeId;
									tmp.labelId = k;
								}
								k++;
							}
						}
					}
					tmp.value += values[i * l_size + j];
					if ((nodes[i].type == 1) || (nodes[i].type == 3)) {
						tmp.nodeId = -1;
					}
					if (nodes[i].type >= 2) {
						if ((best.nodeId == -1) || best.value < tmp.value) {
							best.value = tmp.value;
							best.nodeId = i;
							best.labelId = j;
						}
					}
				}

			} else {
				j++;
				while (j != l_size) {
					if (!hasMaxValue || (maxValue < values[i * l_size + j])) {
						hasMaxValue = true;
						maxValue = values[i * l_size + j];
					}
					j++;
				}
				j = 0;
				while (j != l_size) {
					tmp = alphas[i * l_size + j];
					tmp.value = 0;
					pNodeId = nodes[i].predecessors;
					pPreLabel = preLabels != null ? preLabels[j] : null;
					index2 = 0;
					while ((nodeId = pNodeId[index2]) >= 0) {
						index2++;
						k = -1;
						if (pPreLabel != null) {
							index3 = 0;
							while ((k = pPreLabel[index3]) != -1) {
								index3++;
								preAlpha = alphas[nodeId * l_size + k];
								if (preAlpha.nodeId == -2) continue;
								score = preAlpha.value + llWeights[k * l_size + j];
								if ((tmp.nodeId < 0) || (score > tmp.value)) {
									tmp.value = score;
									tmp.nodeId = nodeId;
									tmp.labelId = k;
								}

							}
						} else {
							k++;
							while (k != l_size) {
								preAlpha = alphas[nodeId * l_size + k];
								if (preAlpha.nodeId == -2) continue;
								score = preAlpha.value + llWeights[k * l_size + j];
								if ((tmp.nodeId < 0) || (score > tmp.value)) {
									tmp.value = score;
									tmp.nodeId = nodeId;
									tmp.labelId = k;
								}
								k++;
							}
						}
					}
					tmp.value += values[i * l_size + j];
					if ((nodes[i].type == 1) || (nodes[i].type == 3)) {
						tmp.nodeId = -1;
					}
					if (nodes[i].type >= 2) {
						if ((best.nodeId == -1) || best.value < tmp.value) {
							best.value = tmp.value;
							best.nodeId = i;
							best.labelId = j;
						}
					}
//					System.out.println(""+tmp.value+" "+tmp.nodeId+" "+tmp.labelId);
					j++;
				}

			}
		}
		tmp = best;
		while (tmp.nodeId >= 0) {
			result[tmp.nodeId] = tmp.labelId;
			tmp = alphas[tmp.nodeId * l_size + tmp.labelId];
		}
		return best.value;
	}
}