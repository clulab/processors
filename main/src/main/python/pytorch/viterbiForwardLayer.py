from pytorch.forwardLayer import *
from pytorch.utils import *

class ViterbiForwardLayer(ForwardLayer):
    def __init__(self, inputSize, isDual, t2i, i2t, actualInputSize, nonlinearity, dropoutProb, spans = None):
        super().__init__(inputSize, isDual, t2i, i2t, actualInputSize, nonlinearity, dropoutProb, spans)

        # Matrix of transition parameters.  Entry i,j is the score of
        # transitioning *to* i *from* j.
        self.transitions = nn.Parameter(
            torch.randn(self.outDim, self.outDim))

        # These two statements enforce the constraint that we never transfer
        # to the start tag and we never transfer from the stop tag
        self.transitions.data[t2i[START_TAG], :] = -10000
        self.transitions.data[:, t2i[STOP_TAG]] = -10000

    def _forward_alg(self, feats):
        # Do the forward algorithm to compute the partition function
        init_alphas = torch.full((1, self.outDim), -10000.)
        # START_TAG has all of the score.
        init_alphas[0][self.t2i[START_TAG]] = 0.

        # Wrap in a variable so that we will get automatic backprop
        forward_var = init_alphas

        # Iterate through the sentence
        for feat in feats:
            alphas_t = []  # The forward tensors at this timestep
            for next_tag in range(self.outDim):
                # broadcast the emission score: it is the same regardless of
                # the previous tag
                emit_score = feat[next_tag].view(
                    1, -1).expand(1, self.outDim)
                # the ith entry of trans_score is the score of transitioning to
                # next_tag from i
                trans_score = self.transitions[next_tag].view(1, -1)
                # The ith entry of next_tag_var is the value for the
                # edge (i -> next_tag) before we do log-sum-exp
                next_tag_var = forward_var + trans_score + emit_score
                # The forward variable for this tag is log-sum-exp of all the
                # scores.
                alphas_t.append(log_sum_exp(next_tag_var).view(1))
            forward_var = torch.cat(alphas_t).view(1, -1)
        terminal_var = forward_var + self.transitions[self.t2i[STOP_TAG]]
        alpha = log_sum_exp(terminal_var)
        return alpha

    def _score_sentence(self, feats, tags):
        # Gives the score of a provided tag sequence
        score = torch.zeros(1)
        tags = torch.cat([torch.tensor([self.t2i[START_TAG]], dtype=torch.long), tags])
        for i, feat in enumerate(feats):
            score = score + \
                self.transitions[tags[i + 1], tags[i]] + feat[tags[i + 1]]
        score = score + self.transitions[self.t2i[STOP_TAG], tags[-1]]
        return score

    def _viterbi_decode(self, feats):
        backpointers = []

        # Initialize the viterbi variables in log space
        init_vvars = torch.full((1, self.outDim), -10000.)
        init_vvars[0][self.t2i[START_TAG]] = 0

        # forward_var at step i holds the viterbi variables for step i-1
        forward_var = init_vvars
        for feat in feats:
            bptrs_t = []  # holds the backpointers for this step
            viterbivars_t = []  # holds the viterbi variables for this step

            for next_tag in range(self.outDim):
                # next_tag_var[i] holds the viterbi variable for tag i at the
                # previous step, plus the score of transitioning
                # from tag i to next_tag.
                # We don't include the emission scores here because the max
                # does not depend on them (we add them in below)
                next_tag_var = forward_var + self.transitions[next_tag]
                best_tag_id = argmax(next_tag_var)
                bptrs_t.append(best_tag_id)
                viterbivars_t.append(next_tag_var[0][best_tag_id].view(1))
            # Now add in the emission scores, and assign forward_var to the set
            # of viterbi variables we just computed
            forward_var = (torch.cat(viterbivars_t) + feat).view(1, -1)
            backpointers.append(bptrs_t)

        # Transition to STOP_TAG
        terminal_var = forward_var + self.transitions[self.t2i[STOP_TAG]]
        best_tag_id = argmax(terminal_var)
        path_score = terminal_var[0][best_tag_id]

        # Follow the back pointers to decode the best path.
        best_path = [best_tag_id]
        for bptrs_t in reversed(backpointers):
            best_tag_id = bptrs_t[best_tag_id]
            best_path.append(best_tag_id)
        # Pop off the start tag (we dont want to return that to the caller)
        start = best_path.pop()
        assert start == self.t2i[START_TAG]  # Sanity check
        best_path.reverse()
        return path_score, best_path

    def loss(self, finalStates, goldLabelStrings):
        goldLabels = torch.tensor([self.t2i[gs] for gs in goldLabelStrings], dtype=torch.long)
        forward_score = self._forward_alg(finalStates)
        gold_score = self._score_sentence(finalStates, goldLabels)
        return forward_score - gold_score
    
    def saveX2i(self):
        x2i = dict()
        x2i["inferenceType"] = TYPE_VITERBI
        x2i["inputSize"] = self.inputSize
        x2i["isDual"] = 1 if self.isDual else 0
        x2i["span"] = spanToString(self.spans) if self.spans else ""
        x2i["nonlinearity"] = self.nonlinearity
        x2i["t2i"] = self.t2i

        return x2i

    def __str__(self):
        return f"ViterbiForwardLayer({self.inDim}, {self.outDim})"

    def inference(self, emissionScores):
        score, labelsIds = self._viterbi_decode(emissionScores)
        return [self.i2t[i] for i in labelsIds]

    def inference2(self, emissionScores):
        return torch.argmax(emissionScores, dim=1)

    def inferenceWithScores(emissionScores):
        raise RuntimeError("ERROR: inferenceWithScores not supported for ViterbiLayer!")

    @classmethod
    def load(cls, x2i):
        inputSize = x2i["inputSize"]
        isDual = x2i.get("isDual", DEFAULT_IS_DUAL) == 1
        sapnValue = x2i.get("span", "")
        spans = None if sapnValue == "" else parseSpan(sapnValue, inputSize)
        nonlinearity = x2i.get("nonlinearity", NONLIN_NONE)
        t2i = x2i["t2i"]
        i2t = {i:t for t, i in t2i.items()}
        dropoutProb = x2i.get("dropoutProb", DEFAULT_DROPOUT_PROBABILITY)

        if spans:
            l = spanLength(spans)
            actualInputSize = 2*l if isDual else l
        else:
            actualInputSize = 2*inputSize if isDual else inputSize

        return cls(inputSize, isDual, t2i, i2t, actualInputSize, nonlinearity, dropoutProb, spans)

