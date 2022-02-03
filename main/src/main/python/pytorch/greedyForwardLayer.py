from pytorch.forwardLayer import *
from pytorch.utils import *
import numpy as np

class GreedyForwardLayer(ForwardLayer):
    def __init__(self, inputSize, isDual, t2i, i2t, actualInputSize, nonlinearity, dropoutProb, spans = None):
        super().__init__(inputSize, isDual, t2i, i2t, actualInputSize, nonlinearity, dropoutProb, spans)

    def loss(self, finalStates, goldLabelStrings):
        goldLabels = [self.t2i[gs] for gs in goldLabelStrings]
        return sentenceLossGreedy(finalStates, goldLabels)

    def saveX2i(self):
        x2i = dict()
        x2i["inferenceType"] = TYPE_GREEDY
        x2i["inputSize"] = self.inputSize
        x2i["isDual"] = 1 if self.isDual else 0
        x2i["span"] = spanToString(self.spans) if self.spans else ""
        x2i["nonlinearity"] = self.nonlinearity
        x2i["t2i"] = self.t2i

        return x2i

    def __str__(self):
        return f"GreedyForwardLayer({self.inDim}, {self.outDim})"

    def inference(self, emissionScores):
        emissionScores = emissionScoresToArrays(emissionScores)
        return [self.i2t[np.argmax(es)] for es in emissionScores]

    def inference2(self, emissionScores):
        return torch.argmax(emissionScores, dim=1)

    def inferenceWithScores(self, emissionScores):
        emissionScores = emissionScoresToArrays(emissionScores)
        return [sorted([(i, s) for i, s in enumerate(scoresForPosition)], key=lambda x: x[1]) for scoresForPosition in emissionScores]

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
    