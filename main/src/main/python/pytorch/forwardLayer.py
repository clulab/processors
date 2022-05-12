import torch
import torch.nn as nn
from torch.autograd import Variable
import torch.nn.functional as F

from pytorch.finalLayer import FinalLayer

from pytorch.utils import *

class ForwardLayer(FinalLayer):
    def __init__(self, inputSize, isDual, t2i, i2t, actualInputSize, nonlinearity, dropoutProb, distanceEmbeddingSize):
        super().__init__()
        self.inputSize = inputSize
        self.isDual = isDual
        self.t2i = t2i
        self.i2t = i2t
        self.nonlinearity = nonlinearity

        self.pH = nn.Linear(actualInputSize, len(t2i))
        nn.init.xavier_uniform_(self.pH.weight)
        self.pRoot = Variable(torch.rand(inputSize)) #TODO: Not sure about the shape here
        self.dropout = nn.Dropout(dropoutProb)

        self.inDim = inputSize
        self.outDim = len(t2i)

        self.distanceEmbeddingSize = distanceEmbeddingSize

        self.distanceLookupParameters = nn.Embedding(101, distanceEmbeddingSize) if distanceEmbeddingSize > 0 else None


    def forward(self, inputExpressions, modHeadPairs):
        if not self.isDual:
            argExp = self.dropout(inputExpressions)
            emissionScores = self.dropout(self.pH(argExp))
            if self.nonlinearity == NONLIN_TANH:
                emissionScores = F.tanh(emissionScores)
            elif self.nonlinearity == NONLIN_RELU:
                emissionScores = F.relu(emissionScores)
        else:
            emissionScores = []
            if modHeadPairs is None:
                raise RuntimeError("ERROR: dual task without information about head positions!")
            for pair in modHeadPairs:
                modPosition = pair.modifier
                headPosition = pair.head

                argExp = self.dropout(inputExpressions[modPosition])
                predExp = self.dropout(inputExpressions[headPosition]) if (headPosition >= 0) else self.dropout(self.pRoot)

                
                if (self.distanceLookupParameters is None):
                    ss = torch.cat([argExp, predExp])
                else:
                    distEmbedding = mkRelativePositionEmbedding(modPosition, headPosition)
                    ss = torch.cat([argExp, predExp, distEmbedding])

                l1 = self.dropout(self.pH(ss))
                if self.nonlinearity == NONLIN_TANH:
                    l1 = F.tanh(l1)
                elif self.nonlinearity == NONLIN_RELU:
                    l1 = F.relu(l1)
                emissionScores.append(l1)

            emissionScores = torch.stack(emissionScores)

        return emissionScores

    def mkRelativePositionEmbedding(modifier, head):
        dist = 0 if head == -1 else max(-50, min(50, head-modifier))
        return self.distanceLookupParameters(torch.LongTensor(dist))


    @staticmethod
    def load(x2i):
        from pytorch.greedyForwardLayer import GreedyForwardLayer
        from pytorch.viterbiForwardLayer import ViterbiForwardLayer
        inferenceType = x2i["inferenceType"]
        if inferenceType == TYPE_VITERBI or inferenceType == TYPE_VITERBI_STRING:#this is a temporary solution to handle a typo in viterbi forward layer...
            return ViterbiForwardLayer.load(x2i)
        elif inferenceType == TYPE_GREEDY or inferenceType == TYPE_GREEDY_STRING:
            return GreedyForwardLayer.load(x2i)
        else:
            raise RuntimeError(f"ERROR: unknown forward layer type {inferenceType}!")

    @staticmethod
    def initialize(config, paramPrefix, labelCounter, isDual, inputSize):
        from pytorch.greedyForwardLayer import GreedyForwardLayer
        from pytorch.viterbiForwardLayer import ViterbiForwardLayer
        if(not config.contains(paramPrefix)):
            return None

        inferenceType = config.get_string(paramPrefix + ".inference", "greedy")
        dropoutProb = config.get_float(paramPrefix + ".dropoutProb", DEFAULT_DROPOUT_PROBABILITY)

        nonlinAsString = config.get_string(paramPrefix + ".nonlinearity", "")
        if nonlinAsString in nonlin_map:
            nonlin = nonlin_map[nonlinAsString]
        else:
            raise RuntimeError(f"ERROR: unknown non-linearity {nonlinAsString}!")

        t2i = {t:i for i, t in enumerate(labelCounter.keys())}
        i2t = {i:t for t, i in t2i.items()}

        distanceEmbeddingSize = config.get_int(paramPrefix + ".distanceEmbeddingSize", 0)

        actualInputSize = 2*inputSize+distanceEmbeddingSize if isDual else inputSize

        if inferenceType == TYPE_GREEDY_STRING:
            return GreedyForwardLayer(inputSize, isDual, t2i, i2t, actualInputSize, nonlin, dropoutProb, distanceEmbeddingSize)
        elif inferenceType == TYPE_VITERBI_STRING:
            return ViterbiForwardLayer(inputSize, isDual, t2i, i2t, actualInputSize, nonlin, dropoutProb, distanceEmbeddingSize)
        else:
            raise RuntimeError(f"ERROR: unknown inference type {inferenceType}!")



























