import torch
import torch.nn
from torch.autograd import Variable
import torch.nn.functional as F

from pytorch.finalLayer import FinalLayer

from pytorch.utils import *

class ForwardLayer(FinalLayer):
    def __init__(self, inputSize, isDual, t2i, i2t, actualInputSize, nonlinearity, dropoutProb, spans = None):
        super().__init__()
        self.inputSize = inputSize
        self.isDual = isDual
        self.t2i = t2i
        self.i2t = i2t
        self.spans = spans
        self.nonlinearity = nonlinearity

        self.pH = nn.Linear(actualInputSize, len(t2i))
        self.pRoot = Variable(torch.rand(inputSize)) #TODO: Not sure about the shape here
        self.dropoutProb = dropoutProb

        self.inDim = spanLength(spans) if spans is not None else inputSize
        self.outDim = len(t2i)


    def pickSpan(self, v, i):
        if self.spans is None:
            return v
        else:
            # Zheng: Will spans overlap?
            vs = list()
            for span in self.spans:
                e = torch.index_select(v, i, torch.tensor(range(span[0], span[1])))
                vs.append(e)
            return torch.cat(vs, dim=i)

    def forward(self, inputExpressions, doDropout, headPositionsOpt = None):
        if not self.isDual:
            # Zheng: Why the for loop here? Can we just use matrix manipulation?
            argExp = expressionDropout(self.pickSpan(inputExpressions, 1), self.dropoutProb, doDropout)
            emissionScores = expressionDropout(self.pH(argExp), self.dropoutProb, doDropout)
            if self.nonlinearity == NONLIN_TANH:
                emissionScores = F.tanh(emissionScores)
            elif self.nonlinearity == NONLIN_RELU:
                emissionScores = F.relu(emissionScores)
            # for i, e in enumerate(inputExpressions):
            #     argExp = expressionDropout(self.pickSpan(e), self.dropoutProb, doDropout)
            #     l1 = expressionDropout(self.pH(argExp), self.dropoutProb, doDropout)
            #     if self.nonlinearity == NONLIN_TANH:
            #         l1 = F.tanh(l1)
            #     elif self.nonlinearity == NONLIN_RELU:
            #         l1 = F.relu(l1)
            #     emissionScores.append(l1)
        else:
            emissionScores = list()
            if headPositionsOpt is None:
                raise RuntimeError("ERROR: dual task without information about head positions!")
            for i, e in enumerate(inputExpressions):
                headPosition = headPositionsOpt[i]
                argExp = expressionDropout(self.pickSpan(e, 0), self.dropoutProb, doDropout)
                if headPosition >= 0:
                    # there is an explicit head in the sentence
                    predExp = expressionDropout(self.pickSpan(inputExpressions[headPosition], 0), self.dropoutProb, doDropout)
                else:
                    # the head is root. we used a dedicated Parameter for root
                    predExp = expressionDropout(self.pickSpan(self.pRoot, 0), self.dropoutProb, doDropout)
                ss = torch.cat([argExp, predExp])
                l1 = expressionDropout(self.pH(ss), self.dropoutProb, doDropout)
                if self.nonlinearity == NONLIN_TANH:
                    l1 = F.tanh(l1)
                elif self.nonlinearity == NONLIN_RELU:
                    l1 = F.relu(l1)
                emissionScores.append(l1)
            emissionScores = torch.stack(emissionScores)
        return emissionScores

    @staticmethod
    def load(x2i):
        from pytorch.greedyForwardLayer import GreedyForwardLayer
        from pytorch.viterbiForwardLayer import ViterbiForwardLayer
        inferenceType = x2i["inferenceType"]
        if inferenceType == TYPE_VITERBI:
            return ViterbiForwardLayer.load(x2i)
        elif inferenceType == TYPE_GREEDY:
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

        spanConfig = config.get_string(paramPrefix + ".span", "")
        if spanConfig is "":
            span = None
        else:
            span = parseSpan(spanConfig)

        if span:
            l = spanLength(span)
            actualInputSize = 2*l if isDual else l
        else:
            actualInputSize = 2*inputSize if isDual else inputSize

        if inferenceType == TYPE_GREEDY_STRING:
            return GreedyForwardLayer(inputSize, isDual, t2i, i2t, actualInputSize, nonlin, dropoutProb, span)
        elif inferenceType == TYPE_VITERBI_STRING:
            layer = ViterbiForwardLayer(inputSize, isDual, t2i, i2t, actualInputSize, nonlin, dropoutProb, span)
            return layer
        else:
            raise RuntimeError(f"ERROR: unknown inference type {inferenceType}!")
    
def spanLength(spans):
    return sum(end - start for start, end in spans)

def parseSpan(spanParam, inputSize=None):
    # Zheng: Why do we need inputSize here?
    spans = list()
    spanParamTokens = spanParam.split(",")
    for spanParamToken in spanParamTokens:
        # spanTokens = spanParamToken.split('-')
        # assert(len(spanTokens) == 2)
        # spans.append((int(spanTokens[0]), int(spanTokens[1])))
        token1, token2 = map(int, spanParamToken.split('-'))
        spans.append((token1, token2))
    return spans

def spanToString(spans):
    return ','.join(f'{start}-{end}' for start, end in spans)


























