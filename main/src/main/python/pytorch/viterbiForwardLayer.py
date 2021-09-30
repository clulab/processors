from forwardLayer import *
from utils import *

class GreedyForwardLayer(ForwardLayer):
    def __init__(self, inputSize, isDual, t2i, i2t, actualInputSize, nonlinearity, dropoutProb, spans = None):
        super().__init__(inputSize, isDual, t2i, i2t, actualInputSize, nonlinearity, dropoutProb, spans)

    def initializeTransitions(self):
        #TODO
        pass
    def initTransitionsTo(self, dst, size, startTag, stopTag):
        #TODO
        pass
    def loss(self, finalStates, goldLabelStrings):
        #TODO
        pass
    def saveX2i(self):
        #TODO
        pass
    def __str__(self):
        #TODO
        pass
    def inference(emissionScores):
        #TODO
        pass
    def inferenceWithScores(emissionScores):
        #TODO
        pass
    @classmethod
    def load(cls, x2i):
        #TODO
        pass