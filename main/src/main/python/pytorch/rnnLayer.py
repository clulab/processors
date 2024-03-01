from pytorch.intermediateLayer import IntermediateLayer
from pytorch.utils import *
import torch
import torch.nn as nn

class RnnLayer(IntermediateLayer):
    def __init__(self, 
        inputSize, 
        numLayers, 
        rnnStateSize, 
        useHighwayConnections, 
        rnnType, 
        wordRnnBuilder, 
        dropoutProb):
        super().__init__()
        self.inDim = self.inputSize = inputSize
        self.numLayers = numLayers
        self.rnnStateSize = rnnStateSize 
        self.useHighwayConnections = useHighwayConnections
        self.rnnType = rnnType
        self.wordRnnBuilder = mkBuilder(*wordRnnBuilder)
        self.dropoutProb = dropoutProb

        highwaySize = inputSize if useHighwayConnections else 0
        self.outDim = 2 * rnnStateSize + highwaySize

    def forward(self, inputExpressions, dropout):

        assert(inputExpressions is not None)

        States, _ = self.wordRnnBuilder(inputExpressions.unsqueeze(1))
        States = States.squeeze(1)
        if self.useHighwayConnections:
            States = torch.cat([States, inputExpressions], dim=1)

        return States

    def saveX2i(self):
        x2i = dict()
        x2i['inputSize'] = self.inputSize
        x2i['numLayers'] = self.numLayers
        x2i['rnnStateSize'] = self.rnnStateSize
        x2i['useHighwayConnections'] = 1 if self.useHighwayConnections else 0
        x2i['rnnType'] = self.rnnType
        x2i['dropoutProb'] = self.dropoutProb
        return x2i

    def __str__(self):
        return f"RnnLayer({self.rnnType}, {self.inDim}, {self.outDim})"

    @classmethod
    def load(cls, x2i):
        inputSize = x2i['inputSize']
        numLayers = x2i['numLayers']
        rnnType = x2i.get('rnnType', 'lstm')
        rnnStateSize = x2i['rnnStateSize']
        useHighwayConnections = x2i['useHighwayConnections'] == 1
        dropoutProb = x2i['dropoutProb']

        builder = (rnnType, numLayers, inputSize, rnnStateSize, dropoutProb)

        return cls(inputSize, numLayers, rnnStateSize, useHighwayConnections, rnnType, builder, dropoutProb)

    @classmethod
    def initialize(cls, config, paramPrefix, inputSize):

        if(not config.contains(paramPrefix)):
            return None

        numLayers = config.get_int(paramPrefix + ".numLayers", 1)
        rnnStateSize = config.get_int(paramPrefix + ".rnnStateSize", None)
        useHighwayConnections = config.get_bool(paramPrefix + '.useHighwayConnections', False)
        rnnType = config.get_string(paramPrefix + ".type", "lstm")
        dropoutProb = config.get_float(paramPrefix + ".dropoutProb", DEFAULT_DROPOUT_PROBABILITY)

        builder = (rnnType, numLayers, inputSize, rnnStateSize, dropoutProb)

        return cls(inputSize, numLayers, rnnStateSize, useHighwayConnections, rnnType, builder, dropoutProb)

def mkBuilder(rnnType, numLayers, inputSize, rnnStateSize, dropoutProb):
    if rnnType == 'gru':
        return nn.GRU(inputSize, rnnStateSize, numLayers, bidirectional=True, dropout=dropoutProb)
    elif rnnType == 'lstm':
        return nn.LSTM(inputSize, rnnStateSize, numLayers, bidirectional=True, dropout=dropoutProb)
    else:
        raise RuntimeError(f'ERROR: unknown rnnType "{rnnType}"!')





