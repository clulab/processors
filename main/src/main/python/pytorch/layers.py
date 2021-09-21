import torch.nn as nn
from utils import *
from embeddingLayer import EmbeddingLayer

class Layers(nn.Module):
    def __init__(self, initialLayer, intermediateLayers, finalLayer):
        super().__init__()
        if finalLayer:
            self.outDim = finalLayer.outDim
        elif intermediateLayers:
            self.outDim = intermediateLayers[-1].outDim
        elif initialLayer:
            self.outDim = initialLayer.outDim
        else:
            self.outDim = None

        if initialLayer and intermediateLayers and finalLayer:
            self.nonEmpty = True
        self.isEmpty = not self.nonEmpty

        self.initialLayer = initialLayer
        self.intermediateLayers = intermediateLayers
        self.finalLayer = finalLayer

    def __str__(self):
        s = ""
        started = False
        if(initialLayer.nonEmpty):
            s += "initial = " + initialLayer
            started = True
        for i in intermediateLayers.indices:
            if(started) s += " "
            s += s"intermediate ({i+1}) = " + intermediateLayers[i]
            started = True
        if(finalLayer.nonEmpty):
          if(started) s += " "
          s += "final = " + finalLayer
        return s

    def forward(self, sentence, constEmnbeddings, doDropout):
        if self.initialLayer.isEmpty:
            raise RuntimeError(f"ERROR: you can't call forward() on a Layers object that does not have an initial layer: {self}!")
        states = self.initialLayer(sentence, constEmnbeddings, doDropout)
        for intermediateLayer in self.intermediateLayers:
            states = intermediateLayer(states, doDropout)
        if self.finalLayer.nonEmpty:
            states = self.finalLayer(states, sentence.headPositions, doDropout)

        return states

    def forwardFrom(self, inStates, headPositions, doDropout):
        if self.initialLayer.nonEmpty:
            raise RuntimeError(f"ERROR: you can't call forwardFrom() on a Layers object that has an initial layer: {self}")
        states = inStates
        for intermediateLayer in self.intermediateLayers:
            states = intermediateLayer(states, doDropout)
        if self.finalLayer.nonEmpty:
            states = self.finalLayer(states, sentence.headPositions, doDropout)

        return states

    def saveX2i(self):
        x2i = dict()
        if self.initialLayer.nonEmpty:
            x2i['hasInitial'] = 1
            x2i['initialLayer'] = self.initialLayer.saveX2i()
        else:
            x2i['hasInitial'] = 0
        x2i['intermediateCount'] = len(intermediateLayers)
        x2i['intermediateLayers'] = list()
        for il in self.intermediateLayers:
            x2i['intermediateLayers'].append(il.saveX2i())
        if self.finalLayer.nonEmpty:
            x2i['hasFinal'] = 1
            x2i['finalLayer'] = self.finalLayer.saveX2i()
        else:
            x2i['finalLayer'] = 0

        return x2i

    @classmethod
    def apply(cls, config, paramPrefix, wordCounter, labelCounter, isDual, providedInputSize):
        initialLayer = EmbeddingLayer.initialize(config, paramPrefix + ".initial", wordCounter)

        if(initialLayer):
            inputSize = initialLayer.outDim
        elif(providedInputSize):
            inputSize = providedInputSize
        else:
            inputSize = None

        intermediateLayers = list()
        done = False
        MAX_INTERMEDIATE_LAYERS = 10

        for i in range(1, MAX_INTERMEDIATE_LAYERS):
            if done:
                break
            if inputSize is None:
                raise RuntimeError("ERROR: trying to construct an intermediate layer without a known input size!")

            intermediateLayer = RnnLayer.initialize(config, paramPrefix + f".intermediate{i}", inputSize)

            if intermediateLayer:
                intermediateLayers.append(intermediateLayer)
                inputSize = intermediateLayer.outDim
            else:
                done = True

        if labelCounter:
            if inputSize is None:
                raise RuntimeError("ERROR: trying to construct a final layer without a known input size!")
            else:
                finalLayer = ForwardLayer.initialize(config, paramPrefix + ".final", labelCounter, isDual, inputSize)
        else:
            finalLayer = None

        return cls(initialLayer, intermediateLayers, finalLayer)

    @classmethod
    def loadX2i(cls, x2i):
        hasInitial = x2i['hasInitial']
        initialLayer = EmbeddingLayer.load(x2i['initialLayer']) if hasInitial == 1 else None

        intermediateLayers = list()
        intermediateCount = x2i['intermediateCount']
        for i in range(intermediateCount):
            il = RnnLayer.load(x2i['intermediateLayers'][i])
            intermediateLayers.append(il)

        hasFinal = x2i['hasFinal']
        finalLayer = ForwardLayer.load(x2i['finalLayer']) if hasFinal == 1 else none

        return cls(initialLayer, intermediateLayers, finalLayer)

    def predictJointly(layers, sentence, constEmnbeddings):
        TODO
    def forwardForTask(layers, taskId, sentence, constEmnbeddings, doDropout):
        TODO
    def predict(layers, taskId, sentence, constEmnbeddings):
        TODO
    def predictWithScores(layers, taskId, sentence, constEmnbeddings):
        TODO
    def parse(layers, sentence, constEmnbeddings):
        TODO
    def loss(layers, taskId, sentence, goldLabels):
        TODO






