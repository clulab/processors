import torch.nn as nn
from pytorch.utils import *
from pytorch.embeddingLayer import EmbeddingLayer
from pytorch.rnnLayer import RnnLayer
from pytorch.forwardLayer import ForwardLayer
from pytorch.constEmbeddingsGlove import ConstEmbeddingsGlove

class Layers(object):
    def __init__(self, initialLayer, intermediateLayers, finalLayer):
        if finalLayer:
            self.outDim = finalLayer.outDim
        elif intermediateLayers:
            self.outDim = intermediateLayers[-1].outDim
        elif initialLayer:
            self.outDim = initialLayer.outDim
        else:
            self.outDim = None

        self.nonEmpty = initialLayer is not None and intermediateLayers is not None and finalLayer is not None
        self.isEmpty = not self.nonEmpty

        self.initialLayer = initialLayer
        self.intermediateLayers = intermediateLayers
        self.finalLayer = finalLayer

    def __str__(self):
        s = ""
        started = False
        if(self.initialLayer is not None):
            s += "initial = " + str(self.initialLayer)
            started = True
        for i in range(len(self.intermediateLayers)):
            if(started): s += " "
            s += f"intermediate ({i+1}) = " + str(self.intermediateLayers[i])
            started = True
        if(self.finalLayer is not None):
          if(started): s += " "
          s += "final = " + str(self.finalLayer)
        return s

    def get_parameters(self):
        parameters = list()
        if self.initialLayer is not None:
            parameters += [p for p in self.initialLayer.parameters() if p.requires_grad]
        for il in self.intermediateLayers:
            parameters += [p for p in il.parameters() if p.requires_grad]
        if self.finalLayer is not None:
            parameters += [p for p in self.finalLayer.parameters() if p.requires_grad]
        return parameters

    def start_train(self):
        if self.initialLayer is not None:
            self.initialLayer.train()
        for il in self.intermediateLayers:
            il.train()
        if self.finalLayer is not None:
            self.finalLayer.train()
    
    def start_eval(self):
        if self.initialLayer is not None:
            self.initialLayer.eval()
        for il in self.intermediateLayers:
            il.eval()
        if self.finalLayer is not None:
            self.finalLayer.eval()

    def get_state_dict(self):
        params = dict()
        j_params = dict()
        if self.initialLayer is not None:
            params['initialLayer'] = self.initialLayer.state_dict()
            j_params['initialLayer'] = {k:v.data.tolist() for k, v in params['initialLayer'].items()}
        if self.intermediateLayers:
            params['intermediateLayers'] = list()
            j_params['intermediateLayers'] = list()
        for il in self.intermediateLayers:
            params['intermediateLayers'].append(il.state_dict())
            j_params['intermediateLayers'].append({k:v.data.tolist() for k, v in params['intermediateLayers'][-1].items()})
        if self.finalLayer is not None:
            params['finalLayer'] = self.finalLayer.state_dict()
            j_params['finalLayer'] = {k:v.data.tolist() for k, v in params['finalLayer'].items()}
        return params, j_params

    def load_state_dict(self, params):
        if self.initialLayer is not None:
            self.initialLayer.load_state_dict(params['initialLayer'])
        for i, il in enumerate(self.intermediateLayers):
            il.load_state_dict(params['intermediateLayers'][i])
        if self.finalLayer is not None:
            self.finalLayer.load_state_dict(params['finalLayer'])


    def forward(self, sentence, constEmbeddings, doDropout):
        if self.initialLayer is None:
            raise RuntimeError(f"ERROR: you can't call forward() on a Layers object that does not have an initial layer: {self}!")
        states = self.initialLayer(sentence, constEmbeddings, doDropout)
        for intermediateLayer in self.intermediateLayers:
            states = intermediateLayer(states, doDropout)
        if self.finalLayer is not None:
            states = self.finalLayer(states, doDropout, sentence.headPositions)

        return states

    def forwardFrom(self, inStates, headPositions, doDropout):
        if self.initialLayer is not None:
            raise RuntimeError(f"ERROR: you can't call forwardFrom() on a Layers object that has an initial layer: {self}")
        states = inStates
        for intermediateLayer in self.intermediateLayers:
            states = intermediateLayer(states, doDropout)
        if self.finalLayer is not None:
            states = self.finalLayer(states, doDropout, headPositions)

        return states

    def saveX2i(self):
        x2i = dict()
        if self.initialLayer is not None:
            x2i['hasInitial'] = 1
            x2i['initialLayer'] = self.initialLayer.saveX2i()
        else:
            x2i['hasInitial'] = 0
        x2i['intermediateCount'] = len(self.intermediateLayers)
        x2i['intermediateLayers'] = list()
        for il in self.intermediateLayers:
            x2i['intermediateLayers'].append(il.saveX2i())
        if self.finalLayer is not None:
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

    @staticmethod
    def predictJointly(layers, sentence, constEmbeddings):
        labelsPerTask = list()
        # layers(0) contains the shared layers
        if layers[0]:
            sharedStates = layers[0].forward(sentence, constEmbeddings, doDropout=False)
            for i in range(1, len(layers)):
                states = layers[i].forwardFrom(sharedStates, sentence.headPositions, doDropout=False)
                labels = layers[i].finalLayer.inference(states)
                labelsPerTask += [labels]
        # no shared layer
        else:
            for i in range(1, len(layers)):
                states = layers[i].forward(sentence, sentence.headPositions, doDropout=False)
                labels = layers[i].finalLayer.inference(states)
                labelsPerTask += [labels]

        return labelsPerTask

    @staticmethod
    def forwardForTask(layers, taskId, sentence, constEmbeddings, doDropout):
        if layers[0]:
            sharedStates = layers[0].forward(sentence, constEmbeddings, doDropout)
            states = layers[taskId+1].forwardFrom(sharedStates, sentence.headPositions, doDropout)
        else:
            states = layers[taskId+1].forward(sentence, constEmbeddings, doDropout)
        return states

    @staticmethod
    def predict(layers, taskId, sentence, constEmbeddings):
        states = Layers.forwardForTask(layers, taskId, sentence, constEmbeddings, doDropout=False)
        return layers[taskId+1].finalLayer.inference(states)

    @staticmethod
    def predictWithScores(layers, taskId, sentence, constEmbeddings):
        states = Layers.forwardForTask(layers, taskId, sentence, constEmbeddings, doDropout=False)
        return layers[taskId+1].finalLayer.inferenceWithScores(states)

    @staticmethod
    def parse(layers, sentence, constEmbeddings):
        #
        # first get the output of the layers that are shared between the two tasks
        #
        assert(layers[0].nonEmpty)
        sharedStates = layers[0].forward(sentence, constEmbeddings, doDropout=False)

        #
        # now predict the heads (first task)
        #
        headStates = layers[1].forwardFrom(sharedStates, None, doDropout=False)
        headScores = layers[1].finalLayer.inference(headStates)

        # store the head values here
        heads = list()
        for wi, predictionsForThisWord in enumerate(headScores):
            # pick the prediction with the highest score, which is within the boundaries of the current sentence
            done = False
            for hi, relative in enumerate(predictionsForThisWord):
                if done:
                    break
                try:
                    relativeHead = int(relative[0])
                    if relativeHead == 0:
                        heads.append(1)
                        done = True
                    else:
                        headPosition = wi + relativeHead
                        heads.append(headPosition)
                        done = True
                except:
                    raise RuntimeError('''some valid predictions may not be integers, e.g., "<STOP>" may be predicted by the sequence model''')
            if not done:
                # we should not be here, but let's be safe
                # if nothing good was found, assume root
                heads.append(1)
        
        #
        # next, predict the labels using the predicted heads
        #
        labelStates = layers[2].forwardFrom(sharedStates, heads, doDropout=False)
        labels = layers[2].finalLayer.inference(labelStates)
        assert(len(labels)==len(heads))

        return zip(heads, labels)

    @staticmethod
    def loss(layers, taskId, sentence, goldLabels):
        # Zheng: I am not sure this is the suitable way to load embeddings or not, need help...
        constEmbeddings = ConstEmbeddingsGlove.get_ConstLookupParams()
        states = Layers.forwardForTask(layers, taskId, sentence, constEmbeddings, doDropout=True) # use dropout during training!
        loss = layers[taskId+1].finalLayer.loss(states, goldLabels)
        return loss






