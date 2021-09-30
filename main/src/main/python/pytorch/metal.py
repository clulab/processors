from pytorch.utils import *
from collections import Counter
from sequences.rowReaders import *
from pytorch.layers import Layers

from torch.optim import SGD, Adam

class Metal(object):
    """docstring for Metal"""
    def __init__(self, taskManager, modelOpt):
        self.taskManager = taskManager

        # One Layers object per task; model(0) contains the Layers shared between all tasks (if any)
        if modelOpt:
            self.model = modelOpt
        else:
            self.model = self.initialize()

    def initialize(self):

        taskWords, taskLabels = self.mkVocabularies()

        layersPerTask = [None for _ in range(self.taskManager.taskCount + 1)]

        layersPerTask[0] = Layers.apply(self.taskManager, "mtl.layers", taskWords[0], None, False, None)

        inputSize = layersPerTask[0].outDim

        for i in self.taskManager.indices:
            layersPerTask[i+1] = Layers.apply(self.taskManager, f"mtl.task{i+1}.layers", taskWords[i + 1], taskLabels[i + 1], self.taskManager.tasks[i].isDual, inputSize)

        for i in range(len(layersPerTask)):
            print (f"Summary of layersPerTask({i}):")
            print (layersPerTask[i])

        return layersPerTask
    
    def mkVocabularies(self):
        # index 0 reserved for the shared Layers; tid + 1 corresponds to each task
        labels = [Counter() for _ in range(self.taskManager.taskCount + 1)]
        for i in range(1, len(labels)): # labels(0) not used, since only task-specific layers have a final layer
          labels[i][START_TAG] += 1
          labels[i][STOP_TAG] += 1

        words = [Counter() for _ in range(self.taskManager.taskCount + 1)]

        reader = MetalRowReader()

        for tid in self.taskManager.indices:
          for sentence in self.taskManager.tasks[tid].trainSentences:
            annotatedSentences = reader.toAnnotatedSentences(sentence)

            for asent in annotatedSentences:
              annotatedSentence = asent[0]
              sentenceLabels = asent[1]
              for i, word in enumerate(annotatedSentence.words):
                words[tid + 1][word] += 1
                words[0][word] += 1
                labels[tid + 1][sentenceLabels[i]] += 1

        return words, labels

    def train(self, modelNamePrefix):
        learningRate = self.taskManager.get_float("mtl.learningRate", 0.001)
        trainerType = self.taskManager.get_string("mtl.trainer", "adam")
        batchSize = self.taskManager.get_int("mtl.batchSize", 1)
        assert(batchSize>0)

        parameters = list()
        for layers in self.model:
            parameters += layers.get_parameters()

        if trainerType == "adam":
            trainer = Adam(parameters, lr=learningRate)
        elif trainerType == "rmsprop":
            trainer = RMSprop(parameters, lr=learningRate)
        elif trainerType == "sgd":
            trainer = SDG(parameters, lr=learningRate)
        else:
            raise RuntimeError(f"ERROR: unknown trainer {trainerType}!")

        reader = MetalRowReader()

        cummulativeLoss = 0.0
        numTagged = 0
        
        maxAvgAcc = 0.0
        maxAvgF1 = 0.0
        bestEpoch = 0

        allEpochScores = list()
        epochPatience = self.taskManager.epochPatience

        for epoch in range(0, self.taskManager.maxEpochs):
            if epochPatience <= 0:
                break
            








