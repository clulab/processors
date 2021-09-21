from utils import *
from collections import Counter
from sequences.rowReader import *

class Metal():
    """docstring for Metal"""
    def __init__(self, taskManager, parameters, modelOpt):
        # One Layers object per task; model(0) contains the Layers shared between all tasks (if any)
        if modelOpt:
            self.model = modelOpt
        else:
            self.model = self.initialize()
        self.taskManager = taskManager

    def initialize(self):

        taskWords, taskLabels = mkVocabularies()

        layersPerTask = [None for _ in range(taskManager.taskCount + 1)]

        layersPerTask[0] = Layers.apply(taskManager, "mtl.layers", parameters, taskWords(0), None, isDual = false, providedInputSize = None)

        inputSize = layersPerTask[0].outDim

        for i in taskManager.indices:
            layersPerTask[i+1] = Layers.apply(taskManager, f"mtl.task{i+1}.layers", parameters, taskWords(i + 1), Some(taskLabels(i + 1)), isDual = taskManager.tasks(i).isDual, inputSize)

        for i in range(len(layersPerTask)):
            print (f"Summary of layersPerTask({i}):")
            print (layersPerTask[i])

        return layersPerTask
    
    def mkVocabularies(self):
        # index 0 reserved for the shared Layers; tid + 1 corresponds to each task
        labels = [Counter() for _ in range(taskManager.taskCount + 1)]
        for i in range(1, len(labels)): # labels(0) not used, since only task-specific layers have a final layer
          labels[i][START_TAG] += 1
          labels[i][STOP_TAG] += 1

        words = [Counter() for _ in range(taskManager.taskCount + 1)]

        reader = MetalRowReader()

        for tid in taskManager.indices:
          for sentence in taskManager.tasks[tid].trainSentences:
            annotatedSentences = reader.toAnnotatedSentences(sentence)

            for asent in annotatedSentences:
              annotatedSentence = asent[0]
              sentenceLabels = asent[1]
              for i, word in enumerate(annotatedSentence.words):
                words[tid + 1][word] += 1
                words[0][word] += 1
                labels[tid + 1][sentenceLabels[i]] += 1

        return words, labels

