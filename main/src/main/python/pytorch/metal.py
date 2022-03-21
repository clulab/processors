from pytorch.utils import *
from collections import Counter
from sequences.rowReaders import *
from pytorch.layers import Layers
from pytorch.seqScorer import *
from pytorch.constEmbeddingsGlove import ConstEmbeddingsGlove

from torch.optim import SGD, Adam, RMSprop
from torch.optim.lr_scheduler import *

import json
import random

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

        learningRate = self.taskManager.get_float("mtl.learningRate", 1e-5)
        trainerType = self.taskManager.get_string("mtl.trainer", "adam")
        batchSize = self.taskManager.get_int("mtl.batchSize", 1)

        torch.manual_seed(self.taskManager.random)
        random.seed(self.taskManager.random)

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

        scheduler = ExponentialLR(trainer, gamma=0.9)

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
            # this fetches randomized training sentences from all tasks
            sentenceIterator = self.taskManager.getSentences()
            sentCount = 0

            for layers in self.model:
                layers.start_train()
            trainer.zero_grad()

            batchLoss = 0
            i = 0

            # traverse all training sentences
            for metaSentence in sentenceIterator:
                taskId = metaSentence[0]
                sentence = metaSentence[1]

                sentCount += 1

                annotatedSentences = reader.toAnnotatedSentences(sentence)
                assert(annotatedSentences is not None)

                unweightedLoss = 0
                for a_sent in annotatedSentences:
                    unweightedLoss += Layers.loss(self.model, taskId, a_sent[0], a_sent[1])

                loss = unweightedLoss * self.taskManager.tasks[taskId].taskWeight # Zheng: I don't think this is necessary: if self.taskManager.tasks[taskId].taskWeight!=1.0 else unweightedLoss

                batchLoss += loss
                i += 1

                if i >= batchSize:
                    cummulativeLoss += batchLoss.item()
                    batchLoss.backward()
                    trainer.step()
                    batchLoss = 0
                    i = 0

                numTagged += len(sentence)

                if(sentCount % 1000 == 0):
                    print (f"Cumulative loss: {cummulativeLoss/numTagged} ({sentCount} sentences)")
                    cummulativeLoss = 0.0
                    numTagged = 0
            # we may have an incomplete batch here
            if batchLoss:
                cummulativeLoss = batchLoss.item()
                batchLoss.backward()
                trainer.step()
                batchLoss = 0
                i = 0
            scheduler.step()

            # check dev performance in this epoch, for all tasks
            totalAcc = 0.0
            totalPrec = 0.0
            totalRec = 0.0
            totalF1 = 0.0
            for taskId in range(0, self.taskManager.taskCount):
                taskName = self.taskManager.tasks[taskId].taskName
                devSentences = self.taskManager.tasks[taskId].devSentences

                if devSentences:
                    acc, prec, rec, f1 = self.evaluate(taskId, taskName, devSentences, "development", epoch)
                    totalAcc += acc
                    totalPrec += prec
                    totalRec += rec
                    totalF1 += f1

            avgAcc = totalAcc / self.taskManager.taskCount
            avgPrec = totalPrec / self.taskManager.taskCount
            avgRec = totalRec / self.taskManager.taskCount
            avgF1 = totalF1 / self.taskManager.taskCount

            print (f"Average accuracy across {self.taskManager.taskCount} tasks in epoch {epoch}: {avgAcc}")
            print (f"Average P/R/F1 across {self.taskManager.taskCount} tasks in epoch $epoch: {avgPrec} / {avgRec} / {avgF1}")

            allEpochScores.append((epoch, avgF1))

            if avgF1 > maxAvgF1:
                maxAvgF1 = avgF1
                maxAvgAcc = avgAcc
                bestEpoch = epoch
                epochPatience = self.taskManager.epochPatience
            else:
                epochPatience -= 1

            self.save(f"{modelNamePrefix}-epoch{epoch}")

        allEpochScores.sort(key=lambda x: x[1])
        print ("Epochs in descending order of scores:")
        for t in allEpochScores:
            print (f"Epoch #{t[0]}: {t[1]}")

    def evaluate(self, taskId, taskName, sentences, name, epoch=-1):
        scoreCountsByLabel = ScoreCountsByLabel()
        taskNumber = taskId + 1
        sentCount = 0

        print (f"Started evaluation on the {name} dataset for task {taskNumber} ({taskName})...")

        if epoch >= 0:
            pw = open(f"task{taskNumber}.dev.output.{epoch}", "w")
        else:
            pw = open(f"task{taskNumber}.test.output", "w")

        reader = MetalRowReader()

        for sent in sentences:
            sentCount += 1

            annotatedSentences = reader.toAnnotatedSentences(sent)

            for asent in annotatedSentences[:1]:
                sentence = asent[0]
                goldLabels = asent[1]

                constEmbeddings = ConstEmbeddingsGlove.get_ConstLookupParams()
                preds = self.predict(taskId, sentence, constEmbeddings)

                sc = SeqScorer.f1(goldLabels, preds)
                scoreCountsByLabel.incAll(sc)

                printCoNLLOutput(pw, sentence.words, goldLabels, preds)
        
        pw.close()

        print (f"Accuracy on {len(sentences)} {name} sentences for task {taskNumber} ({taskName}): {scoreCountsByLabel.accuracy()}")
        print (f"Precision on {len(sentences)} {name} sentences for task {taskNumber} ({taskName}): {scoreCountsByLabel.precision()}")
        print (f"Recall on {len(sentences)} {name} sentences for task {taskNumber} ({taskName}): {scoreCountsByLabel.recall()}")
        print (f"Micro F1 on {len(sentences)} {name} sentences for task {taskNumber} ({taskName}): {scoreCountsByLabel.f1()}")
        for label in scoreCountsByLabel.labels():
            print (f"\tP/R/F1 for label {label} ({scoreCountsByLabel.map[label].gold}): {scoreCountsByLabel.precision(label)} / {scoreCountsByLabel.recall(label)} / {scoreCountsByLabel.f1(label)}")

        return ( scoreCountsByLabel.accuracy(), scoreCountsByLabel.precision(), scoreCountsByLabel.recall(), scoreCountsByLabel.f1() )

    def predictJointly(self, sentence, constEmbeddings):
        # for layers in self.model:
        #     layers.start_eval()
        return Layers.predictJointly(self.model, sentence, constEmbeddings)

    def predict(self, taskId, sentence, constEmbeddings):
        # for layers in self.model:
        #     layers.start_eval()
        return Layers.predict(self.model, taskId, sentence, constEmbeddings)

    def predictWithScores(self, taskId, sentence, constEmbeddings):
        # for layers in self.model:
        #     layers.start_eval()
        return Layers.predictWithScores(self.model, taskId, sentence, constEmbeddings)

    # Custom method for the parsing algorithm
    # @param sentence Input sentence
    # @param constEmbeddings Constant embeddings for this sentence
    # @return Tuple of (head, label) for each word in the sentence
    def parse(self, sentence, constEmbeddings):
        Layers.parse(self.model, sentence, constEmbeddings)

    def test(self):

        for layers in self.model:
            layers.start_eval()
        for taskId in range(0, self.taskManager.taskCount):
            taskName = self.taskManager.tasks[taskId].taskName
            testSentences = self.taskManager.tasks[taskId].testSentences
            if testSentences:
                self.evaluate(taskId, taskName, testSentences, "testing")

    def save(self, baseFilename):

        params = list()
        if "-epoch0" in baseFilename:
            j_params = list()
        for layers in self.model:
            sd = layers.get_state_dict()
            params.append(sd)
            if "-epoch0" in baseFilename:
                x2i = layers.saveX2i()
                j_params.append({"x2i": x2i})

        # torch pickle save
        try:
            torch.save(params, baseFilename+".torch")
            print("model saved to {}".format(baseFilename+".torch"))
        except BaseException:
            print("[Warning: Saving failed... continuing anyway.]")

        # We can also save as text json file:
        if "-epoch0" in baseFilename:
            with open(baseFilename.replace("-epoch0", "")+".json", "w") as f:
                f.write(json.dumps(j_params))


    @classmethod
    def load(cls, modelFilenamePrefix):
        print (f"Loading MTL model from {modelFilenamePrefix}...")
        layersSeq = list()
        checkpoint = torch.load(modelFilenamePrefix+".torch")
        with open(modelFilenamePrefix+".json") as f:
            x2i = josn.load(f)
        for i, param in enumerate(checkpoint):
            layers = Layers.loadX2i(x2i[i])
            layers.load_state_dict(param)
            layersSeq.append(layers)

        print (f"Loading MTL model from {modelFilenamePrefix} complete.")

        return layersSeq

    @classmethod
    def load_multi(cls, models):
        print (f"Loading MTL models from {models}...")

        layersSeq = list()
        for model in models:
            checkpoint = torch.load(model+".torch")
            with open(model+".json") as f:
                x2i = josn.load(f)
            for i, param in enumerate(checkpoint):
                layers = Layers.loadX2i(x2i[i])
                layers.load_state_dict(param)
                if len(layersSeq)<len(checkpoint):
                    layersSeq.append(layers)
                else:
                    layersSeq[i].add_state_dict(layers)
        for layers in layersSeq:
            layers.avg_state_dict(len(models))
        print (f"Loading MTL models from {models} complete.")
        return layersSeq

    @classmethod
    def apply(cls, modelFilenamePrefix, taskManager=None):
        model = Metal.load(modelFilenamePrefix)
        return cls(taskManager, model)
























