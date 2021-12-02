import torch
import argparse
from pyhocon import ConfigFactory
import random

from pytorch.taskManager import TaskManager
from pytorch.metal import Metal
from pytorch.utils import *
from pytorch.constEmbeddingsGlove import ConstEmbeddingsGlove
from sequences.rowReaders import *

class Saving_Model(nn.Module):
    """docstring for Saving_Model"""
    def __init__(self, model, constEmbeddings):
        super().__init__()
        self.model = model
        self.constEmbeddings = constEmbeddings
        self.initialLayers = [None for _ in range(len(model))]
        for i, layers in enumerate(model):
            if layers.initialLayer is not None:
                self.initialLayers[i] = {"wordLookupParameters":layers.initialLayer.wordLookupParameters,
                                         "charLookupParameters":layers.initialLayer.charLookupParameters,
                                         "charRnnBuilder":layers.initialLayer.charRnnBuilder}
        
    def forward(self, word_ids, char_ids_list):

        #In current setting, each layer is a nn.Moudle and we need to export each of them. This is not very elegant...
        for i, layers in enumerate(self.model):
            if self.initialLayers[i]:
                embeddings = constEmbeddings.emb(idxs)
                learnedWordEmbeddings = self.initialLayers[i].wordLookupParameters(word_ids)
                charEmbedding = torch.stack([mkCharacterEmbedding2(char_ids, self.initialLayers[i].charLookupParameters, self.initialLayers[i].charRnnBuilder) for char_ids in char_ids_list])
                embedParts = [embeddings, learnedWordEmbeddings, charEmbedding]#, posTagEmbed, neTagEmbed, distanceEmbedding, positionEmbedding, predEmbed]
                embedParts = [ep for ep in embedParts if ep is not None]
                embed = torch.cat(embedParts, dim=1)
            for j, il in enumerate(layers.intermediateLayers):
                dummy_input = il(dummy_input)
            if layers.finalLayer is not None:
                output = layers.finalLayer(dummy_input)
        return output

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--model_file', type=str, help='Filename of the model.')
    parser.add_argument('--config', type=str, help='Filename of the configuration.')
    parser.add_argument('--seed', type=int, default=1234)
    args = parser.parse_args()

    config = ConfigFactory.parse_file(f'../resources/org/clulab/{args.config}.conf')
    taskManager = TaskManager(config, args.seed)
    modelName = args.model_file
    model = Metal.load(modelName)
    for layers in model:
        layers.start_eval()
    constEmbeddings = ConstEmbeddingsGlove.get_ConstLookupParams()
    export_model = Saving_Model(model, constEmbeddings)
    export_model.eval()

    torch.manual_seed(taskManager.random)
    random.seed(taskManager.random)

    for i, layers in enumerate(model):
        if layers.initialLayer is not None:
            c2i = layers.initialLayer.c2i

    for taskId in range(0, taskManager.taskCount):
        taskName = taskManager.tasks[taskId].taskName
        testSentences = taskManager.tasks[taskId].testSentences
        if testSentences:
            reader = MetalRowReader()
            annotatedSentences = reader.toAnnotatedSentences(testSentences[0])

            asent = annotatedSentences[0]
            sentence = asent[0]
            goldLabels = asent[1]

            words = sentence.words

            word_ids = torch.LongTensor([constEmbeddings.w2i[word] if word in constEmbeddings.w2i else 0 for word in words])
            char_ids_list = [torch.LongTensor([c2i.get(c, UNK_EMBEDDING) for c in word]) for word in words]

            dummy_input = [word_ids, char_ids_list]

            input_names = [ "input1", "input2" ]
            output_names = [ "output" ]

    torch.onnx.export(export_model, dummy_input, "model.onnx", verbose=True, input_names=input_names, output_names=output_names)

    
