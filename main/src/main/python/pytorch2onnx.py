import torch
import argparse
from pyhocon import ConfigFactory
import random

from pytorch.taskManager import TaskManager
from pytorch.metal import Metal
from pytorch.utils import *
from pytorch.constEmbeddingsGlove import ConstEmbeddingsGlove
from sequences.rowReaders import *

import onnx
import onnxruntime

class Saving_Model(torch.nn.Module):
    """docstring for Saving_Model"""
    def __init__(self, model, constEmbeddings):
        super().__init__()
        self.model_length = len(model)
        self.constEmbeddings = constEmbeddings
        self.initialLayers = [None for _ in range(self.model_length)]
        self.intermediateLayerss = [None for _ in range(self.model_length)]
        self.finalLayers = [None for _ in range(self.model_length)]
        for i, layers in enumerate(model):
            if layers.initialLayer is not None:
                self.initialLayers[i] = nn.ModuleList([layers.initialLayer.wordLookupParameters,
                                         layers.initialLayer.charLookupParameters,
                                         layers.initialLayer.charRnnBuilder])
            self.intermediateLayerss[i] = nn.ModuleList(layers.intermediateLayers)
            self.finalLayers[i] = layers.finalLayer
        self.initialLayers = nn.ModuleList(self.initialLayers)
        self.intermediateLayerss = nn.ModuleList(self.intermediateLayerss)
        self.finalLayers = nn.ModuleList(self.finalLayers)
    def forward(self, input_list):
        word_ids, char_ids_list = input_list
        #In current setting, each layer is a nn.Moudle and we need to export each of them. This is not very elegant...
        for i in range(self.model_length):
            if self.initialLayers[i]:
                embeddings = constEmbeddings.emb(word_ids)
                learnedWordEmbeddings = self.initialLayers[i][0](word_ids)
                charEmbedding = torch.stack([mkCharacterEmbedding2(char_ids, self.initialLayers[i][1], self.initialLayers[i][2]) for char_ids in char_ids_list])
                embedParts = [embeddings, learnedWordEmbeddings, charEmbedding]#, posTagEmbed, neTagEmbed, distanceEmbedding, positionEmbedding, predEmbed]
                embedParts = [ep for ep in embedParts if ep is not None]
                embed = torch.cat(embedParts, dim=1)
            for il in self.intermediateLayerss[i]:
                output = il(embed, False)
            if self.finalLayers[i]:
                output = self.finalLayers[i](output, False, None)#headPositions set to be None for now, we can add it in input list later
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
    for param in export_model.parameters():
        param.requires_grad = False

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

            word_ids = torch.LongTensor([constEmbeddings.w2i[word] if word in constEmbeddings.w2i else 0 for word in words]).detach()
            char_ids_list = [torch.LongTensor([c2i.get(c, UNK_EMBEDDING) for c in word]).detach() for word in words]

            dummy_input = [word_ids, char_ids_list]

            output = export_model(dummy_input)

            input_names = ["input_list"]
            output_names = [ "output" ]

    torch.onnx.export(export_model,               # model being run
                  dummy_input,                         # model input (or a tuple for multiple inputs)
                  "model.onnx",   # where to save the model (can be a file or file-like object)
                  export_params=True,        # store the trained parameter weights inside the model file
                  opset_version=10,          # the ONNX version to export the model to
                  do_constant_folding=True,  # whether to execute constant folding for optimization
                  input_names  = ['input'],   # the model's input names
                  output_names = ['output'], # the model's output names
                  dynamic_axes = {'input' : {0 : 'batch_size'},    # variable length axes
                                'output' : {0 : 'batch_size'}})

    onnx_model = onnx.load("model.onnx")
    onnx.checker.check_model(onnx_model)

    ort_session = onnxruntime.InferenceSession("model.onnx")

    def to_numpy(tensor):
        return tensor.detach().cpu().numpy() if tensor.requires_grad else tensor.cpu().numpy()

    # compute ONNX Runtime output prediction
    ort_inputs = {ort_session.get_inputs()[0].name: to_numpy(x)}
    ort_outs = ort_session.run(None, ort_inputs)

    # compare ONNX Runtime and PyTorch results
    np.testing.assert_allclose(to_numpy(output), ort_outs[0], rtol=1e-03, atol=1e-05)

    print("Exported model has been tested with ONNXRuntime, and the result looks good!")

    
