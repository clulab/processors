import torch

from pytorch.metal import Metal
from pytorch.utils import mkCharacterEmbedding2

class Saving_Model(nn.Module):
    """docstring for Saving_Model"""
    def __init__(self, model, constEmbeddings):
        super().__init__()
        self.model = model
        for layers in model:
            layers.start_eval()
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
    args = parser.parse_args()
    modelName = args.model_file
    model = Metal.load(modelName)

    export_model = Saving_Model(model)

    torch.onnx.export(export_model, dummy_input, "model.onnx", verbose=True, input_names=input_names_2, output_names=output_names_2)

    