import torch
import torch.nn as nn

class InitialLayer(nn.Module):

    def __init__(self):
        super().__init__()
        self.outDim = None

    def forward(self, sentence, constEmbeddings, doDropout):
        raise NotImplementedError