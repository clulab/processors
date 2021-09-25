import torch
import torch.nn as nn

class IntermediateLayer(nn.Module):

    def __init__(self):
        super().__init__()
        self.inDim = None
        self.outDim = None

    def forward(self, inputExpressions, doDropout):
        raise NotImplementedError