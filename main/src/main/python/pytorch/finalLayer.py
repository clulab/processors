import torch
import torch.nn as nn

class FinalLayer(nn.Module):

    def __init__(self):
        super().__init__()
        self.inDim = None
        self.outDim = None

    def forward(self, inputExpressions, headPositionsOpt, doDropout):
        raise NotImplementedError

    def loss(self, emissionScoresAsExpression, goldLabels):
        raise NotImplementedError

    def inference(self, emissionScores):
        raise NotImplementedError

    def inferenceWithScores(self, emissionScores):
        raise NotImplementedError