from dataclasses import dataclass
import torch.nn as nn
from embeddings.wordEmbeddingMap import *

@dataclass
class ConstEmbeddingParameters:
    emb: nn.Embedding
    w2i: dict

def ConstEmbeddingsGlove:
    def __init__(self):
        self.SINGLETON_WORD_EMBEDDING_MAP = None
        self.load('../resources/org/clulab/glove.conf')
        self.dim = self.SINGLETON_WORD_EMBEDDING_MAP.dim

    def load(self, config):
        if self.SINGLETON_WORD_EMBEDDING_MAP is None:
            self.SINGLETON_WORD_EMBEDDING_MAP = WordEmbeddingMap(config)

    def mkConstLookupParams(self, words):
        w2i = dict()
        for i,w  in enumerate(words):
            weights[i] = self.SINGLETON_WORD_EMBEDDING_MAP.emd_dict.get(w, self.SINGLETON_WORD_EMBEDDING_MAP.emd_dict[0])
            w2i[w] = i
        emd = nn.Embedding.from_pretrained(weight)
        emd.weight.requires_grad=False
        return ConstEmbeddingParameters(emb ,w2i)
