from dataclasses import dataclass
import torch.nn as nn
from embeddings.wordEmbeddingMap import *
from pyhocon import ConfigFactory
import torch

@dataclass
class ConstEmbeddingParameters:
    emb: nn.Embedding
    w2i: dict

class _ConstEmbeddingsGlove:
    def __init__(self):
        self.SINGLETON_WORD_EMBEDDING_MAP = None
        self.cep = None
        config = ConfigFactory.parse_file('../resources/org/clulab/glove.conf')
        self.load(config)
        self.dim = self.SINGLETON_WORD_EMBEDDING_MAP.dim

    def load(self, config):
        if self.SINGLETON_WORD_EMBEDDING_MAP is None:
            self.SINGLETON_WORD_EMBEDDING_MAP = WordEmbeddingMap(config)
        self.cep = ConstEmbeddingParameters(self.SINGLETON_WORD_EMBEDDING_MAP.emb, self.SINGLETON_WORD_EMBEDDING_MAP.w2i)

    def get_ConstLookupParams(self):
        return self.cep

ConstEmbeddingsGlove = _ConstEmbeddingsGlove()
