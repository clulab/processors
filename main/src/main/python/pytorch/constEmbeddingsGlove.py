from dataclasses import dataclass
import torch.nn as nn
from embeddings.wordEmbeddingMap import *
from pyhocon import ConfigFactory
import numpy as np
import torch

@dataclass
class ConstEmbeddingParameters:
    emb: nn.Embedding
    w2i: dict

class _ConstEmbeddingsGlove:
    def __init__(self):
        self.SINGLETON_WORD_EMBEDDING_MAP = None
        config = ConfigFactory.parse_file('../resources/org/clulab/glove.conf')
        self.load(config)
        self.dim = self.SINGLETON_WORD_EMBEDDING_MAP.dim

    def load(self, config):
        if self.SINGLETON_WORD_EMBEDDING_MAP is None:
            self.SINGLETON_WORD_EMBEDDING_MAP = WordEmbeddingMap(config)

    def mkConstLookupParams(self, words):
        w2i = dict()
        weights = np.zeros((len(words), self.dim))
        for i,w  in enumerate(words):
            weights[i] = self.SINGLETON_WORD_EMBEDDING_MAP.emb_dict.get(w, self.SINGLETON_WORD_EMBEDDING_MAP.emb_dict["<UNK>"])
            w2i[w] = i
        emb = nn.Embedding.from_pretrained(torch.tensor(weights), freeze=True)
        return ConstEmbeddingParameters(emb ,w2i)

ConstEmbeddingsGlove = _ConstEmbeddingsGlove()
