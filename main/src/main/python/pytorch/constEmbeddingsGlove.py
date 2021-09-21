from dataclasses import dataclass
import torch.nn as nn

@dataclass
class ConstEmbeddingParameters:
    emb: nn.Embedding.from_pretrianed("....")
    w2i: dict