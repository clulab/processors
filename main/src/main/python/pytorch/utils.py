import torch.nn as nn
import torch

concatenateCount = 0

UNK_WORD = "<UNK>"
EOS_WORD = "<EOS>"

UNK_EMBEDDING = 0

START_TAG = "<START>"
STOP_TAG = "<STOP>"

RANDOM_SEED = 2522620396 # used for both DyNet, and the JVM seed for shuffling data
WEIGHT_DECAY = 1e-5

LOG_MIN_VALUE = -10000.0

DEFAULT_DROPOUT_PROBABILITY = 0.0 # no dropout by  default

IS_DYNET_INITIALIZED = False

def save(file, values, comment):
    file.write("# " + comment + "\n")
    for key, value in values.items():
        file.write(f"{key}\t{value}\n")
    file.write("\n")

def mkCharacterEmbedding(word, c2i, charLookupParameters, charRnnBuilder, hidden_dim):
    charEmbeddings = charLookupParameters(torch.LongTensor([c2i[c] for c in word]))
    (h, c) =  (torch.zeros(2, 1, hidden_dim), torch.zeros(2, 1, hidden_dim)) 
    output, (result, c) = charRnnBuilder(charEmbeddings.view(len(word), 1, -1), (h, c))
    return result.view(1, hidden_dim*2)

def readString2Ids(s2iFilename):
    s2i = dict()
    with open(s2iFilename) as f:
        for line in f:
            if not line.startswith("#"):
                k, v = line.strip().split('\t')
                s2i[k] = int(v)

def readChar2Ids(s2iFilename):
    s2i = dict()
    with open(s2iFilename) as f:
        for line in f:
            if not line.startswith("#"):
                k, v = line.strip().split('\t')
                s2i[char(int(k))] = int(v)


