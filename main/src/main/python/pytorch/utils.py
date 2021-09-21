
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

def mkCharacterEmbedding(word, c2i, charLookupParameters, charFwRnnBuilder, charBwRnnBuilder):
    TODO




