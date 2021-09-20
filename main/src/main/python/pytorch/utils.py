
class Utils:
    def __init__(self):
        self.concatenateCount = 0

        self.UNK_WORD = "<UNK>"
        self.EOS_WORD = "<EOS>"

        self.UNK_EMBEDDING = 0

        self.START_TAG = "<START>"
        self.STOP_TAG = "<STOP>"

        self.RANDOM_SEED = 2522620396L # used for both DyNet, and the JVM seed for shuffling data
        self.WEIGHT_DECAY = 1e-5

        self.LOG_MIN_VALUE = -10000.0

        self.DEFAULT_DROPOUT_PROBABILITY = 0.0 # no dropout by  default

        self.IS_DYNET_INITIALIZED = False

