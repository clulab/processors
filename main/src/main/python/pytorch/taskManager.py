import random
import math
from sequences.columnReader import ColumnReader
from dataclasses import dataclass

TYPE_BASIC = 0
TYPE_DUAL = 1

class TaskManager():

  def __init__(self, config, seed):

    self.config = config
    self.random = seed

    # How many shards to have per epoch
    self.shardsPerEpoch = config.get_int("mtl.shardsPerEpoch", 10)

    # Total number of epochs 
    self.maxEpochs:Int = config.get_int("mtl.maxEpochs", 100)

    # Training patience in number of epochs 
    self.epochPatience:Int = config.get_int("mtl.epochPatience", 5)

    # Array of all tasks to be managed 
    self.tasks = self.readTasks()

    self.taskCount = len(self.tasks)
    self.indices = range(self.taskCount)

    # Training shards from all tasks 
    self.shards = self.mkShards()

  def contains(self, paramPrefix):
    return self.config.__contains__(paramPrefix)

  def get_int(self, x, defualt=None):
    return self.config.get_int(x, defualt)

  def get_string(self, x, defualt=None):
    return self.config.get_string(x, defualt)

  def get_float(self, x, defualt=None):
    return self.config.get_float(x, defualt)

  def get_bool(self, x, defualt=None):
    return self.config.get_bool(x, defualt)

  def get_list(self, x, defualt=None):
    return self.config.get_list(x, defualt)

  def get_config(self, x, defualt=None):
    return self.config.get_config(x, defualt)

  # Construct training shards by interleaving shards from all tasks 
  def mkShards(self):
    shardsByTasks = list()

    # construct the shards for each task
    for i in self.indices:
      shardsByTasks += [self.tasks[i].mkShards()]
      assert(len(shardsByTasks[i]) == self.shardsPerEpoch)

    # now interleave the tasks
    interleavedShards = list()
    for i in range(self.shardsPerEpoch):
      for j in self.indices:
        crtShard = shardsByTasks[j][i]
        interleavedShards += [crtShard]

    return interleavedShards

  # Iterator over all sentences coming from all interleaved shards 
  def getSentences(self):
    random.seed(self.random)
    randomizedShards = random.sample(self.shards, len(self.shards))
    for shard in randomizedShards:
      sents = random.sample(range(shard.startPosition, shard.endPosition), shard.endPosition-shard.startPosition)
      for sent in sents:
        yield (shard.taskId, self.tasks[shard.taskId].trainSentences[sent])

  # Reads all tasks from disk in memory 
  def readTasks(self):
    numberOfTasks = self.config.get_int("mtl.numberOfTasks", None)
    tasks = list()
    for i in range(numberOfTasks):
      tasks += [self.readTask(i + 1)]

    print (f"Read {numberOfTasks} tasks from config file.")
    return tasks

  def readTask(self, taskNumber):
    taskName = self.config.get_string(f"mtl.task{taskNumber}.name", None)
    train = self.config.get_string(f"mtl.task{taskNumber}.train", None)

    dev = self.config.get_string(f"mtl.task{taskNumber}.dev", None) if f"mtl.task{taskNumber}.dev" in self.config else None
    test = self.config.get_string(f"mtl.task{taskNumber}.test", None) if f"mtl.task{taskNumber}.test" in self.config else None

    taskType = self.parseType(self.config.get_string(f"mtl.task{taskNumber}.type", "basic"))

    weight = self.config.get_float(f"mtl.task{taskNumber}.weight", 1.0)

    return Task(taskNumber - 1, taskName, taskType, self.shardsPerEpoch, weight, train, dev, test)

  def parseType(self, inf):
    if inf == "basic": return TYPE_BASIC
    elif inf == "dual": return TYPE_DUAL
    else: raise ValueError(f"ERROR: unknown task type {inf}!")

  def debugTraversal(self):
    for epoch in range(self.maxEpochs):
      print (f"Started epoch {epoch}")
      sentCount = 0
      taskId = 0
      totalSents = 0
      for sentence in self.getSentences():
        totalSents += 1
        if(sentence[0] != taskId):
          print (f"Read {sentCount} sentences from task {taskId}")
          taskId = sentence[0]
          sentCount = 1
        else:
          sentCount += 1
      print (f"Read {sentCount} sentences from task {taskId}")
      print (f"Read {totalSents} sentences in epoch {epoch}.")

@dataclass
class Shard:
  taskId: int
  startPosition: int
  endPosition: int

class Task:
  def __init__(self,
  taskId, # this starts at 0 so we can use it as an index in the array of tasks
  taskName:str,
  taskType:int,
  shardsPerEpoch:int,
  taskWeight:float,
  trainFileName:str,
  devFileName:str = None,
  testFileName:str = None):
    self.taskId = taskId
    taskNumber = taskId + 1
    print (f"Reading task {taskNumber} ({taskName})...")
    self.trainSentences = ColumnReader.readColumns(trainFileName)
    l = len(self.trainSentences)
    self.trainSentences = self.trainSentences
    self.devSentences = ColumnReader.readColumns(devFileName) if devFileName else None
    self.testSentences = ColumnReader.readColumns(testFileName) if testFileName else None

    self.isBasic:bool = taskType == TYPE_BASIC
    self.isDual:bool = taskType == TYPE_DUAL

    if taskType == TYPE_BASIC: 
      self.prettyType = "basic"
    elif taskType == TYPE_DUAL: 
      self.prettyType = "dual"
    else: 
      self.prettyType = "unknown"

    # The size of the training shard for this task
    self.shardSize = math.ceil(len(self.trainSentences) / shardsPerEpoch)

    # Current position in the training sentences when we iterate during training
    currentTrainingSentencePosition = 0

    self.taskWeight = taskWeight
    self.taskName = taskName

    print (f"============ starting task {taskNumber} ============")
    print (f"Read {len(self.trainSentences)} training sentences for task {taskNumber}, with shard size {self.shardSize}.")
    if(self.devSentences is not None):
      print (f"Read {len(self.devSentences)} development sentences for task {taskNumber}.")
    if(self.testSentences is not None):
      print (f"Read {len(self.testSentences)} testing sentences for task {taskNumber}.")
    print (f"Using taskWeight = {taskWeight}")
    print (f"Task type = {self.prettyType}.")
    print (f"============ completed task {taskNumber} ============")

  # Construct the shards from all training sentences in this task 
  def mkShards(self):
    shards = list()
    crtPos = 0
    while(crtPos < len(self.trainSentences)):
      endPos = min(crtPos + self.shardSize, len(self.trainSentences))
      shards += [Shard(self.taskId, crtPos, endPos)]
      crtPos = endPos
    return shards
