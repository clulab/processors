#!/bin/bash

#
# Script to train a NER model 
#

MODEL=$1

sbt 'runMain org.clulab.dynet.Metal -train '"$MODEL"' -conf org/clulab/mtl-en-ner.conf'

