#!/bin/bash

#
# Script to train a syntactic dependency parser model
#

MODEL=$1

sbt 'runMain org.clulab.dynet.Metal -train '"$MODEL"' -conf org/clulab/mtl-en-depsh.conf'
sbt 'runMain org.clulab.dynet.Metal -train '"$MODEL"' -conf org/clulab/mtl-en-depsl.conf'

