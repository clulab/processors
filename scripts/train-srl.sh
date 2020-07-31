#!/bin/bash

#
# Script to train a SRL model for the recognition of arguments (assuming gold predicates)
#

MODEL=$1

sbt 'runMain org.clulab.dynet.Metal -train '"$MODEL"' -conf org/clulab/mtl-en-srla.conf'

