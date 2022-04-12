#!/bin/bash

#
# Script to train a SRL model for the recognition of arguments (assuming gold predicates)
#

if [ "$#" -ne 1 ]; then
    echo "You must provide the model name in the command line!"
    exit 1
fi

MODEL=$1

sbt 'runMain org.clulab.dynet.Metal -train '"$MODEL"' -conf org/clulab/mtl-en-srla.conf'

