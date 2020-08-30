#!/bin/bash

#
# Script to train a syntactic dependency parser model
#

if [ "$#" -ne 1 ]; then
    echo "You must provide the model name in the command line!"
    exit 1
fi

MODELH="$1"h
MODELL="$1"l

echo "Using $MODELH for heads and $MODELL for labels."

sbt 'runMain org.clulab.dynet.Metal -train '"$MODELH"' -conf org/clulab/mtl-en-depsh.conf'
sbt 'runMain org.clulab.dynet.Metal -train '"$MODELL"' -conf org/clulab/mtl-en-depsl.conf'

