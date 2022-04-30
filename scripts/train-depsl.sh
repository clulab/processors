#!/bin/bash

#
# Script to train a syntactic dependency parser model
#

if [ "$#" -ne 1 ]; then
    echo "You must provide the model name in the command line!"
    exit 1
fi

MODELL="$1"

echo "Using $MODELL for the dependency labels."

sbt 'runMain org.clulab.dynet.Metal -train '"$MODELL"' -conf org/clulab/mtl-en-depsl.conf'


