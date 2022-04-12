#!/bin/bash

#
# Script to train a syntactic dependency parser model
#

if [ "$#" -ne 1 ]; then
    echo "You must provide the model name in the command line!"
    exit 1
fi

MODELH="$1"

echo "Using $MODELH for unlabeled dependency parsing"

sbt 'runMain org.clulab.dynet.Metal -train '"$MODELH"' -conf org/clulab/mtl-en-depsh.conf'


