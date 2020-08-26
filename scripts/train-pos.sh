#!/bin/bash

#
# Script to train a POS tagger, syntactic chunker, and SRL predicates
#

if [ "$#" -ne 1 ]; then
    echo "You must provide the model name in the command line!"
    exit 1
fi

MODEL=$1

sbt 'runMain org.clulab.dynet.Metal -train '"$MODEL"' -conf org/clulab/mtl-en-pos-chunk-srlp.conf'

