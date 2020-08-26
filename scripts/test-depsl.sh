#!/bin/bash

#
# Script to test a syntactic dependency parser model for dependency labels only
#

if [ "$#" -ne 1 ]; then
    echo "You must provide the model name in the command line!"
    exit 1
fi

MODELL=$1

sbt 'runMain org.clulab.dynet.Metal -test '"$MODELL"' -conf org/clulab/mtl-en-depsl.conf'

