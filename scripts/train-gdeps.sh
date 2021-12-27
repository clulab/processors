#!/bin/bash

#
# Script to train a syntactic dependency parser model
#

if [ "$#" -ne 1 ]; then
    echo "You must provide the model name in the command line!"
    exit 1
fi

MTL_MODEL="$1"

echo "Using $MTL_MODEL for the joint head/label model."

sbt 'runMain org.clulab.dynet.Metal -train '"$MTL_MODEL"' -conf org/clulab/mtl-en-gdeps.conf'


