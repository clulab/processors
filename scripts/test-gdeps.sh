#!/bin/bash

#
# Script to test a syntactic dependency parser model for the graph parser
#

if [ "$#" -ne 1 ]; then
    echo "You must provide the model name in the command line!"
    exit 1
fi

MODEL=$1

sbt 'runMain org.clulab.dynet.Metal -test '"$MODEL"' -conf org/clulab/mtl-en-gdeps.conf'

