#!/bin/bash

#
# Script to run the temporal averaging code
#

if [ "$#" -lt 3 ]; then
    echo "You must average at least 2 models!"
    exit 1
fi

sbt 'runMain org.clulab.dynet.ModelAveraging '"$@"'

