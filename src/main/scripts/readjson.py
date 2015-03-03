#!/usr/bin/env python

import sys
import json

filename = sys.argv[1]

with open(filename) as f:
    data = json.load(f)
    for item in data:
        print item['text']
