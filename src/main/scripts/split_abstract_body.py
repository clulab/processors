#!/usr/bin/env python

# this script reads the text and standoff files produced by nxml2txt and extracts body and abstract
# ignoring section titles, figures and tables

import sys
import re
import logging
logging.basicConfig(format='%(levelname)s:%(message)s', level=logging.DEBUG)

def first(it):
    return next(iter(it))

def span_contains(span1, span2):
    return span2[0] >= span1[0] and span2[1] <= span1[1]

def get_spans(name, standoff):
    pat = r'\t' + re.escape(name) + r' (?P<start>\d+) (?P<end>\d+)'
    for m in re.finditer(pat, standoff):
        start = int(m.group('start'))
        end = int(m.group('end'))
        yield (start, end)

def keep_spans(span, ignore):
    start, end = span
    for s in sorted(ignore):
        yield (start, s[0])
        start = s[1]
    yield (start, end)

def extract(text, standoff, tag, ignore_tags):
    span = first(get_spans(tag, standoff))
    ignore = []
    for t in ignore_tags:
        for s in get_spans(t, standoff):
            if span_contains(span, s):
                ignore.append(s)

    # Remove unwanted sections (materials, methods, supplementary materials)
    #if tag == 'body':
    #    ignore.extend(remove_sections(standoff))

    result = ''
    for (start, end) in keep_spans(span, ignore):
        result += text[start:end]

    # FIXME This not always holds true! - ENRIQUE: This will add a period at the end of the titles
    #locations = get_spans('title', standoff) # Get the title tags
    #locations = [s[1] for s in locations if span_contains(span, s)] # Keep those that really belong to the section
    #
    # Extract the title text
    #for s, i in zip(locations, xrange(len(locations))):
    #    # Get the relative span of the title
    #    r = s-span[0]+i
    #    # Insert a period
    #    result = result[:r] + '.' + result[r:]

    return result

def remove_sections(standoff):
    ''' Removes sections from the body by matching certain title heuristics'''

    prev_span = None

    pat = r'\tsec (?P<start>\d+) (?P<end>\d+)\t(?P<title>[^\t]+)'
    for m in re.finditer(pat, standoff):
        start = int(m.group('start'))
        end = int(m.group('end'))
        text = m.group('title')

        tokens = text.split('\\n')

        title = tokens[0].lower() if len(tokens) > 1 else ''


        if prev_span is None:
            prev_span = (start, end)
        elif span_contains(prev_span, (start, end)):
            continue
        else:
            prev_span = (start, end)

        matches = 0
        lookup = ('material', 'method', 'and', 'suplementary', 'supporting', 'information')
        avoid = ('preliminary', 'finding', 'result', 'methodology')

        for word in lookup:
            if word in title:
                matches += 1

        words = len(title.split())

        if words > 4 or any(x in title for x in avoid):
            print "Didn't remove: %s"%title
            continue
        elif matches >= 2:
            print "Removed: %s"%title
            yield (start, end)
        elif matches == 1 and words <= 2:
            print "Removed: %s"%title
            yield (start, end)

        elif any(x in title for x in ('materials', 'methods')) and any(x in title for x in ('of', 'for')):
            print "Removed: %s"%title
            yield (start, end)
        else:
            print "Didn't remove: %s"%title



def extract_body(text, standoff):
    #return extract(text, standoff, 'body', ['title', 'table-wrap', 'fig'])
    return extract(text, standoff, 'body', ['object-id'])

def extract_abstract(text, standoff):
    #return extract(text, standoff, 'abstract', ['title', 'table-wrap', 'fig'])
    return extract(text, standoff, 'abstract', ['object-id'])

if __name__ == '__main__':
    textfn = sys.argv[1]
    sofn = sys.argv[2]
    abstractfn = sys.argv[3]
    bodyfn = sys.argv[4]

    with open(textfn) as f:
        text = f.read()

    with open(sofn) as f:
        standoff = f.read()

    try:
        abstract = extract_abstract(text, standoff)
        with open(abstractfn, 'w') as f:
            f.write(abstract.strip())
    except StopIteration:
        logging.warning('no abstract')

    try:
        body = extract_body(text, standoff)
        with open(bodyfn, 'a') as f:
            f.write(body.strip())
    except StopIteration:
        logging.warning('no body')
