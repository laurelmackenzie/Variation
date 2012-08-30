#!/usr/bin/env python
"""
Analysis of contexts for the postnasal plosive deletion change.
Constantine Lignos
August 2012
"""

# Copyright (C) 2012 Constantine Lignos
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# at your option any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from __future__ import division
import sys
import argparse
import re
from collections import defaultdict
from math import log, floor

from lexinfo.cmudictreader import CMUDict
from eng_syll import eng_syllabify, ENG_CONSONANTS, ENG_ONSETS
from syllabification import get_onset

SUFFIXES = set(('mb', 'ng', 'mn'))
SUFFIX_LAST_PHONEME = {'mb': 'B', 'ng': 'G', 'mn': 'N'}

def tolerated_exceptions(n):
    """Compute the number of tolerated exceptions for n."""
    return int(floor(n / log(n)))


def tolerated_rate(n):
    """Compute the rate of application required for a productive rule."""
    return (1.0 - (tolerated_exceptions(n) / n))


def analyze(inpath, prondict):
    """Analyze the given file."""
    prons = CMUDict(prondict)
    infile = open(inpath, 'rU')
    phrase_contexts = set()
    word_contexts = set()
    word_exception_contexts = set()
    for line in infile:
        tokens = line.strip().split()
        # Count token_n, token_n+1 pairs, which excludes the last token
        for token, next_token in zip(tokens, tokens[1:]):
            token_suffix = None
            for suffix in SUFFIXES:
                if token.endswith(suffix):
                    token_suffix = suffix
                    break # Can only end in one suffix
            
            if not token_suffix:
                continue
            
            # Get the first phoneme of the next token
            try:
                next_syll = eng_syllabify(prons[next_token.lower()])[0]
            except KeyError:
                continue
            
            # Always count toward the word level
            word_contexts.add(token)
            
            # See if the last phoneme can re-syllabify to the next work
            next_onset = get_onset(next_syll, ENG_CONSONANTS)
            resyll_onset = tuple([SUFFIX_LAST_PHONEME[token_suffix]] + list(next_onset))
            
            # If resyllabification wouldn't bleed the deletion, it's a valid phrase level context
            if resyll_onset not in ENG_ONSETS:
                phrase_contexts.add(token)
            else:
                word_exception_contexts.add(token)

        # Count the last token (phrase-final occurrences)
        last_token = tokens[-1]
        for suffix in SUFFIXES:
            if last_token.endswith(suffix):
                # Counts toward both levels
                phrase_contexts.add(last_token)
                word_contexts.add(last_token)
                break # Can only end in one suffix
    
    # Validate counts
    assert word_contexts >= phrase_contexts, \
        "Word contexts should be a superset of phrase contexts"
    
    # Print simple counts
    print "All word participants:", len(word_contexts)
    print "All overlapped participants:", len(phrase_contexts & word_contexts)
    print "Apparent word exceptions:", len(word_exception_contexts)
    
    # Now remove items that were unreliable
    reliable_word_contexts = word_contexts - word_exception_contexts
    reliable_word_exception_contexts = word_exception_contexts - word_contexts
    print "Reliable word participants:", len(reliable_word_contexts)
    print "Reliable word exceptions:", len(reliable_word_exception_contexts)

    word_max_exceptions = tolerated_exceptions(len(word_contexts))
    print "Word tolerated exceptions:", word_max_exceptions
    print "Word observed exceptions:", len(word_contexts - phrase_contexts) 


def main():
    """Analyze contexts for postnasal plosive deletion change."""
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.add_argument('file', help='file to analyze')
    parser.add_argument('prondict', help='pronunciation dictionary in cmudict format')
    args = parser.parse_args()
    analyze(args.file, args.prondict)


if __name__ == "__main__":
    main()
