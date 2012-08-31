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
import random
from collections import Counter
from math import log, floor

from lexinfo.cmudictreader import CMUDict
from eng_syll import eng_syllabify, ENG_CONSONANTS, ENG_ONSETS
from syllabification import get_onset

# Seed for replicability
random.seed(0)

SUFFIXES = set(('mb', 'ng', 'mn'))
SUFFIX_LAST_PHONEME = {'mb': 'B', 'ng': 'G', 'mn': 'N'}

def tolerated_exceptions(n):
    """Compute the number of tolerated exceptions for n."""
    return int(floor(n / log(n)))


def tolerated_rate(n):
    """Compute the rate of application required for a productive rule."""
    return (1.0 - (tolerated_exceptions(n) / n))


def is_productive(participants, exceptions):
    """Return whether the numbers of exceptions is at or below the tolerated threshold."""
    return exceptions <= tolerated_exceptions(participants + exceptions)


def strategy_str(name, participant_set, exception_set):
    """Return a string representing results for a strategy."""
    n_participants = len(participant_set)
    n_exceptions = len(exception_set)
    return "\n".join((name, "Participants: %d, Exceptions: %d" % (n_participants, n_exceptions),
                      "Tolerance: %d" % (tolerated_exceptions(n_participants + n_exceptions)),
                      "Productive?: %s" % is_productive(n_participants, n_exceptions)))


def analyze(inpath, prondict, misperceive_rate):
    """Analyze the given file."""
    prons = CMUDict(prondict)
    infile = open(inpath, 'rU')
    phrase = set()
    word = set()
    word_exceptions = set()
    word_exceptions_perceived = set()
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
            word.add(token)

            # See if the last phoneme can re-syllabify to the next work
            next_onset = get_onset(next_syll, ENG_CONSONANTS)
            resyll_onset = tuple([SUFFIX_LAST_PHONEME[token_suffix]] + list(next_onset))
            
            # If resyllabification wouldn't bleed the deletion, it's a valid phrase level context
            if resyll_onset not in ENG_ONSETS:
                phrase.add(token)
            else:
                word_exceptions.add(token)
                # With some probability, allow it to be heard as deleted, so it isn't counted
                # as an exception. We express this as being counted as an exception at 1-p
                if random.random() > misperceive_rate:
                    word_exceptions_perceived.add(token)

        # Count the last token (phrase-final occurrences)
        last_token = tokens[-1]
        for suffix in SUFFIXES:
            if last_token.endswith(suffix):
                # Counts toward both levels
                phrase.add(last_token)
                word.add(last_token)
                break # Can only end in one suffix
    
    # Validate counts
    assert word >= phrase, \
        "Word contexts should be a superset of phrase contexts"
    assert phrase <= (word | word_exceptions), \
        "All phrase contexts should appear in the union of phrase participants and exceptions"
    
    # Create derived sets
    word_participants = word & phrase
    word_participants_only = word_participants - word_exceptions
    word_exceptions_only = word_exceptions - word_participants
    word_exceptions_perceived_only = word_exceptions_perceived - word_participants
    word_both = word_exceptions & word_participants
    assert word == (word_participants_only | word_exceptions_only | word_both), \
        "Participants/exceptions/both should cover all word contexts"

    # Print simple counts
    print "Possible word participants:", len(word)
    print "All observed word participants:", len(word_participants)
    print "All observed word exceptions:", len(word_exceptions)
    print
    print "Word participant only:", len(word_participants_only)
    print "Word exception only:", len(word_exceptions_only)
    print "Word exception and participant:", len(word_both)
    print

    # Remove items that were unreliable
    print strategy_str("Strategy: Only count reliable items", word_participants_only,
                       word_exceptions_only)
    print

    # Conservative reanalysis strategy
    print strategy_str("Strategy: Reanalyze when certain", word_participants_only, 
                       word_exceptions)
    print

    # Aggressive reanalysis strategy
    print strategy_str("Strategy: Reanalyze when possible", word_participants, 
                       word_exceptions_only)
    print

    # Aggressive reanalysis strategy with mishearing
    print strategy_str("Strategy: Reanalyze when possible, occasionally misperceiving", 
                       word_participants, word_exceptions_perceived_only)
    print


def main():
    """Analyze contexts for postnasal plosive deletion change."""
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.add_argument('file', help='file to analyze')
    parser.add_argument('prondict', help='pronunciation dictionary in cmudict format')
    parser.add_argument('deletion_perception_rate', type=float,
                        help='rate at which a deletion is falsely perceived')
    args = parser.parse_args()
    analyze(args.file, args.prondict, args.deletion_perception_rate)


if __name__ == "__main__":
    main()
