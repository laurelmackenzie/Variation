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

PHON_SEQUENCES = set(('mb', 'ng', 'mn'))
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

def update_stem(token, phon_sequences, postnasal_stems, stem, stem_exceptions):
    """Updates stem counts for a token.

    Ugly function, but better than pasting it twice.
    """
    # Check for whether this looks like an exception to the stem level
    if is_postnasal_stem_exception(token, PHON_SEQUENCES, postnasal_stems):
        stem.add(token)
        stem_exceptions.add(token)
    else:
        # See if it ends in one of the sequences. This would be the
        # same as word level, so it would not be an exception.
        for suffix in PHON_SEQUENCES:
            if token.endswith(suffix):
                stem.add(token)
                break


_MIN_STEM_LENGTH = None
def is_postnasal_stem_exception(token, phon_sequences, stems):
    """Return whether postnasal deletion should apply at the stem but not the word level."""
    # Optimization: cache the minimum stem length so we only compute it once.
    global _MIN_STEM_LENGTH # We could do the global on the fly, but boy is that dodgy.
    if _MIN_STEM_LENGTH is None:
        _MIN_STEM_LENGTH = min(len(stem) for stem in stems)

    # Check for whether the token starts with anything in the stems
    stem_suffixes = ((token[:idx], token[idx:]) for idx in range(_MIN_STEM_LENGTH, len(token)))
    for stem, suffix in stem_suffixes:
        # 's' suffixes would trigger coda simplification, so they don't count
        if suffix not in ('s', "'s") and stem in stems:
            return True

    # False if no matching stem was found
    return False


def analyze(inpath, prondict, misperceive_rate, stempath):
    """Analyze the given file."""
    prons = CMUDict(prondict)
    postnasal_stems = set(line.strip() for line in open(stempath, 'rU'))
    infile = open(inpath, 'rU')
    phrase = set()
    word = set()
    stem = set()
    word_exceptions = set()
    word_exceptions_perceived = set()
    stem_exceptions = set()
    for line in infile:
        tokens = line.strip().split()
        # Count token_n, token_n+1 pairs, which excludes the last token
        for token, next_token in zip(tokens, tokens[1:]):
            # Get the first phoneme of the next token
            try:
                next_syll = eng_syllabify(prons[next_token.lower()])[0]
            except KeyError:
                continue

            # At this point, it's okay to count the word for the stem level
            # as there's no chance of skipping it based on pron

            # See if it should count toward stem level
            update_stem(token, PHON_SEQUENCES, postnasal_stems, stem, stem_exceptions)

            # Check word ending for applicattion at word/phrase level
            token_suffix = None
            for suffix in PHON_SEQUENCES:
                if token.endswith(suffix):
                    token_suffix = suffix
                    break # Can only end in one suffix
            
            if not token_suffix:
                continue

            # Always count toward the word level, as the word ends in the suffix.
            word.add(token)

            # See whether it counts toward phrase level
            # See if the last phoneme can re-syllabify to the next word
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
        for suffix in PHON_SEQUENCES:
            if last_token.endswith(suffix):
                # Counts toward both levels
                phrase.add(last_token)
                word.add(last_token)
                # See if it should count toward stem level
                update_stem(token, PHON_SEQUENCES, postnasal_stems, stem, stem_exceptions)
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
    stem_participants = stem & word
    stem_participants_only = stem_participants - stem_exceptions
    stem_exceptions_only = stem_exceptions - stem_participants
    stem_both = stem_exceptions & stem_participants
    assert stem == (stem_participants_only | stem_exceptions_only | stem_both), \
        "Participants/exceptions/both should cover all stem contexts: %s" % (stem ^ (stem_participants_only | stem_exceptions_only | stem_both))

    # Print simple counts
    print "Phrase participants:", len(phrase)
    print
    print "Possible word participants:", len(word)
    print "All observed word participants:", len(word_participants)
    print "All observed word exceptions:", len(word_exceptions)
    print "Word participant only:", len(word_participants_only)
    print "Word exception only:", len(word_exceptions_only)
    print "Word exception and participant:", len(word_both)
    print
    print "Possible stem participants:", len(stem)
    print "All observed stem participants:", len(stem_participants)
    print "All observed stem exceptions:", len(stem_exceptions)
    print "Stem participant only:", len(stem_participants_only)
    print "Stem exception only:", len(stem_exceptions_only)
    print "Stem exception and participant:", len(stem_both)
    print


    print '*' * 20, "Word Level", '*' * 20
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

    print '*' * 20, "Stem Level", '*' * 20
    print "All exceptions:", ", ".join(stem_exceptions)
    # Remove items that were unreliable
    print strategy_str("Strategy: Only count reliable items", stem_participants_only,
                       stem_exceptions_only)
    print

    # Conservative reanalysis strategy
    print strategy_str("Strategy: Reanalyze when certain", stem_participants_only, 
                       stem_exceptions)
    print

    # Aggressive reanalysis strategy
    print strategy_str("Strategy: Reanalyze when possible", stem_participants, 
                       stem_exceptions_only)
    print



def main():
    """Analyze contexts for postnasal plosive deletion change."""
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.add_argument('file', help='file to analyze')
    parser.add_argument('prondict', help='pronunciation dictionary in cmudict format')
    parser.add_argument('deletion_perception_rate', type=float,
                        help='rate at which a deletion is falsely perceived')
    parser.add_argument('stems', help='file of stems that should show postnasal delection')
    args = parser.parse_args()
    analyze(args.file, args.prondict, args.deletion_perception_rate, args.stems)


if __name__ == "__main__":
    main()
