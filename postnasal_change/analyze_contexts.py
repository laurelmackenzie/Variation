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
import argparse
from collections import Counter
from math import log, floor
from itertools import takewhile

from lexinfo.cmudictreader import CMUDict

DEBUG = False

PHON_SEQUENCES = set(("ng",))
SUFFIX_LAST_PHONEME = {"ng": "G"}
COUNT_CONSERVATIVE = "conservative"
COUNT_AGGRESSIVE = "aggressive"
COUNT_CAUTIOUS = "cautious"
COUNT_FREQUENT = "frequent"
COUNT_STRATEGIES = [COUNT_CONSERVATIVE, COUNT_AGGRESSIVE, COUNT_CAUTIOUS,
                    COUNT_FREQUENT]

# General English phonological constants
ENG_CONSONANTS = set(('B', 'CH', 'D', 'DH', 'F', 'G', 'HH',
    'JH', 'K', 'L', 'M', 'N', 'NG', 'P', 'R', 'S', 'SH', 'T', 'TH', 'V', 'W',
    'Y', 'Z', 'ZH'))
ENG_VOWELS = set(('AA', 'AE', 'AH', 'AO', 'AW', 'AY', 'EH',
    'ER', 'EY', 'IH', 'IY', 'OW', 'OY', 'UH', 'UW'))
ENG_ONSETS = set(
    [(cons,) for cons in ENG_CONSONANTS] +
    [('P', 'R'), ('T', 'R'), ('K', 'R'), ('B', 'R'),
    ('D', 'R'), ('G', 'R'), ('F', 'R'), ('TH', 'R'), ('SH', 'R'), ('P', 'L'),
    ('K', 'L'), ('B', 'L'), ('G', 'L'), ('F', 'L'), ('S', 'L'), ('T', 'W'),
    ('K', 'W'), ('D', 'W'), ('S', 'W'), ('S', 'P'), ('S', 'T'), ('S', 'K'),
    ('S', 'F'), ('S', 'M'), ('S', 'N'), ('G', 'W'), ('SH', 'W'),
    ('S', 'P', 'R'), ('S', 'P', 'L'), ('S', 'T', 'R'), ('S', 'K', 'R'),
    ('S', 'K', 'W'), ('S', 'K', 'L'), ('TH', 'W'), ('P', 'Y'),
    ('K', 'Y'), ('B', 'Y'), ('F', 'Y'), ('HH', 'Y'), ('V', 'Y'), ('TH', 'Y'),
    ('M', 'Y'), ('S', 'P', 'Y'), ('S', 'K', 'Y'), ('G', 'Y')])


def _remove_stress(phoneme):
    """Remove stress from a CMUDict phoneme.

    >>> _remove_stress('AW1')
    'AW'
    >>> _remove_stress('AW0')
    'AW'
    >>> _remove_stress('AW')
    'AW'
    """
    return "".join(char for char in phoneme if not char.isdigit())


def _get_first_stress(phonemes):
    """Return the first stress marker, or -1 if none found.

    >>> _get_first_stress(['K', 'AA1', 'N', 'S', 'T', 'AH0', 'N', 'T', 'IY2', 'N'])
    1
    >>> _get_first_stress(['T', 'UW1'])
    1
    >>> _get_first_stress(['T', 'UW0'])
    0
    >>> _get_first_stress(['T', 'UW'])
    -1
    """
    # pylint: disable=W0120
    for phoneme in phonemes:
        digits = [char for phoneme in phonemes for char in phoneme
                  if char.isdigit()]
        if digits:
            return int(digits[0])
    else:
        return -1


def _is_consonant(phoneme):
    """Return whether a phoneme is a consonant.

    >>> _is_consonant('K')
    True
    >>> _is_consonant('AW')
    False
    >>> _is_consonant('AW1')
    False
    >>> _is_consonant('AW0')
    False
    """
    return phoneme in ENG_CONSONANTS


def _get_onset(phonemes):
    """Return all consonants that precede the first vowel.

    >>> _get_onset(['K', 'AW1'])
    ['K']
    >>> _get_onset(['K', 'AW'])
    ['K']
    >>> _get_onset(['AW0', 'T'])
    []
    """
    return list(takewhile(_is_consonant, phonemes))


def tolerated_exceptions(n):
    """Compute the number of tolerated exceptions for n."""
    return int(floor(n / log(n))) if n else 0


def tolerated_rate(n):
    """Compute the rate of application required for a productive rule."""
    return (1.0 - (tolerated_exceptions(n) / n)) if n else 0


def is_productive(participants, exceptions):
    """Return whether the numbers of exceptions is at or below the tolerated threshold."""
    return exceptions <= tolerated_exceptions(participants + exceptions)


class ExceptionCounter(object):
    """Count exceptions given a strategy.

    >>> exc = ExceptionCounter()
    >>> exc.count_participant('a')
    >>> exc.count_participant('a')
    >>> exc.count_exception('a')
    >>> exc.count_participant('b')
    >>> exc.count_exception('b')
    >>> exc.count_exception('b')
    >>> exc.count_participant('c')
    >>> exc.count_exception('d')
    >>> sorted(exc.participants(COUNT_CONSERVATIVE))
    ['c']
    >>> sorted(exc.exceptions(COUNT_CONSERVATIVE))
    ['a', 'b', 'd']
    >>> sorted(exc.participants(COUNT_AGGRESSIVE))
    ['a', 'b', 'c']
    >>> sorted(exc.exceptions(COUNT_AGGRESSIVE))
    ['d']
    >>> sorted(exc.participants(COUNT_CAUTIOUS))
    ['c']
    >>> sorted(exc.exceptions(COUNT_CAUTIOUS))
    ['d']
    >>> sorted(exc.participants(COUNT_FREQUENT))
    ['a', 'c']
    >>> sorted(exc.exceptions(COUNT_FREQUENT))
    ['b', 'd']

    """

    def __init__(self):
        self.exception_counts = Counter()
        self.participant_counts = Counter()

    def count_exception(self, item):
        """Count the given item as an exception."""
        self.exception_counts[item] += 1

    def count_participant(self, item):
        """Count the given item as an exception."""
        self.participant_counts[item] += 1

    def counts(self, strategy):
        """Return counts of (exceptions, participants) for the given strategy."""
        return (len(self.exceptions(strategy)),
                len(self.participants(strategy)))

    def exceptions(self, strategy):
        """Return the set of exceptions for the given strategy."""
        if strategy == COUNT_CONSERVATIVE:
            return set(self.exception_counts)
        elif strategy == COUNT_AGGRESSIVE or strategy == COUNT_CAUTIOUS:
            return set(self.exception_counts) - set(self.participant_counts)
        elif strategy == COUNT_FREQUENT:
            return set(_morefreq_items(self.exception_counts, self.participant_counts))
        else:
            raise ValueError("Unknown strategy: {}".format(strategy))

    def participants(self, strategy):
        """Return the set of participants for the given strategy."""
        if strategy == COUNT_CONSERVATIVE or strategy == COUNT_CAUTIOUS:
            return set(self.participant_counts) - set(self.exception_counts)
        elif strategy == COUNT_AGGRESSIVE:
            return set(self.participant_counts)
        elif strategy == COUNT_FREQUENT:
            return set(_morefreq_items(self.participant_counts, self.exception_counts))
        else:
            raise ValueError("Unknown strategy: {}".format(strategy))

    def is_productive(self, strategy):
        """Return whether the generalization is productive."""
        return is_productive(len(self.participants(strategy)), len(self.exceptions(strategy)))

    def tolerated_exceptions(self, strategy):
        """Return the number of tolerated exceptions."""
        return tolerated_exceptions(len(self.participants(strategy)) + len(self.exceptions(strategy)))

    def summary(self, strategy):
        """Return a string representing results for a strategy."""
        n_participants = len(self.participants(strategy))
        n_exceptions = len(self.exceptions(strategy))
        return "\n".join(("Strategy: {}".format(strategy.capitalize()),
                          "Participants: {}, Exceptions: {}".format(n_participants, n_exceptions),
                          "Tolerance: {}".format(self.tolerated_exceptions(strategy)),
                          "Productive?: {}".format(is_productive(n_participants, n_exceptions))))

    def all_summary(self):
        """Return the summary of all strategies."""
        return "\n\n".join(self.summary(strategy) for strategy in COUNT_STRATEGIES)


def _morefreq_items(counter1, counter2):
    """Return the items with greater frequency in counter1 than counter2."""
    return [item for item in counter1 if counter1[item] > counter2[item]]


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


def analyze(inpath, prondict, stempath):
    """Analyze the given file."""
    prons = CMUDict(prondict)
    postnasal_stems = set(line.strip() for line in open(stempath, 'rU'))
    infile = open(inpath, 'rU')
    phase1 = ExceptionCounter()
    phase1_restrict = ExceptionCounter()
    phase2 = ExceptionCounter()

    for line in infile:
        tokens = line.strip().split()
        # Count token_n, token_n+1 pairs, which excludes the last token
        for idx, token in enumerate(tokens):
            token = token.lower()
            next_token = tokens[idx + 1].lower() if idx < (len(tokens) - 1) else None
            next_pron = prons[next_token] if next_token and next_token in prons else None

            # Skip the token if there is one but no pron for it. We don't want any data
            # that we cannot examine at the phrase level.
            if next_token and not next_pron:
                continue

            # See if it should count toward stem level
            # TODO: Figure out what's going on in the stem level
            # update_stem(token, PHON_SEQUENCES, postnasal_stems, stem, stem_exceptions)

            # Check word ending for application at word/phrase level
            token_suffix = None
            for suffix in PHON_SEQUENCES:
                if token.endswith(suffix):
                    token_suffix = suffix
                    break # Can only end in one suffix
            else:
                # If it doesn't end with a suffix of interest, it isn't a
                # candidate at the phrase or word levels
                continue

            # See whether it counts toward phrase level
            # See if the last phoneme can re-syllabify to the next word
            next_onset = _get_onset(next_pron) if next_pron else None
            if next_onset is not None:
                resyll_onset = tuple([SUFFIX_LAST_PHONEME[token_suffix]] + next_onset)
                next_stress = _get_first_stress(next_pron)

            # Unrestricted phrase level deletion

            # If unrestricted resyllabification wouldn't bleed the deletion, deletion
            # will occur at the phrase level. next_onset = None marks phrase-final,
            # so that always deletes.
            if next_onset is None or resyll_onset not in ENG_ONSETS:
                # In phase 1, this means a participant because word and phrase level deletion agree
                phase1.count_participant(token)
                phase1_restrict.count_participant(token)
                if DEBUG:
                    print "Both participant:"
                    if next_onset:
                        print token, next_pron
                    else:
                        print token
            elif next_stress or next_onset:
                # In restricted resyllabification, following stress or onset blocks resyllabification,
                # so phrase-level deletion ocrrus.
                phase1_restrict.count_participant(token)
                # Consider restricted resyllabification, where resyllabification is blocked
                # In the unrestricted case, this is an exception as phrase level deletion would not
                # occur here.
                phase1.count_exception(token)
                if DEBUG:
                    print "Restricted participant:"
                    print token, next_pron
            else:
                # Resyllabification blocks deletion
                phase1.count_exception(token)
                phase1_restrict.count_exception(token)
                if DEBUG:
                    print "Exception:"
                    print token, next_pron

            if DEBUG:
                print

    # Print simple counts
    print '*' * 20, "Phase 1 Unrestricted Resyllabification", '*' * 20
    print phase1.all_summary()
    print
    print '*' * 20, "Phase 1 Restricted Resyllabification", '*' * 20
    print phase1_restrict.all_summary()
    print
    print '*' * 20, "Phase 2", '*' * 20
    print phase2.all_summary()


def main():
    """Analyze contexts for postnasal plosive deletion change."""
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.add_argument('file', help='file to analyze')
    parser.add_argument('prondict', help='pronunciation dictionary in cmudict format')
    parser.add_argument('stems', help='file of stems that should show postnasal delection')
    args = parser.parse_args()
    analyze(args.file, args.prondict, args.stems)


if __name__ == "__main__":
    main()
