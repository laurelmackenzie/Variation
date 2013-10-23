#!/usr/bin/env python
"""
Output good stems for postnasal plosive deletion based on the output
of finding them.

"""

# Copyright (C) 2012-2013 Constantine Lignos
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

import sys
from collections import defaultdict, Counter

TARGET = "ng"
# These do not include -s and -ed because of coda simplification
GOLD_SUFFIXES = \
    set([
        "er",
        "ing",
        "ings",
        "ly",
        "ers",
        "es",
        "est",
        "er's",
        "y",
        ])

# These represent forms in which the /g/ appears among multiple
# word-level affixes
BAD_SUFFIXES = \
    set([
        "ingly",
        ])

BAD_STEMS = \
    set([
        "fing",  # finger
        "angel",  # angeles
        "ang",  # anger
        "beng", # bengy
        "dilling",  # dillinger
        "peading",  # peadinger
        "saling",  # salinger
        "messeng",  # messenger
        "ging",  # ginger
        "hing",  # hinges
        "passeng",  # passenger
        "sting",  # stingy
        "spong",  # sponges
        "plung",  # plunger
        "challeng", # challenging
        ])


words = set(line.strip().split()[1] for line in sys.stdin)
# These data structures are for debugging only
stem_words = defaultdict(list)
suffixes = Counter()

for word in sorted(words):
    # Find "ng" if it's there. This will find the first one, so it won't
    # be in the suffix if there's one in the stem.
    idx = word.find(TARGET)
    suffix =  word[idx + len(TARGET):]

    # If it wasn't found, was only found at the end, or is only
    # followed by -s, skip it.
    if idx in (-1, len(word) - len(TARGET)) or suffix == "s":
        continue

    # Skip bad stems, or compounds that end in the bad stems (e.g.,
    # blackfinger), or bad suffixes
    stem = word[:-len(suffix)]
    if (stem in BAD_STEMS or any(stem.endswith(bad) for bad in BAD_STEMS) or
            any(word.endswith(bad) for bad in BAD_SUFFIXES)):
        continue

    # Record it if the suffix is good
    if suffix in GOLD_SUFFIXES:
        print ",".join((word, stem))
        stem_words[stem].append(word)
        suffixes[suffix] += 1
