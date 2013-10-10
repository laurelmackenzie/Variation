#!/usr/bin/env python
"""
Dump possible stems for the postnasal plosive deletion change.
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
from collections import Counter

AFFIX = "ng"
GOLD_SUFFIXES = set([
s,
er,
's,
ing,
ed
ly,
les
ers
a
es
est
ham
ton
er's
y
led
os
ling
us
])

BAD_STEMS = set([
'fing'

words = set(line.strip().split()[1] for line in sys.stdin)
stem_words = {}
suffixes = Counter()
good_suffixes

for word in sorted(words):
    # Find "ng" if it's there. This will find the first one, so it won't
    # be in the suffix if there's one in the stem.
    idx = word.find(AFFIX)
    # If it wasn't found, or was only found at the end, skip it.
    if idx == -1 or idx == len(word) - len(AFFIX):
        continue

    suffixes[word[idx + 2:]] += 1

for suff, count in suffixes.most_common():
    print suff

