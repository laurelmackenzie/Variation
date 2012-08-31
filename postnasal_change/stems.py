#!/usr/bin/env python
"""
Dump possible stems for the postnasal plosive deletion change.
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

import sys

words = set(word for line in sys.stdin for word in line.strip().split())

for word in sorted(words):
    # Different cases for finding stems based on suffix:
    # 1. mb/mn: Anything that ends in these at all
    if word.endswith('mb') or word.endswith('mn'):
        print word
    # -ng: Complicated because of -ing. Basically, take it
    # if any of the following:
    # 1. Ends in -ng but not -ing
    # 2. Ends in -ing but the stem, stem+e, and stem - repeated last letter 
    # are not words
    elif ((word.endswith('ng') and not word.endswith('ing')) or
          (word.endswith('ing') and 
           not(word[:-3] in words or 
               word[:-3] + 'e' in words or
               (word[-4] == word[-5] and word[:-4] in words)))):
        print word

        
