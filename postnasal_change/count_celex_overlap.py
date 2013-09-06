#!/usr/bin/env python
"""
Analysis of overlap between stem and word levels per language.
Constantine Lignos
February 2013
"""

# Copyright (C) 2013 Constantine Lignos
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
import re
from collections import defaultdict

from lexinfo.celexreader import CelexDB, SUPPORTED_LANGS

EXCLUSION_DEBUG = False
HEAD_DEBUG = False
RATIO_DEBUG = True
HEAD_RE = re.compile("-| |'")


def analyze(celex_root, lang):
    """Analyze the stem/word overlap in CELEX for the given language."""
    celex = CelexDB(celex_root, lang)
    # For each root group count the frequency of the head word compared to inflectionally
    # related members.
    # The only way to do this safely is to get words that are given as stems in an
    # analysis and then look at their inflectional lemma.
    head_children = defaultdict(set)
    roots = set(celex.root_lemmas.keys())
    sorted_lemma_heads = sorted(celex.lemma_heads.items(), key=lambda x: x[1].lower())
    
    for lemma, head in sorted_lemma_heads:
        if head not in roots:
            if EXCLUSION_DEBUG:
                print "Not root:", head
            continue
        
        # Skip heads that are hyphenated, multi-word, or have apostrophes 
        if HEAD_RE.search(head):
            if EXCLUSION_DEBUG:
                print "Bad head:", head
            continue

        # Include children that are suffixed: do not end with the head word.
        # This automatically excludes the head itself; if the exclusion criteria are
        # changed, make sure to require word != head.
        children = set(word for word in celex.lemma_words[lemma]
                       if not word.endswith(head))
        head_freq = celex.word_freqs[head]
        # Skip zero frequency words
        if not head_freq or not children:
            continue

        head_children[head].update(children)

    head_children_freqs = {}
    for head, children in sorted(head_children.items(), key=lambda x: x[0].lower()):
        head_freq = celex.word_freqs[head]
        if HEAD_DEBUG:
            print "{}: {}".format(head, ', '.join(sorted(children)))

        children_freq = sum(celex.word_freqs[word] for word in children)
        head_children_freqs[head] = (head_freq, children_freq)

        if RATIO_DEBUG:
            print ("{} ({}): ".format(head, head_freq) +
                   ", ".join("{} ({})".format(child, celex.word_freqs[child])
                             for child in children))

    # Unsmoothed. Applying smoothing to frequency ratios can result in insane results
    ratios = _frequency_ratio(head_children_freqs, False)
    mean_ratio = sum(ratios.values()) / len(ratios)
    print "Zeroes excluded head/child frequency ratio: %.2f" % mean_ratio


def _frequency_ratio(head_child_freqs, smooth_zero):
    """Return the ratio between head and children freq., optionally with laplace smoothing."""
    return ({head: (head_freq + 1) / (children_freq + 1)
            for head, (head_freq, children_freq) in head_child_freqs.items()}
            if smooth_zero else
            {head: (head_freq) / (children_freq)
            for head, (head_freq, children_freq) in head_child_freqs.items() if children_freq})


def main():
    """Analyze overlap between word and stem levels in CELEX."""
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.add_argument('celex_root', help='root of the CELEX database')
    parser.add_argument('lang', help='language to analyze', choices=SUPPORTED_LANGS)
    args = parser.parse_args()
    analyze(args.celex_root, args.lang)


if __name__ == "__main__":
    main()
