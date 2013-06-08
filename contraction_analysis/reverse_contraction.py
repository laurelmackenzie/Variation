#!/usr/bin/env python
"""Turn contractions into their uncontracted form."""

import argparse
from collections import Counter


TAG_DELIM = "_"
CONTR_S = "'s"
CONTR_LL = "'ll"
CONTR_D = "'d"
CONTR_VE = "'ve"
CONTR_RE = "'re"
CONTR_M = "'m"
CONTRACTIONS = \
    set([CONTR_S, CONTR_LL, CONTR_D, CONTR_D, CONTR_VE, CONTR_RE, CONTR_M])
UNAMBIG_FORMS = {
    "'ll": 'will',
    "'ve": 'have',
    "'m": 'am',
    "'re": 'are'
    }
AMBIG_FORMS = {
    "'s": {'HVS': 'has', 'BES': 'is', 'POS': 'GEN'},
    "'d": {'MD': 'would', 'VBD': 'had'}
}
    
def process(input_path, output_path):
    """Convert the input to uncontracted output."""
    input_file = open(input_path, 'Ur')
    output_file = open(output_path, 'w')

    total_lines = 0
    contraction_counts = Counter()
    for line in input_file:
        bad_line = False
        total_lines += 1
        tokens, tags = zip(*[_split_token(token)
                             for token in line.rstrip().split(" ")])
        tokens = list(tokens)  # tokens needs to be mutable
        for idx, token in enumerate(tokens):
            if token not in CONTRACTIONS:
                continue

            # Map it to its original form
            if token in UNAMBIG_FORMS:
                new_token = UNAMBIG_FORMS[token]
                tokens[idx] = new_token
                contraction_counts[(token, new_token)] += 1
            else:
                tag = tags[idx]
                # Resolve ambiguities
                try:
                    disambig = AMBIG_FORMS[token]
                except KeyError:
                    raise ValueError("Unhandled form: {!r}".format(token))
                # If there are any misses at this point, it is
                # intentional. The input must be mistagged to cause
                # this.
                try:
                    new_token = disambig[tag]
                except KeyError:
                    # Exclude this utterance
                    bad_line = True
                    break
                tokens[idx] = new_token
                contraction_counts[(token, new_token)] += 1

        if not bad_line:
            print >> output_file, " ".join(tokens)

    print "Total lines:", total_lines
    print "Contractions:", sum(contraction_counts.values())
    for (token, new_token), count in sorted(contraction_counts.items()):
        print "{} -> {}: {}".format(token, new_token, count)


def _split_token(token):
    """Split a token into (word, tag)."""
    try:
        word, tag = token.rsplit(TAG_DELIM, 1)
    except ValueError:
        raise ValueError("Bad token/tag pair: {!r}".format(token))
    return (word, tag)


def main():
    """Turn a contracted corpus into one with uncontracted forms."""
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.add_argument('input_file',
                        help='input file with one sentence per line')
    parser.add_argument('output_file',
                        help='destination file')

    args = parser.parse_args()
    process(args.input_file, args.output_file)


if __name__ == "__main__":
    main()
