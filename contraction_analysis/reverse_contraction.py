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
    "'s": ['has', 'is'],  # We can filter out possessive and "us" using tags
    "'d": ['would', 'had']
}
TAG_POS = 'POS'
TAG_MODAL = 'MD'
TAGS_US = set(['NNS', 'PRP'])  # Tags given to "us" in "let's"
    
def process(input_path, output_path, contract_only=True):
    """Convert the input to uncontracted output."""
    input_file = open(input_path, 'Ur')
    output_file = open(output_path, 'w')

    total_lines = 0
    contractions = 0
    ambig_contractions = 0
    for line in input_file:
        total_lines += 1
        tokens, tags = zip(*[_split_token(token)
                             for token in line.rstrip().split(" ")])
        tokens = list(tokens)  # tokens needs to be mutable
        for idx, token in enumerate(tokens):
            if token not in CONTRACTIONS:
                continue

            # Count the contraction
            contractions += 1

            # Map it to its original form
            if token in UNAMBIG_FORMS:
                tokens[idx] = UNAMBIG_FORMS[token]
            else:
                tag = tags[idx]
                # Resolve ambiguities
                if token == CONTR_S:
                    # Possessive and us can be identified by tag
                    if tag == TAG_POS:
                        tokens[idx] = TAG_POS
                        continue
                    elif tag in TAGS_US:
                        tokens[idx] = "us"
                        continue

                    # Otherwise, disambiguate between is/has
                    ambig_contractions += 1
                elif token == CONTR_D:
                    ambig_contractions += 1
                else:
                    raise ValueError("Unhandled form: {!r}".format(token))

        print >> output_file, " ".join(tokens)

    print "Total lines:", total_lines
    print "Contractions:", contractions
    print "Amibguous contractions:", ambig_contractions


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
