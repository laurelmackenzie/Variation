#!/usr/bin/env python
"""Add predictability scores to contraction data."""

import sys
import csv
from itertools import chain

from ngram import NgramModel


NA = "NA"
BEGIN = "BEGIN"
END = "END"


def train(train_file):
    """Return the required language models trained from a file."""
    print "Reading tokens..."
    fake_tokens = set([BEGIN, END])
    # First, organize by utterances
    utt_tokens = [[BEGIN] + line.split() + [END] for line in train_file]
    # Then, flatten it
    tokens = [token for token in chain.from_iterable(utt_tokens)]
    # Pull out just unigrams as well
    raw_tokens = [token for token in tokens if token not in fake_tokens]

    # Compute probs
    print "Computing unigram probabilities..."
    unigram = NgramModel(1, raw_tokens)
    print "Computing bigram left probabilities..."
    bigram_left = NgramModel(2, tokens)
    # Reverse to get right context
    print "Computing bigram right probabilities..."
    bigram_right = NgramModel(2, tokens[::-1])

    return (unigram, bigram_left, bigram_right)


def main():
    """Fill in parse trees in the CSV."""
    in_file = open(sys.argv[1], "Ur")
    out_file = open(sys.argv[2], "w")
    train_file = open(sys.argv[3], "Ur")

    # Set up CSVs, doing so early just in case there's a bad path
    reader = csv.DictReader(in_file)
    out_fields = (reader.fieldnames +
                  ["PFORWARD", "PBACKWARD", "PHOST", "PAFTER"])
    writer = csv.DictWriter(out_file, out_fields)

    # Train
    unigram, bigram_left, bigram_right = train(train_file)

    # Write output!
    print "Writing output..."
    writer.writeheader()
    for fields in reader:
        target = fields["WORD"].lower()
        host = fields["PREC_WORD"].lower()
        after = fields["FOLL_WORD"].lower()
        fields["PHOST"] = unigram.prob(host, None)
        fields["PAFTER"] = unigram.prob(after, None)
        fields["PFORWARD"] = bigram_left.prob(target, host)
        fields["PBACKWARD"] = bigram_right.prob(target, after)

        writer.writerow(fields)


if __name__ == "__main__":
    main()
