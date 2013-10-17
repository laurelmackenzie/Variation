#!/usr/bin/env python
"""Add predictability scores to contraction data."""

import sys
import csv

from lexinfo.subtlexreader import SubtlexDict
from ngram import NgramModel, NoSuchContextException

NA = "NA"
BEGIN = "BEGIN"
END = "END"


def train(train_file):
    """Return the required language models trained from a file."""
    unigram = NgramModel(1)
    bigram_left = NgramModel(2)
    bigram_right = NgramModel(2)
    for line in train_file:
        tokens = line.rstrip().split()
        unigram.update(tokens)
        bigram_left.update(tokens)
        bigram_right.update(reversed(tokens))

    return (unigram, bigram_left, bigram_right)


def main():
    """Fill in parse trees in the CSV."""
    try:
        # TODO: Make error handling robust
        in_file = open(sys.argv[1], "Ur")
        out_file = open(sys.argv[2], "w")
        train_file = open(sys.argv[3], "Ur")
        subtlex_path = sys.argv[4]
    except IndexError:
        print "Usage: add_information input_csv output_csv train subtlex"
        sys.exit(1)
    except IOError:
        print "Error: could not open one input, output, or training files."
        sys.exit(1)

    # Set up CSVs
    reader = csv.DictReader(in_file)
    out_fields = (reader.fieldnames +
                  ["PFORWARD", "PBACKWARD", "PFORWARD_COUNT",
                   "PBACKWARD_COUNT", "PHOST", "PAFTER", "FREQHOST"])
    writer = csv.DictWriter(out_file, out_fields)

    # Read frequencies
    print "Reading frequency information..."
    subtlex = SubtlexDict(subtlex_path)

    # Train
    print "Training language models..."
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
        try:
            fields["PFORWARD"] = bigram_left.prob(target, (host,))
        except NoSuchContextException:
            fields["PFORWARD"] = NA
        fields["PFORWARD_COUNT"] = bigram_left.context_count((host,))
        try:
            fields["PBACKWARD"] = bigram_right.prob(target, (after,))
        except NoSuchContextException:
            fields["PBACKWARD"] = NA
        fields["PBACKWARD_COUNT"] = bigram_right.context_count((after,))

        try:
            fields["FREQHOST"] = subtlex[host].freq_count_low
        except KeyError:
            fields["FREQHOST"] = NA

        writer.writerow(fields)


if __name__ == "__main__":
    main()
