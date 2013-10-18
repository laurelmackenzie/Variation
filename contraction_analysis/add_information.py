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


def get_subtlex_freq(word, subtlex):
    """Return the SUBTLEX frequencies (all, lowercase) of a word, or NA if there is none."""
    # Figure out whether the entry in SUBTLEX is capitalized or not
    # and capitalize if needed
    word_key = word
    if word_key not in subtlex:
        word_key_caps = word_key.capitalize()
        word_key = word_key_caps if word_key_caps in subtlex else None
    # Put in zeroes if we don't know the frequency. These will
    # become NAs in a second. We put in zero instead of NA so that
    # the later pass will catch words in SUBTLEX but with
    # freq_count_low == 0 and freq_count > 0
    freq = subtlex[word_key].freq_count if word_key else 0
    freq_low = subtlex[word_key].freq_count_low if word_key else 0

    # Write NAs if needed
    freq = freq if freq else NA
    freq_low = freq_low if freq_low else NA
    return (freq, freq_low)


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
                   "PBACKWARD_COUNT", "PHOST", "PAFTER", "FREQHOST", "FREQLOWHOST",
                   "FREQAFTER", "FREQLOWAFTER"])
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

        fields["FREQHOST"], fields["FREQLOWHOST"] = get_subtlex_freq(host, subtlex)
        fields["FREQAFTER"], fields["FREQLOWAFTER"] = get_subtlex_freq(after, subtlex)

        writer.writerow(fields)


if __name__ == "__main__":
    main()
