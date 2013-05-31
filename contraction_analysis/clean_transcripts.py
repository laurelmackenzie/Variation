#!/usr/bin/env python
"""Clean up Switchboard transcripts."""

import sys
import re


def main():
    """Clean STDIN input of a transcript, writing to STDOUT."""
    cut_left = re.compile(r"^-\[([\w']+)\]")
    cut_right = re.compile(r"\[([\w']+)\]-$")
    dys = re.compile(r"^[\w']+-$")
    laughter = re.compile(r"^\[laughter-([\w']+)\]$")
    number = re.compile(r"(_\d+)$")
    bad_tokens = set(["[noise]", "[laughter]", "[vocalized-noise]"])

    for line in sys.stdin:
        line = line.strip()

        # Skip lines of only silence
        if line == "[silence]":
            continue
        
        # Tokenize
        tokens = line.split()

        # Remove bad tokens
        tokens = [token for token in tokens if token not in bad_tokens]

        # Clean up cut off words
        tokens = [cut_left.sub(r"\1", token) for token in tokens]
        tokens = [cut_right.sub(r"\1", token) for token in tokens]
        tokens = [dys.sub("", token) for token in tokens]

        # Clean up numbers after tokens
        tokens = [number.sub(r"", token) for token in tokens]

        # Clean up laughter
        tokens = [laughter.sub(r"\1", token) for token in tokens]

        # Skip if we've removed all tokens
        if not tokens:
            continue

        # Concatenate
        line = " ".join(token for token in tokens if token) 

        # Just skip the line in the case that there's still brackets in there
        if "[" in line or "]" in line:
            continue

        print line


if __name__ == "__main__":
    main()
