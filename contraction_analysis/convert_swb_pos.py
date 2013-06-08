#!/usr/bin/env python
"""Convert tagged Switchboard into a more usable tagged format."""

import argparse

TAG_DELIM = '/'
TAG_END = '.'
UNWANTED_TAGS = set([',', ':', 'UH', 'SYM', 'XX'])
COMMENT_MARKER = '*'
DIVIDER_MARKER = '======================================'
BRACKETS = set(['[', ']'])


def _split_token(token):
    """Split a token into (word, tag)."""
    try:
        word, tag = token.rsplit(TAG_DELIM, 1)
    except ValueError:
        raise ValueError("Bad token/tag pair: {!r}".format(token))
    return (word, tag)


def process(input_path, output_path):
    """Clean up the input file and write to output."""
    input_file = open(input_path, 'Ur')
    output_file = open(output_path, 'w')
    all_tags = set()

    line_tokens = []
    for line in input_file:
        line = line.rstrip()

        # Skip useless lines
        if (not line or line.startswith(COMMENT_MARKER)
            or line == DIVIDER_MARKER):
            continue

        # Accumulate tokens
        tokens = line.split(" ")
        for token in tokens:
            if not token or token in BRACKETS:
                continue
            else:
                try:
                    word, tag = _split_token(token)
                except ValueError:
                    print "Line has bad token:"
                    print line
                    raise

                # Skip tokens that have tags we don't want to output
                if tag in UNWANTED_TAGS:
                    continue

                # If tag was period, flush
                if tag == TAG_END:
                    # Don't output an empty line
                    if line_tokens:
                        # Lowercase and switch the delimiter
                        out_tokens = ["_".join([word.lower(), tag])
                                      for word, tag in line_tokens]
                        print >> output_file, " ".join(out_tokens)
                        line_tokens = []
                else:
                    # Otherwise, accumulate the token
                    line_tokens.append((word, tag))
                    # Track the tags in the output
                    all_tags.add(tag)


def main():
    """Convert Treebank 3 SWB annotation into something more normal."""
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.add_argument('input_file',
                        help='input file with one sentence per line')
    parser.add_argument('output_file',
                        help='destination file')
    args = parser.parse_args()
    process(args.input_file, args.output_file)


if __name__ == "__main__":
    main()
