#!/bin/sh -e
./valid_stems.py < ../data/all_us_CHILDES_wordlist.csv > stems.txt
./analyze_contexts.py ../data/all_us_CHILDES.txt ../data/cmudict.0.7a_ext_reduced stems.txt > results.txt
