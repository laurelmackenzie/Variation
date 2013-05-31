#!/bin/sh -e
DATA=data
# Concatenate everything
cat data/swb_ms98_transcriptions/*/*/*trans.text > $DATA/all_trans_raw.txt
# Remove everything other than what they said
cut -d" " -f1-3 --complement $DATA/all_trans_raw.txt > $DATA/all_trans_notime.txt
# Do the rest in python
python clean_transcripts.py < $DATA/all_trans_notime.txt > $DATA/all_trans_clean.txt
# Add information to the contraction data
./add_information.py data/contraction_3.8.12_parses.csv data/contraction_3.8.12_probs.csv data/all_trans_clean.txt &
./add_information.py data/560_contractions_3.8.12_parses.csv data/560_contractions_3.8.12_probs.csv data/all_trans_clean.txt
