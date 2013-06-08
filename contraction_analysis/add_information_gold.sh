#!/bin/sh -e
DATA=data
# Alternatively, add from the gold standard
./add_information.py $DATA/contraction_3.8.12_parses.csv $DATA/contraction_3.8.12_probs.csv $DATA/swb_pos_reversed.txt &
./add_information.py $DATA/560_contractions_3.8.12_parses.csv $DATA/560_contractions_3.8.12_probs.csv $DATA/swb_pos_reversed.txt
