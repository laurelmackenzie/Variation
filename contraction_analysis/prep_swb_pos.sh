#!/bin/sh -e
TBROOT=$HOME/data/ptb
DATA=data
cat $TBROOT/treebank_3/tagged/pos/swbd/*/*.pos > $DATA/swb_pos_raw.txt
./convert_swb_pos.py $DATA/swb_pos_raw.txt $DATA/swb_pos_converted.txt
