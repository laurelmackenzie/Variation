#!/bin/sh -e
# Assumes Stanford Tagger is in the below directory
DATADIR=data
TAGGERDIR=$HOME/bin/stanford-postagger
echo "Tokenizing..."
./tokenizer.sed < $DATADIR/all_trans_clean.txt > $DATADIR/all_trans_tokenized.txt 
echo "Tagging..."
java -Xmx2g -cp $TAGGERDIR/stanford-postagger.jar edu.stanford.nlp.tagger.maxent.MaxentTagger -model swb-caseless-left3words-nodistsim.tagger -tokenize false -textFile $DATADIR/all_trans_tokenized.txt > $DATADIR/all_trans_tagged.txt
echo "Done"
