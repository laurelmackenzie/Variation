#!/bin/sh -e
# Assumes Stanford Tagger is in the below directory
DATADIR=data
TAGGERDIR=$HOME/bin/stanford-postagger
echo "Tokenizing..."
./tokenizer.sed < $DATADIR/all_trans_clean.txt > $DATADIR/all_trans_tokenized.txt 
echo "Tagging..."
java -Xmx512m -cp $TAGGERDIR/stanford-postagger.jar edu.stanford.nlp.tagger.maxent.MaxentTagger -model $TAGGERDIR/models/english-caseless-left3words-distsim.tagger -tokenize false -textFile $DATADIR/all_trans_tokenized.txt > $DATADIR/all_trans_tagged.txt
echo "Done"
