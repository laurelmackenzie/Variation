#!/bin/sh -e
# Assumes Stanford Tagger is in the below directory
DATADIR=data
TAGGERDIR=$HOME/bin/stanford-postagger
# Choose your model, the built-in has not been trained to disambiguate
# contractions well.
# Caseless model distributed with tagger
# MODEL=$TAGGERDIR/models/english-caseless-left3words-distsim.tagger
# Custom trained model, using train_tagger.sh
MODEL=swb-caseless-left3words-nodistsim.tagger
echo "Tokenizing..."
./tokenizer.sed < $DATADIR/all_trans_clean.txt > $DATADIR/all_trans_tokenized.txt 
echo "Tagging..."
java -Xmx2g -cp $TAGGERDIR/stanford-postagger.jar edu.stanford.nlp.tagger.maxent.MaxentTagger -model $MODEL -tokenize false -textFile $DATADIR/all_trans_tokenized.txt > $DATADIR/all_trans_tagged.txt
echo "Done"
