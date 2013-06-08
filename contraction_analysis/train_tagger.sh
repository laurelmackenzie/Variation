#!/bin/sh -e
# Assumes Stanford Tagger is in the below directory
DATADIR=data
TAGGERDIR=$HOME/bin/stanford-postagger
java -Xmx4g -cp $TAGGERDIR/stanford-postagger.jar edu.stanford.nlp.tagger.maxent.MaxentTagger -props swb.tagger.props

