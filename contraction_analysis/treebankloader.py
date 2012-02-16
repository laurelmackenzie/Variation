import nltk

# pylint: disable-msg=E0611
from nltk.tokenize import RegexpTokenizer
from nltk.tag import simplify_wsj_tag
from nltk.corpus.reader import BracketParseCorpusReader, ChunkedCorpusReader, \
                        tagged_treebank_para_block_reader
from nltk.corpus.util import LazyCorpusLoader


# pylint: disable-msg=C0103
treebank_wsj = LazyCorpusLoader(
    'treebank_full/combined', BracketParseCorpusReader, r'wsj_.*\.mrg',
    tag_mapping_function=simplify_wsj_tag)

treebank_brown = LazyCorpusLoader(
    'treebank_full/combined', BracketParseCorpusReader, r'c.*\.mrg',
    tag_mapping_function=simplify_wsj_tag)

treebank_sw = LazyCorpusLoader(
    'treebank_full/combined', BracketParseCorpusReader, r'sw.*\.mrg',
    tag_mapping_function=simplify_wsj_tag)

treebank_wsj_chunk = LazyCorpusLoader(
    'treebank_full/tagged', ChunkedCorpusReader, r'wsj_.*\.pos',
    sent_tokenizer=RegexpTokenizer(r'(?<=/\.)\s*(?![^\[]*\])', gaps=True),
    para_block_reader=tagged_treebank_para_block_reader)

treebank_brown_chunk = LazyCorpusLoader(
    'treebank_full/tagged', ChunkedCorpusReader, r'c.*\.pos',
    sent_tokenizer=RegexpTokenizer(r'(?<=/\.)\s*(?![^\[]*\])', gaps=True),
    para_block_reader=tagged_treebank_para_block_reader)
