import csv
import re
import string

from nltk import TreebankWordTokenizer
from treebankloader import treebank_sw

MATCH_THRESHOLD = .66
PUNC = set(string.punctuation)
tokenizer = TreebankWordTokenizer()
NA = "NA"

def count_overlap(words1, words2):
    """Return the ratio of overlapping words."""
    return len(set(words1) & set(words2)) / float(max(len(words1), len(words2)))


def find_subtree(text, node_label, best_parse):
    """Find the best subtree in best_parse that matches the text and node label."""
    text_words = [word.lower() for word in tokenizer.tokenize(text)]
    matching_trees = best_parse.subtrees() # lambda tree: node_label in tree.node
    best_overlap = MATCH_THRESHOLD
    best_tree = None
    for tree in matching_trees:
        # Throw out punctuation in the parse
        tree_words = [word.lower() for word in tree.leaves() if word not in PUNC]
        overlap = count_overlap(tree_words, text_words)
        if overlap > best_overlap:
            best_overlap = overlap
            best_tree = tree

    return best_tree if best_overlap else None
    
    
def main():
    """Fill in parse trees in the CSV."""
    in_file = open("contraction_filtered.csv", "Ur")
    out_file = open("contraction_parses.csv", "w")
    reader = csv.DictReader(in_file)
    out_fields = reader.fieldnames + ["SUBJ_PARSE", "SUBJ_DEPTH", "FULL_PARSE"]
    writer = csv.DictWriter(out_file, out_fields)
    writer.writeheader()

    for fields in reader:
        # Grab the text from the transcription and clean out brackets
        trans_line =  re.sub(r'\[LM:.+\]', '', fields["LINE"])
        trans_words = [word.lower() for word in tokenizer.tokenize(trans_line)]

        # Now search the right file for the best parse
        fileid = fields["CONVO"]
        sents = treebank_sw.parsed_sents(fileid)
        best_overlap = 0
        best_parse = None
        for sent in sents:
            # Throw out punctuation in the parse and lowercase it
            try:
                parse_words = [word.lower() for word in sent.leaves() if word not in PUNC]
            except AttributeError:
                # Caused by tree failing to parse properly, just move on
                continue
            overlap = count_overlap(parse_words, trans_words)
            if overlap > best_overlap:
                best_overlap = overlap
                best_parse = sent

        # Try to find the NP in the parse
        subject_parse = find_subtree(fields["COMPLETE_SUBJ"], "NP", best_parse)

        fields["FULL_PARSE"] = repr(best_parse)
        if subject_parse:
            fields["SUBJ_PARSE"] = repr(subject_parse)
            fields["SUBJ_DEPTH"] = subject_parse.height()
        else:
            fields["SUBJ_PARSE"] = NA
            fields["SUBJ_DEPTH"] = NA
            
        writer.writerow(fields)
        

if __name__ == "__main__":
    main()
