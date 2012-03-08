"""Update a CSV with an existing NO_PHON_WORDS column with new values."""

import sys
import csv
from string import punctuation

WORDS_FIELD = "NO_WORDS"
PHON_WORDS_FIELD = "NO_PHON_WORDS"
SUBJ_FIELD = "COMPLETE_SUBJ"
FUNC_WORDS_MONO_FIELD = "NO_FUNC_WORDS_MONO"
FUNC_WORDS_MULTI_FIELD = "NO_FUNC_WORDS_MULTI"

# The list of function words comes from Selkirk 1984:352--353, with my addition of 'uh', 'um', and [noise]. I can
# only assume that's principled. Words are arranged in the order she has them (i.e. by phonological shape).
# I added because_1 which is 'cause. Selkirk is missing 'have' from her list of auxes; added.
# Now something to realize: Selkirk 1995 says that when a function word precedes a lexical word, the lexical word
# forms a PrWd and the function word joins with it to form a PhPhrse. But when a function word FOLLOWS a lexical
# word -- specifically, is an object clitic, like 'him' -- the lexical word forms a PrWd, and then the function
# word forms another PrWd with it! So there's this asymmetry between function words that precede and those that
# follow. Now, how many function words really follow? There's 'him, her, them.' So I've removed 'him' & 'them'*
# the list below, despite the fact that they're monosyllabic function words, because they only function as object
# clitics. But what to do with 'her'? I'm leaving it in, because my hunch is that it's used more often as a
# possessive pronoun than in an object pronoun in subjects like I'm looking at. (By that same reasoning, it
# probably doesn't change much to take out 'him' and 'them', but whatever.)
# *Geez, but what about 'one of them'? That's where many of our 'them' tokens surface anyway. Is it really a PrWd?
# There's no lexical word for that function word to join up with! I don't think it could be. So 'both of them'
# should be 2, but 'one of them' should be 0. So what I want to do is something like "if it has 'them' in it, and
# the count is 0, it remains 0. But if it has 'them' in it and the count is greater than 0, add 1 to it. I don't
# have time to do this now, and I'm sure it won't change much. But fix this for the future.
# So for now I'm going to leave it the way I had it, which has 'them_1' in here but not 'them'. I'm actually ok
# with that; it seems pretty true to the spirit of Selkirk's proposal anyway.
# 'him' is officially out, but there were only 3 of them in SWB + Fisher anyway!
# CEL: We removed 'them', because we believe it's a prosodic word.

# Dysfluency tokens
DYSFLUENCIES = frozenset(['[noise]','uh','um'])

# Monosyllabic function words
FWORDS_MONO = frozenset(['a','the','for','from','in','on','till','am','are','were','been','will','shall','can','her','them_1','their','one','when','an','some','all','than','that','for','or','nor','at','of','with','as','up','but','is','was','here','has','had','does','did','would','should','could','it','us','what','his','as','that','this','such','and','but','as','that','if','since',"aren't",'must',"can't",'its','by','to','through','do','be','may','I','i','you','she','he','we','they','me','my','why','who','so','no','too','down','out','round','like','might','your','our','their','whom','whose','these','those','each','both','because_1','have']) 

# Multisyllabic function words
# CEL: We rejected Selkirk's 'having','being',"haven't","isn't","hadn't","aren't","won't", "wouldn't",
# 'going', and "couldn't"
FWORDS_MUTLI = frozenset(['over','under','after','during','before','behind','beneath','beyond','below','between','among','along','against','across','above','about','around','towards','until','except','any','every','either','neither','because'])

def count_phon_words(text):
    """Return the (phonological word, monosyllabic func., multisyllabic func.) counts for the given text."""
    phon, func_mono, func_multi = 0, 0, 0
    for word in text.split():
        if word in DYSFLUENCIES:
            pass
        elif word in FWORDS_MONO:
            func_mono += 1
        elif word in FWORDS_MUTLI:
            func_multi += 1
        else:
            phon += 1
    #Adjust for zero function words
    if not phon:
        # Steal from function words to add a phonological word
        if func_mono:
            func_mono -= 1
        elif func_multi:
            func_multi -= 1
        else:
            # There's no real content to this text, so return zero counts
            return (0, 0, 0)
        phon += 1
    return (phon, func_mono, func_multi)


def main(inpath, outpath):
    """Write a new CSV to outpath containing updated NO_PHON_WORDS."""
    reader = csv.DictReader(open(inpath, 'rUb'))
    fieldnames = reader.fieldnames

    # Add fieldnames if needed
    if WORDS_FIELD not in fieldnames:
        fieldnames.append(WORDS_FIELD)
    if PHON_WORDS_FIELD not in fieldnames:
        fieldnames.append(PHON_WORDS_FIELD)
    if FUNC_WORDS_MONO_FIELD not in fieldnames:
        fieldnames.append(FUNC_WORDS_MONO_FIELD)
    if FUNC_WORDS_MULTI_FIELD not in fieldnames:
        fieldnames.append(FUNC_WORDS_MULTI_FIELD)

    # Set up the output file
    writer = csv.DictWriter(open(outpath, 'wb'), fieldnames)
    writer.writeheader()

    # Modify each row
    for row in reader:
        # Get the subject and clean it up
        subj = row[SUBJ_FIELD].lower()
        subj = ''.join(char for char in subj if char not in punctuation)
        
        row[PHON_WORDS_FIELD], row[FUNC_WORDS_MONO_FIELD], row[FUNC_WORDS_MULTI_FIELD] =\
            count_phon_words(subj)
        row[WORDS_FIELD] = sum([row[PHON_WORDS_FIELD], row[FUNC_WORDS_MONO_FIELD], row[FUNC_WORDS_MULTI_FIELD]])
        writer.writerow(row)


if __name__ == "__main__":
    try:
        main(sys.argv[1], sys.argv[2])
    except IndexError:
        print >> sys.stderr, "Usage: update_phon_words infile outfile"""
        sys.exit(1)
