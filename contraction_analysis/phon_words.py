# Last updated 1/1/12
# The list of function words comes from Selkirk 1984:352--353, with my addition of 'uh' and 'um'. I can
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

import csv

if __name__ == "__main__":
    reader = csv.reader(open('/Users/laurel/Dropbox/Dissertation/Empirical/Contraction/combined_data/contraction.csv', 'rUb'))
    #reader = csv.reader(open('/Users/laurel/Dropbox/Speakers to code/560_contractions.csv', 'rUb'))
    header = reader.next()
    coded = []
    for row in reader:
        coded.append(row)
        #   fwords = set(['a','the','for','from','in','on','till','am','are','were','been','will','shall','can','her','them_1','their','one','when','an','some','all','than','that','for','or','nor','at','of','with','as','up','but','is','was','here','has','had','does','did','would','should','could','it','us','what','his','as','that','this','such','and','but','as','that','if','since',"aren't",'must',"can't",'its','by','to','through','do','be','may','I','i','you','she','he','we','they','me','my','why','who','so','no','too','down','out','round','like','might','your','our','their','whom','whose','these','those','each','both','[noise]','uh','um','because_1','have']) # this one has only monosyllabic function words
        fwords = set(['a','the','for','from','in','on','till','am','are','were','been','will','shall','can','her','them_1','their','one','when','an','some','all','than','that','for','or','nor','at','of','with','as','up','but','is','was','here','has','had','does','did','would','should','could','it','us','what','his','as','that','this','such','and','but','as','that','if','since',"aren't",'must',"can't",'its','by','to','through','do','be','may','I','i','you','she','he','we','they','me','my','why','who','so','no','too','down','out','round','like','might','your','our','their','whom','whose','these','those','each','both','[noise]','uh','um','because_1','have','over','under','after','during','before','behind','beneath','beyond','below','between','among','along','against','across','above','about','around','towards','until','except','having','being',"haven't","isn't","hadn't","aren't","won't", "wouldn't",'going',"couldn't",'any','every','either','neither','because'])
    for line in coded:
        subj = line[36].split(' ')
        counter = 0
        for word in fwords:
            counter = counter + subj.count(word)
        phon_words = len(subj) - counter
        line.append(phon_words)

    header.append('NO_PHON_WORDS')
    out = csv.writer(open('/Users/laurel/Dropbox/Dissertation/Empirical/Contraction/combined_data/contraction_phonwords.csv', 'wb'))
    #out = csv.writer(open('/Users/laurel/Dropbox/Speakers to code/560_contractions_phonwords.csv', 'wb'))
    out.writerow(header)
    out = csv.writer(open('/Users/laurel/Dropbox/Dissertation/Empirical/Contraction/combined_data/contraction_phonwords.csv', 'ab'))
    #out = csv.writer(open('/Users/laurel/Dropbox/Speakers to code/560_contractions_phonwords.csv', 'ab'))
    out.writerows(coded)

