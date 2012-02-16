data560 = read.csv("~/Dropbox/Speakers to code/560_contractions.csv", header=T)

#########################
#rename one-letter codes#
#########################

#unreducible contexts
#levels(data560$UNREDUC) = c("pseudocleft", "ellipsis", "hesitation", "infinitival", "null subject", "preceding pause", "question", "stressed", "thing-is", "not unreducible", "yes-no question")
levels(data560$UNREDUC) = c("pseudocleft", "ellipsis", "hesitation", "null subject", "preceding pause", "question", "stressed", "thing-is", "not unreducible", "yes-no question")

#preceding stress
levels(data560$PREC_STRESS) = c("monosyll", "stressed", "unstressed")

#preceding grammatical class
#levels(data560$PREC_GRAMM_CLASS) = c("adjective", "adverb", "auxiliary", "demonstrative", "disfluency", "emb pronoun", "how", "noun phrase", "pronoun", "preposition", "quantifier", "relative", "verb", "wh-word", "where", "who", "why", "expletive")
levels(data560$PREC_GRAMM_CLASS) = c("adjective", "adverb", "demonstrative", "disfluency", "emb pronoun", "noun phrase", "pronoun", "preposition", "quantifier", "relative", "verb", "wh-word", "expletive")

#subject complexity
levels(data560$SUBJ_COMPLEXITY) = c("emb", "multi", "single")

###############################
#various other coding clean-up#
###############################

#group years of birth into decades of birth
data560$DOB <- as.numeric(data560$YOB)
data560[data560$YOB < 1900,]$DOB <- 1890
data560[data560$YOB < 1910 & data560$YOB >= 1900,]$DOB <- 1900
data560[data560$YOB < 1920 & data560$YOB >= 1910,]$DOB <- 1910
data560[data560$YOB < 1930 & data560$YOB >= 1920,]$DOB <- 1920
data560[data560$YOB < 1940 & data560$YOB >= 1930,]$DOB <- 1930
data560[data560$YOB < 1950 & data560$YOB >= 1940,]$DOB <- 1940
data560[data560$YOB < 1960 & data560$YOB >= 1950,]$DOB <- 1950
data560[data560$YOB < 1970 & data560$YOB >= 1960,]$DOB <- 1960
data560[data560$YOB < 1980 & data560$YOB >= 1970,]$DOB <- 1970
data560[data560$YOB >= 1980,]$DOB <- 1980

data560$OBSERVED = as.factor(data560$OBSERVED)

#############################
#######    cont    ##########
#############################

#cont consists of all contractible data (i.e. not in unreducible contexts) but excludes wouldhave and auxhave. It will also exclude post-pronoun has/have got, but let's keep them in one subset just so we can graph them to show they're categorical. I'm also excluding AAVE speakers for now.
cont_hasgot560 = subset(data560, UNREDUC == "not unreducible"&WORD!= "wouldhave"&WORD!= "auxhave" & WORD!="did"& WORD!="does" & DIALECT!="PHILADELPHIA_AAVE")

#3-level coding
cont_hasgot560$THREE <- as.factor(cont_hasgot560$OBSERVED)
cont_hasgot560[cont_hasgot560$OBSERVED == 1, ]$THREE <- "1"
cont_hasgot560[cont_hasgot560$OBSERVED == 2, ]$THREE <- "1"
cont_hasgot560[cont_hasgot560$OBSERVED == 3, ]$THREE <- "3"
cont_hasgot560[cont_hasgot560$OBSERVED == 4, ]$THREE <- "4"

#it also gets rid of all the tokens of "has/have got" after pronouns/"other"s (this is a categorical environment for contraction)
cont560 = cont_hasgot560[!(cont_hasgot560$FOLL_WORD == "got" & cont_hasgot560$NP != "NP" & !is.na(cont_hasgot560$FOLL_WORD)),]

#some level-dropping
cont560$WORD = cont560$WORD[, drop = TRUE]

#tense coding for past vs. non-past
cont560$TENSE <- as.character(cont560$WORD)
cont560[cont560$WORD == "has"|cont560$WORD=="have"|cont560$WORD=="is"|cont560$WORD=="will", ]$TENSE <- "non-past"
cont560[cont560$WORD == "had"|cont560$WORD=="would", ]$TENSE <- "past"
cont560$TENSE = as.factor(cont560$TENSE)

#Recoding stress to two levels: stressed vs. unstressed. Default: monosyllables count as stressed. This covers nouns, adjectives ("else," fronted adjectives in wh-questions), adverbs ("now, here, much"), demonstratives (reading through the examples, embedded demonstratives always seem to get stress), prepositions (usually they precede a movement site and as such don't seem to be reduced), quantifiers ("one"), verbs. Embedded pronouns count as unstressed: this seems to work for e.g. "most of them" (and those seem to make up a lot of the examples), though not sure I feel totally comfortable doing this with e.g. "to me, for us," and especially not "ours/mine," but not sure what else to do.
#cont560$PREC_STRESS2 <- as.factor(cont560$PREC_STRESS)
cont560$PREC_STRESS2 <- as.character(cont560$PREC_STRESS)
cont560[cont560$PREC_STRESS == "monosyll", ]$PREC_STRESS2 <- "stressed"
cont560[cont560$PREC_STRESS == "monosyll" & cont560$PREC_GRAMM_CLASS == "emb pronoun", ]$PREC_STRESS2 <- "unstressed"
cont560$PREC_STRESS2 = as.factor(cont560$PREC_STRESS2)
cont560$PREC_STRESS2 = cont560$PREC_STRESS2[, drop = TRUE]

#Some auxiliaries have context codings that need to be grouped. Make a CONTEXT2 that is by default the same as CONTEXT, unless otherwise specified.
cont560$CONTEXT2 <- as.character(cont560$CONTEXT)

#'is' auxiliary vs. copula
cont560[cont560$WORD == "is" & cont560$CONTEXT == "a", ]$CONTEXT2 <- "copula"
cont560[cont560$WORD == "is" & cont560$CONTEXT == "n", ]$CONTEXT2 <- "copula"
cont560[cont560$WORD == "is" & cont560$CONTEXT == "i", ]$CONTEXT2 <- "copula"
cont560[cont560$WORD == "is" & cont560$CONTEXT == "l", ]$CONTEXT2 <- "copula"
cont560[cont560$WORD == "is" & cont560$CONTEXT == "g", ]$CONTEXT2 <- "auxiliary"
cont560[cont560$WORD == "is" & cont560$CONTEXT == "v", ]$CONTEXT2 <- "auxiliary"

#'would' pragmatic contexts
cont560[cont560$WORD == "would" & cont560$CONTEXT == "s", ]$CONTEXT2 <- "h"
cont560[cont560$WORD == "would" & cont560$CONTEXT == "t", ]$CONTEXT2 <- "h"
cont560[cont560$WORD == "would" & cont560$CONTEXT == "r", ]$CONTEXT2 <- "p"
cont560[cont560$WORD == "would" & cont560$CONTEXT == "l", ]$CONTEXT2 <- "p"
#cont560[cont560$WORD == "would" & cont560$CONTEXT == "b", ]$CONTEXT2 <- "c"

#need to do this after CONTEXT2 is all done
cont560$CONTEXT2 = as.factor(cont560$CONTEXT2)

##########################
#  group years recorded  #
##########################

cont560$YEAR_REC_BROAD <- as.numeric(cont560$YEAR_RECORDED)
cont560[cont560$YEAR_RECORDED < 2000, ]$YEAR_REC_BROAD <- "1980"
cont560[cont560$YEAR_RECORDED >= 2000, ]$YEAR_REC_BROAD <- "2000"
#cont560$YEAR_REC_BROAD <- as.factor(cont560$YEAR_REC_BROAD)

#######################
#    trend samples    #
#######################

cont560$TREND <- as.character(cont560$SPEAKER)
cont560[cont560$SPEAKER %in% c("Victor Sarsparilla", "Sam Y", "Dan Good", "Jerome Long", "Jane Union", "Fannie Brown", "Maggie Rose", "Burt Crane", "Antonette Lembolini", "Gloria Stein", "Jim Lewis", "Ellen Connors", "Joe Rossini", "Melissa Purcell", "Carla Cirillo", "John Stevens", "Gary Salvi", "Brian D'Addario", "Bag-A-Donuts", "Lara McIntosh", "Burt McIntosh", "Karen Swanson", "Jenny McPhee", "Edie Carlin", "Earl Carlin", "Jean", "Patrick", "Brooke", "Amy", "Samantha") & cont560$YEAR_REC_BROAD==1980, ]$TREND <- "t1980" 
cont560[cont560$SPEAKER %in% c("Victor Sarsparilla", "Sam Y", "Dan Good", "Jerome Long", "Jane Union", "Fannie Brown", "Maggie Rose", "Burt Crane", "Antonette Lembolini", "Gloria Stein", "Jim Lewis", "Ellen Connors", "Joe Rossini", "Melissa Purcell", "Carla Cirillo", "John Stevens", "Gary Salvi", "Brian D'Addario", "Bag-A-Donuts", "Lara McIntosh", "Burt McIntosh", "Karen Swanson", "Jenny McPhee", "Edie Carlin", "Earl Carlin", "Jean", "Patrick", "Brooke", "Amy", "Samantha") & cont560$YEAR_REC_BROAD==2000, ]$TREND <- "t2000" 
cont560$TREND = as.factor(cont560$TREND)

#2000
#OM: Bag-a-Donuts, Burt, Earl, Patrick
#OW: Samantha, Jean, Edie, Lara
#YM: Brian, John, Gary (are there even any other options?)
#YW: Brooke, Karen, Jenny, Amy

#1980
#OM: Victor, Sam, Jim, Joe
#OW: Antonette, Gloria, Fannie, Carla
#YM: Dan, Burt, Jerome (again, anyone else?)
#YW: Maggie, Ellen, Melissa, Jane

#########################
#cont560 dependent variable#
#########################

#3-level coding
cont560$THREE <- as.factor(cont560$OBSERVED)
cont560[cont560$OBSERVED == 1, ]$THREE <- "1"
cont560[cont560$OBSERVED == 2, ]$THREE <- "1"
cont560[cont560$OBSERVED == 3, ]$THREE <- "3"
cont560[cont560$OBSERVED == 4, ]$THREE <- "4"

#Additional 2-level coding where what happens to 3 is dependent on what the auxiliary is
#Assumptions made for this division (these hold for both NPs and pronouns):
#had: yellow forms are full + h-deletion -- have been lumped in with full forms (problematic after NPs, where they're actually hybrid, but works post-pronouns)
#has: yellow forms are full + h-deletion -- have been lumped in with full forms
#have: yellow forms are v + schwa-insertion -- are being called cont560racted (problematic: they are actually hybrid after NPs; for pronouns I've actually changed this so yellow forms are full + h-deletion) 
#is: yellow forms don't exist so it doesn't matter
#will: yellow forms are l + schwa-insertion -- are being called cont560racted
#would: yellow forms are d + schwa-insertion -- are being called cont560racted
#10.11.10: added 'are' -- straightforward (1+2 = 1; 4 = 4). So far there are no level-2 'are' after pronouns, but if there ever are any, I'll need to add a level for that.
cont560$NEWTWO <- as.numeric(cont560$OBSERVED)
cont560[cont560$OBSERVED == 1 & cont560$WORD == "had" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 2 & cont560$WORD == "had" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 3 & cont560$WORD == "had" & cont560$NP == "NP", ]$NEWTWO <- 0
#cont560[cont560$OBSERVED == 4 & cont560$WORD == "had" & cont560$NP == "NP", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "has" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 2 & cont560$WORD == "has" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 3 & cont560$WORD == "has" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 4 & cont560$WORD == "has" & cont560$NP == "NP", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "have" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 2 & cont560$WORD == "have" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 3 & cont560$WORD == "have" & cont560$NP == "NP", ]$NEWTWO <- 1
#cont560[cont560$OBSERVED == 4 & cont560$WORD == "have" & cont560$NP == "NP", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "is" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 2 & cont560$WORD == "is" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 4 & cont560$WORD == "is" & cont560$NP == "NP", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "will" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 2 & cont560$WORD == "will" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 3 & cont560$WORD == "will" & cont560$NP == "NP", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "would" & cont560$NP == "NP", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 3 & cont560$WORD == "would" & cont560$NP == "NP", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 4 & cont560$WORD == "would" & cont560$NP == "NP", ]$NEWTWO <- 1

cont560[cont560$OBSERVED == 1 & cont560$WORD == "had" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 2 & cont560$WORD == "had" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 3 & cont560$WORD == "had" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 4 & cont560$WORD == "had" & cont560$NP == "pro", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "has" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 2 & cont560$WORD == "has" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 3 & cont560$WORD == "has" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 4 & cont560$WORD == "has" & cont560$NP == "pro", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "have" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 2 & cont560$WORD == "have" & cont560$NP == "pro", ]$NEWTWO <- 0
#there actually is one of these but it was said by Vanessa, who's being excluded for now.
#cont560[cont560$OBSERVED == 3 & cont560$WORD == "have" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 4 & cont560$WORD == "have" & cont560$NP == "pro", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "is" & cont560$NP == "pro", ]$NEWTWO <- 0
#cont560[cont560$OBSERVED == 2 & cont560$WORD == "is" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 4 & cont560$WORD == "is" & cont560$NP == "pro", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "will" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 2 & cont560$WORD == "will" & cont560$NP == "pro", ]$NEWTWO <- 0
cont560[cont560$OBSERVED == 3 & cont560$WORD == "will" & cont560$NP == "pro", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 4 & cont560$WORD == "will" & cont560$NP == "pro", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 1 & cont560$WORD == "would" & cont560$NP == "pro", ]$NEWTWO <- 0
#cont560[cont560$OBSERVED == 3 & cont560$WORD == "would" & cont560$NP == "pro", ]$NEWTWO <- 1
cont560[cont560$OBSERVED == 4 & cont560$WORD == "would" & cont560$NP == "pro", ]$NEWTWO <- 1

########################
#aux-specific variables#
########################

had560 = subset(cont560, WORD == "had")
has560 = subset(cont560, WORD == "has")
have560 = subset(cont560, WORD == "have")
is560 = subset(cont560, WORD == "is")
will560 = subset(cont560, WORD == "will")
would560 = subset(cont560, WORD == "would")

################
#drop and clean#
################

had560$PREC_GRAMM_CLASS = had560$PREC_GRAMM_CLASS[, drop = TRUE]
had560$CL_ENV = had560$CL_ENV[, drop = TRUE]
had560$SUBJ_COMPLEXITY = had560$SUBJ_COMPLEXITY[, drop = TRUE]
had560$FOLLOWING_BROAD = had560$FOLLOWING_BROAD[, drop = TRUE]

has560$PREC_GRAMM_CLASS = has560$PREC_GRAMM_CLASS[, drop = TRUE]
has560$CL_ENV = has560$CL_ENV[, drop = TRUE]
has560$SUBJ_COMPLEXITY = has560$SUBJ_COMPLEXITY[, drop = TRUE]

have560$PREC_GRAMM_CLASS = have560$PREC_GRAMM_CLASS[, drop = TRUE]
have560$CL_ENV = have560$CL_ENV[, drop = TRUE]
have560$SUBJ_COMPLEXITY = have560$SUBJ_COMPLEXITY[, drop = TRUE]
have560$PRONOUN = have560$PRONOUN[, drop = TRUE]

is560$PREC_GRAMM_CLASS = is560$PREC_GRAMM_CLASS[, drop = TRUE]
is560$CL_ENV = is560$CL_ENV[, drop = TRUE]
is560$SUBJ_COMPLEXITY = is560$SUBJ_COMPLEXITY[, drop = TRUE]
is560$CONTEXT = is560$CONTEXT[, drop = TRUE]
is560$CONTEXT2 = is560$CONTEXT2[, drop = TRUE]

will560$PREC_GRAMM_CLASS = will560$PREC_GRAMM_CLASS[, drop = TRUE]
will560$CL_ENV = will560$CL_ENV[, drop = TRUE]
will560$SUBJ_COMPLEXITY = will560$SUBJ_COMPLEXITY[, drop = TRUE]

would560$PREC_GRAMM_CLASS = would560$PREC_GRAMM_CLASS[, drop = TRUE]
would560$CL_ENV = would560$CL_ENV[, drop = TRUE]
would560$SUBJ_COMPLEXITY = would560$SUBJ_COMPLEXITY[, drop = TRUE]
would560$CONTEXT = would560$CONTEXT[, drop = TRUE]
would560$CONTEXT2 = would560$CONTEXT2[, drop = TRUE]
would560$FOLLOWING_BROAD = would560$FOLLOWING_BROAD[, drop = TRUE]
would560$PRONOUN = would560$PRONOUN[, drop = TRUE]

#separate variables for the broad 'would' pragmatic contexts

levels(would560$CONTEXT2) = c("conditional", "hedging", "imperfect", "none", "politeness", "quoted")

would_hedge560 = subset(would560, CONTEXT2 ==  "hedging")
would_hedge560$FOLLOWING_BROAD = would_hedge560$FOLLOWING_BROAD[, drop = TRUE]

would_polite560 = subset(would560, CONTEXT2 ==  "politeness")
would_polite560$FOLLOWING_BROAD = would_polite560$FOLLOWING_BROAD[, drop = TRUE]

would_imp560 = subset(would560, CONTEXT2 ==  "imperfect")
would_imp560$FOLLOWING_BROAD = would_imp560$FOLLOWING_BROAD[, drop = TRUE]

#######################################
#aux-specific variables, post-pronouns#
#######################################

had_pron560 = subset(had560, PREC_GRAMM_CLASS == "pronoun")
had_pron560$PRONOUN = had_pron560$PRONOUN[, drop = TRUE]
had_pron560_N = table(had_pron560$CL_ENV)
levels(had_pron560$CL_ENV) = c("it", "others")

has_pron560 = subset(has560, PREC_GRAMM_CLASS == "pronoun")
has_pron560$CL_ENV = has_pron560$CL_ENV[, drop = TRUE]
levels(has_pron560$CL_ENV) = "(all)"

is_pron560 = subset(is560, PREC_GRAMM_CLASS == "pronoun")
is_pron560$CL_ENV = is_pron560$CL_ENV[, drop = TRUE]
levels(is_pron560$CL_ENV) = "(all)"

have_pron560 = subset(have560, PREC_GRAMM_CLASS == "pronoun")
have_pron560$CL_ENV = have_pron560$CL_ENV[, drop = TRUE]
levels(have_pron560$CL_ENV) = "(all)"

will_pron560 = subset(will560, PREC_GRAMM_CLASS == "pronoun")
will_pron560$PRONOUN = will_pron560$PRONOUN[, drop = TRUE]
will_pron560_N = table(will_pron560$CL_ENV)
levels(will_pron560$CL_ENV) = c("it", "others")
will_pron560$UNREDUC = will_pron560$UNREDUC[, drop = TRUE]

would_pron560 = subset(would560, PREC_GRAMM_CLASS == "pronoun")
would_pron560$PRONOUN = would_pron560$PRONOUN[, drop = TRUE]

#for the 'would have' comparison
would_pron560$UNREDUC = would_pron560$UNREDUC[, drop = TRUE]
would_pron560_N = table(would_pron560$CL_ENV)
levels(would_pron560$CL_ENV) = c("it", "others")

###################################
#aux-specific variables, post-'it'#
###################################

had_it560 = subset(had560, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
had_it560$PREC_GRAMM_CLASS = had_it560$PREC_GRAMM_CLASS[, drop = TRUE]
had_it560_N = table(had_it560$PREC_GRAMM_CLASS)

has_it560 = subset(has560, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
has_it560$PREC_GRAMM_CLASS = has_it560$PREC_GRAMM_CLASS[, drop = TRUE]
has_it560_N = table(has_it560$PREC_GRAMM_CLASS)

is_it560 = subset(is560, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
is_it560$PREC_GRAMM_CLASS = is_it560$PREC_GRAMM_CLASS[, drop = TRUE]
is_it560_N = table(is_it560$PREC_GRAMM_CLASS)

will_it560 = subset(will560, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
will_it560$PREC_GRAMM_CLASS = will_it560$PREC_GRAMM_CLASS[, drop = TRUE]
will_it560_N = table(will_it560$PREC_GRAMM_CLASS)

would_it560 = subset(would560, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
would_it560$PREC_GRAMM_CLASS = would_it560$PREC_GRAMM_CLASS[, drop = TRUE]
would_it560_N = table(would_it560$PREC_GRAMM_CLASS)

#####################################
#aux-specific variables, post-'that'#
#####################################

had_that560 = subset(had560, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
had_that560$PREC_GRAMM_CLASS = had_that560$PREC_GRAMM_CLASS[, drop = TRUE]
had_that560_N = table(had_that560$PREC_GRAMM_CLASS)

has_that560 = subset(has560, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
has_that560$PREC_GRAMM_CLASS = has_that560$PREC_GRAMM_CLASS[, drop = TRUE]
has_that560_N = table(has_that560$PREC_GRAMM_CLASS)

is_that560 = subset(is560, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
is_that560$PREC_GRAMM_CLASS = is_that560$PREC_GRAMM_CLASS[, drop = TRUE]
is_that560_N = table(is_that560$PREC_GRAMM_CLASS)

will_that560 = subset(will560, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
will_that560$PREC_GRAMM_CLASS = will_that560$PREC_GRAMM_CLASS[, drop = TRUE]
will_that560_N = table(will_that560$PREC_GRAMM_CLASS)

would_that560 = subset(would560, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
would_that560$PREC_GRAMM_CLASS = would_that560$PREC_GRAMM_CLASS[, drop = TRUE]
would_that560_N = table(would_that560$PREC_GRAMM_CLASS)

#########################################
#aux-specific variables, post-expletives#
#########################################

had_exp560 = subset(had560, PREC_GRAMM_CLASS == "expletive")
had_exp560$PREC_SEG_M = had_exp560$PREC_SEG_M[, drop = TRUE]
had_exp_N560 = table(had_exp560$PREC_SEG_M)

has_exp560 = subset(has560, PREC_GRAMM_CLASS == "expletive")
has_exp560$PREC_SEG_M = has_exp560$PREC_SEG_M[, drop = TRUE]
has_exp_N560 = table(has_exp560$PREC_SEG_M)

is_exp560 = subset(is560, PREC_GRAMM_CLASS == "expletive")
is_exp560$PREC_SEG_M = is_exp560$PREC_SEG_M[, drop = TRUE]
is_exp_N560 = table(is_exp560$PREC_SEG_M)

will_exp560 = subset(will560, PREC_GRAMM_CLASS == "expletive")
will_exp560$PREC_SEG_M = will_exp560$PREC_SEG_M[, drop = TRUE]
will_exp_N560 = table(will_exp560$PREC_SEG_M)

would_exp560 = subset(would560, PREC_GRAMM_CLASS == "expletive")
would_exp560$PREC_SEG_M = would_exp560$PREC_SEG_M[, drop = TRUE]
would_exp_N560 = table(would_exp560$PREC_SEG_M)

##########################
#all auxes after pronouns#
##########################

#after all pronouns, regardless of cliticization capability
pron560 = subset(cont560, PREC_GRAMM_CLASS == "pronoun")

#Because close phonology isn't possible after all pronouns (e.g. it + had, it + would, it + will) and sometimes I just want to focus on the ones where it is
pron_cl560 = subset(pron560, CL_ENV=="y")
pron_cl560$NEWTWO = pron_cl560$NEWTWO[, drop = TRUE]
pron_cl560$URTWO = pron_cl560$URTWO[, drop = TRUE]
pron_cl560$WORD = pron_cl560$WORD[, drop = TRUE]

#pron_cl_noit560 just includes the vowel-final pronouns, to be consistent.

pron_cl_noit560 = subset(pron_cl560, PRONOUN!="it")
pron_cl_noit560$PRONOUN = pron_cl_noit560$PRONOUN[, drop = TRUE]

#####################
#all auxes after NPs#
#####################

#I used to have NPadv folded in with NP. I've since decided that's not the best idea. There are still a few NPs that end in an adverb that get included in NP, but they're ones in which the adverb appears to be DP-attached (as far as I can tell), e.g. 'too, also, here, there, tonight, today'...
NP560 = subset(cont560, NP == "NP")

NP560$WORD = NP560$WORD[, drop = TRUE]
NP560$NO_SYLLS = NP560$NO_SYLLS[, drop = TRUE]
NP560$NO_SYLLS = as.numeric(as.character(NP560$NO_SYLLS))

#create a CV variable based on preceding segment
NP560$CV <- as.character(NP560$PREC_SEG_M)
NP560[NP560$PREC_SEG_M ==  "a", ]$CV <- "vowel"
NP560[NP560$PREC_SEG_M !=  "a", ]$CV <- "cons"
NP560$CV = as.factor(NP560$CV)

#########################
#number-of-syllable bins#
#########################

NP560$NO_SYLLS_BIN <- as.character(NP560$NO_SYLLS)
NP560[NP560$NO_SYLLS < 3, ]$NO_SYLLS_BIN <- "1-2"
NP560[NP560$NO_SYLLS >= 3 & NP560$NO_SYLLS < 5, ]$NO_SYLLS_BIN <- "3-4"
NP560[NP560$NO_SYLLS >= 5 & NP560$NO_SYLLS < 7, ]$NO_SYLLS_BIN <- "5-6"
NP560[NP560$NO_SYLLS >= 7, ]$NO_SYLLS_BIN <- "7+"

#alternative: only bin anything above 10
#NP[NP560$NO_SYLLS > 10 & NP560$NO_SYLLS <=  15, ]$NO_SYLLS_BIN <- "10-15"
#NP[NP560$NO_SYLLS > 15, ]$NO_SYLLS_BIN <- "16+"
NP560$NO_SYLLS_BIN = as.factor(NP560$NO_SYLLS_BIN)
#NP560$NO_SYLLS_BIN = factor(NP560$NO_SYLLS_BIN, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10-15", "16+"))

#############################################################################
#all auxes after NPs EXCEPT sibiliant-final ones where the aux is 'is'/'has'#
#############################################################################

#When we do the NP analysis, we don't want to include is/has after sibilant-final NPs because any schwa-initial forms we see there could be due to schwa-insertion for geminate avoidance. So we want to omit such forms, but include all others. The NP_EX(amine) field allows us to do this. (This is the same thing we did with would/had after 'it': cut out the ones where a schwa could be for phonotactic reasons.) All analyses of NPs are done on this body of data. This only ends up affecting is/has and really doesn't cut out that much data.
NP560$NP_EX560 <- as.factor(NP560$CL_ENV)
NP560[NP560$WORD == "had" & NP560$CL_ENV == "y", ]$NP_EX560 <- "y"
NP560[NP560$WORD == "had" & NP560$CL_ENV == "n", ]$NP_EX560 <- "y"
#NP560[NP560$WORD == "have" & NP560$CL_ENV == "y", ]$NP_EX560 <- "y"
NP560[NP560$WORD == "have" & NP560$CL_ENV == "n", ]$NP_EX560 <- "y"
NP560[NP560$WORD == "would" & NP560$CL_ENV == "y", ]$NP_EX560 <- "y"
NP560[NP560$WORD == "would" & NP560$CL_ENV == "n", ]$NP_EX560 <- "y"
NP560[NP560$WORD == "will" & NP560$CL_ENV == "y", ]$NP_EX560 <- "y"
NP560[NP560$WORD == "will" & NP560$CL_ENV == "n", ]$NP_EX560 <- "y"
NP560[NP560$WORD == "would" & NP560$CL_ENV == "n", ]$NP_EX560 <- "y"
NP_EX560 = subset(NP560, NP_EX560 == "y")

#do some dropping
NP_EX560$NEWTWO = NP_EX560$NEWTWO[, drop = TRUE]
NP_EX560$WORD = NP_EX560$WORD[, drop = TRUE]

#opposing single-word subjects to multi-word subjects without caring about embedding
NP_EX560$COMP2 <- as.factor(NP_EX560$SUBJ_COMPLEXITY)
NP_EX560[NP_EX560$SUBJ_COMPLEXITY ==  "emb", ]$COMP2 <- "multi"
NP_EX560$COMP2 = NP_EX560$COMP2[, drop = TRUE]

######################################
#aux-specific variables, post-NP only#
######################################

#These variables are formed based on NP_EX560, which doesn't have any effect on have/had/will/would but omits tokens after sibilants for has/is.
had_NP560 = subset(NP_EX560, WORD == "had")
had_NP560$SUBJ_COMPLEXITY = had_NP560$SUBJ_COMPLEXITY[, drop = TRUE]
had_NP_N560 = table(had_NP560$SUBJ_COMPLEXITY)
had_NP560$PREC_STRESS = had_NP560$PREC_STRESS[, drop = TRUE]
had_NP_stress_N560 = table(had_NP560$PREC_STRESS)

has_NP560 = subset(NP_EX560, WORD == "has")
has_NP560$SUBJ_COMPLEXITY = has_NP560$SUBJ_COMPLEXITY[, drop = TRUE]
has_NP_N560 = table(has_NP560$SUBJ_COMPLEXITY)
has_NP560$PREC_STRESS = has_NP560$PREC_STRESS[, drop = TRUE]
has_NP_stress_N560 = table(has_NP560$PREC_STRESS)

have_NP560 = subset(NP_EX560, WORD == "have")
have_NP560$SUBJ_COMPLEXITY = have_NP560$SUBJ_COMPLEXITY[, drop = TRUE]
have_NP_N560 = table(have_NP560$SUBJ_COMPLEXITY)
have_NP560$PREC_STRESS = have_NP560$PREC_STRESS[, drop = TRUE]
have_NP_stress_N560 = table(have_NP560$PREC_STRESS)

#to see if h-deletion of 'have' differs after s-final vs. non-s-final nouns
have_NP560$S <- as.character(have_NP560$PREC_SEG_M)
have_NP560[have_NP560$PREC_SEG_M == "s",]$S <- "s"
#have_NP560[have_NP560$PREC_SEG_M != "s",]$S <- "other"
have_NP560$S = as.factor(have_NP560$S)

is_NP560 = subset(NP_EX560, WORD == "is")
is_NP560$SUBJ_COMPLEXITY = is_NP560$SUBJ_COMPLEXITY[, drop = TRUE]
is_NP_N560 = table(is_NP560$SUBJ_COMPLEXITY)
is_NP560$PREC_STRESS = is_NP560$PREC_STRESS[, drop = TRUE]
is_NP_stress_N560 = table(is_NP560$PREC_STRESS)

will_NP560 = subset(NP_EX560, WORD == "will")
will_NP560$SUBJ_COMPLEXITY = will_NP560$SUBJ_COMPLEXITY[, drop = TRUE]
will_NP_N560 = table(will_NP560$SUBJ_COMPLEXITY)
will_NP560$PREC_STRESS = will_NP560$PREC_STRESS[, drop = TRUE]
will_NP_stress_N560 = table(will_NP560$PREC_STRESS)
will_NP560$UNREDUC = will_NP560$UNREDUC[, drop = TRUE]

would_NP560 = subset(NP_EX560, WORD == "would")
would_NP560$SUBJ_COMPLEXITY = would_NP560$SUBJ_COMPLEXITY[, drop = TRUE]
would_NP_N560 = table(would_NP560$SUBJ_COMPLEXITY)
would_NP560$PREC_STRESS = would_NP560$PREC_STRESS[, drop = TRUE]
would_NP_stress_N560 = table(would_NP560$PREC_STRESS)
#for the 'would have' comparison
would_NP560$UNREDUC = would_NP560$UNREDUC[, drop = TRUE]

#has, is, and will together -- the three whose surface forms can be unambiguously attributed to underlying forms.
hiw560 = subset(NP_EX560, WORD=="has"|WORD=="is"|WORD=="will")
hiw560$PREC_GRAMM_CLASS = hiw560$PREC_GRAMM_CLASS[, drop = TRUE]

hiw560$RECODE_THREE <- as.numeric(hiw560$THREE)
hiw560[hiw560$WORD=="is"&hiw560$THREE==1,]$RECODE_THREE = 0
hiw560[hiw560$WORD=="is"&hiw560$THREE==4,]$RECODE_THREE = 1
hiw560[hiw560$WORD=="has"&hiw560$NEWTWO ==0,]$RECODE_THREE = 0
#hiw560[hiw560$WORD=="has"&hiw560$NEWTWO ==1,]$RECODE_THREE = 1
hiw560[hiw560$WORD=="will"&hiw560$NEWTWO ==0,]$RECODE_THREE = 0
hiw560[hiw560$WORD=="will"&hiw560$NEWTWO ==1,]$RECODE_THREE = 1

############
#would have#
############

#wouldhave goes in its own variable because it was measured on a different scale than the others
wouldhave560 = subset(data560, UNREDUC == "not unreducible"&WORD == "wouldhave")

#rename preceding grammatical class 
wouldhave560$PREC_GRAMM_CLASS = wouldhave560$PREC_GRAMM_CLASS[, drop = TRUE]

wouldhave560$CL_ENV = wouldhave560$CL_ENV[, drop = TRUE]

wouldhave560$SUBJ_COMPLEXITY = wouldhave560$SUBJ_COMPLEXITY[, drop = TRUE]

#so I can cheat and have stacked bar graphs even though there's only one level on the x-axis
wouldhave560$UNREDUC = wouldhave560$UNREDUC[, drop = TRUE]

#wouldhave560$TWOLEVELS <- as.character(wouldhave560$OBSERVED)
#wouldhave560$TWOLEVELS <- "full"
#wouldhave[wouldhave560$OBSERVED == 8|wouldhave560$OBSERVED == 9, ]$TWOLEVELS <- "contracted"
#wouldhave560$TWOLEVELS <- as.factor(wouldhave560$TWOLEVELS)

wouldhave_pron560 = subset(wouldhave560, PREC_GRAMM_CLASS == "pronoun")
levels(wouldhave_pron560$CL_ENV) = c("it", "others")

wouldhave_NP560 = subset(wouldhave560, NP == "NP")

########################
# multi speakers 10/17 #
########################

#This gives us word, observed, prec_seg_m (which we'll use to code vowel vs. consonant), subj # words, NP/pro, speaker, sex, age, educ, and subject, for everyone's post-NP and post-pronoun. Let the model decide whether there's enough data -- I'm just putting everything in (well, everything from NP_EX and pron_cl)
joe2 = rbind(NP_EX560[,c(1, 2, 6, 11, 14, 20, 21, 22, 24, 37)], pron_cl560[,c(1, 2, 6, 11, 14, 20, 21, 22, 24, 37)])

#get rid of NP + had, its IFs are ambiguous so they won't help us
joe1 = joe2[!(joe2$NP=="NP"&joe2$WORD=="had"),]

#do the same with NP + have
joe = joe1[!(joe1$NP=="NP"&joe1$WORD=="have"),]

colnames(joe)[c(2,4)] = c("DEP_VAR", "SUBJ_NO_WORDS")
joe$MORPH = as.character(joe$DEP_VAR)
joe$PHON = as.character(joe$DEP_VAR)
#create a consonant/vowel column based on preceding segment
joe$PREC_SEG <- "vowel"
joe[joe$PREC_SEG_M !=  "a", ]$PREC_SEG <- "cons"
joe$PREC_SEG = as.factor(joe$PREC_SEG)
#Get rid of the commas in the subject, they mess things up in the CSV -- replace with apostrophes
joe$SUBJ = gsub(',', "'", as.character(joe$COMPLETE_SUBJ))
#Get rid of that subject column that had commas in it and also prec_seg_m
joe = joe[,c(1:2, 4:9, 11:14)]

#If it starts with an h and it's a full form, call its PHON 0. If it doesn't start with an h, we don't care about its phon.
joe[(joe$WORD == "had" | joe$WORD == "have" | joe$WORD == "has") & (joe$DEP_VAR=="1" | joe$DEP_VAR == "2"),]$PHON = 0
joe[!(joe$WORD == "had" | joe$WORD == "have" | joe$WORD == "has"),]$PHON = NA

#if it's a full form, call its MORPH 0.
joe[joe$DEP_VAR=="1" | joe$DEP_VAR == "2",]$MORPH = 0

#Intermediate forms of h-initial guys had no contraction but yes h-deletion.
joe[(joe$WORD == "had" | joe$WORD == "have" | joe$WORD == "has") & joe$DEP_VAR == "3",]$MORPH = 0
joe[(joe$WORD == "had" | joe$WORD == "have" | joe$WORD == "has") & joe$DEP_VAR == "3",]$PHON = 1

#Intermediate forms of w-initial guys had contraction, i.e. yes on MORPH.
joe[(joe$WORD == "will" | joe$WORD == "would") & joe$DEP_VAR == "3",]$MORPH = 1

#Anyone who surfaces as contracted was a yes on MORPH.
joe[joe$DEP_VAR=="4",]$MORPH = 1
joe[joe$DEP_VAR=="4",]$PHON = NA
joe$MORPH = as.factor(joe$MORPH)
joe$PHON = as.factor(joe$PHON)

#write.csv(joe, file = "/Users/laurel/Desktop/contraction_560_NWAV.csv", quote = F, row.names = F)