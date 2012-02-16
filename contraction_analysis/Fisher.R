d = read.csv("/Users/laurel/Dropbox/Dissertation/Empirical/Contraction/combined_data/contraction.csv")
dataFisher = subset(d, CORPUS == "Fisher")

#########################
#rename one-letter codes#
#########################

#unreducible contexts
levels(dataFisher$UNREDUC) = c("pseudocleft", "ellipsis", "hesitation", "infinitival", "null subject", "preceding pause", "question", "stressed", "thing-is", "not unreducible", "yes-no question")

#preceding stress
levels(dataFisher$PREC_STRESS) = c("monosyll", "stressed", "unstressed")

#preceding grammatical class
levels(dataFisher$PREC_GRAMM_CLASS) = c("adjective", "adverb", "auxiliary", "demonstrative", "disfluency", "emb pronoun", "how", "noun phrase", "pronoun", "preposition", "quantifier", "relative", "verb", "wh-word", "where", "who", "why", "expletive")

#subject complexity
levels(dataFisher$SUBJ_COMPLEXITY) = c("emb", "multi", "single")

###############################
#various other coding clean-up#
###############################

#group years of birth into decades of birth
dataFisher$DOB <- as.numeric(dataFisher$YOB)
#dataFisher[dataFisher$YOB < 1910,]$DOB <- 1900
#dataFisher[dataFisher$YOB < 1920 & dataFisher$YOB >= 1910,]$DOB <- 1910
dataFisher[dataFisher$YOB < 1930 & dataFisher$YOB >= 1920,]$DOB <- 1920
dataFisher[dataFisher$YOB < 1940 & dataFisher$YOB >= 1930,]$DOB <- 1930
dataFisher[dataFisher$YOB < 1950 & dataFisher$YOB >= 1940,]$DOB <- 1940
dataFisher[dataFisher$YOB < 1960 & dataFisher$YOB >= 1950,]$DOB <- 1950
dataFisher[dataFisher$YOB < 1970 & dataFisher$YOB >= 1960,]$DOB <- 1960
dataFisher[dataFisher$YOB < 1980 & dataFisher$YOB >= 1970,]$DOB <- 1970
dataFisher[dataFisher$YOB >= 1980,]$DOB <- 1980

dataFisher$OBSERVED = as.factor(dataFisher$OBSERVED)

#############################
#######    cont    ##########
#############################

#cont consists of all contractible data (i.e. not in unreducible contexts) but excludes wouldhave and auxhave. It will also exclude post-pronoun has/have got, but let's keep them in one subset just so we can graph them to show they're categorical. I'm also excluding AAVE speakers for now.
cont_hasgotFisher = subset(dataFisher, UNREDUC == "not unreducible"&WORD!= "wouldhave"&WORD!= "auxhave" & WORD!="did"& WORD!="does" & DIALECT!="PHILADELPHIA_AAVE")

#3-level coding
cont_hasgotFisher$THREE <- as.factor(cont_hasgotFisher$OBSERVED)
cont_hasgotFisher[cont_hasgotFisher$OBSERVED == 1, ]$THREE <- "1"
cont_hasgotFisher[cont_hasgotFisher$OBSERVED == 2, ]$THREE <- "1"
#cont_hasgotFisher[cont_hasgotFisher$OBSERVED == 3, ]$THREE <- "3"
cont_hasgotFisher[cont_hasgotFisher$OBSERVED == 4, ]$THREE <- "4"

#it also gets rid of all the tokens of "has/have got" after pronouns/"other"s (this is a categorical environment for contraction)
contFisher = cont_hasgotFisher[!(cont_hasgotFisher$FOLL_WORD == "got" & cont_hasgotFisher$NP != "NP" & !is.na(cont_hasgotFisher$FOLL_WORD)),]

#some level-dropping
contFisher$WORD = contFisher$WORD[, drop = TRUE]

#tense coding for past vs. non-past
contFisher$TENSE <- as.character(contFisher$WORD)
contFisher[contFisher$WORD == "has"|contFisher$WORD=="have"|contFisher$WORD=="is"|contFisher$WORD=="will", ]$TENSE <- "non-past"
#contFisher[contFisher$WORD == "had"|contFisher$WORD=="would", ]$TENSE <- "past"
contFisher$TENSE = as.factor(contFisher$TENSE)

#Recoding stress to two levels: stressed vs. unstressed. Default: monosyllables count as stressed. This covers nouns, adjectives ("else," fronted adjectives in wh-questions), adverbs ("now, here, much"), demonstratives (reading through the examples, embedded demonstratives always seem to get stress), prepositions (usually they precede a movement site and as such don't seem to be reduced), quantifiers ("one"), verbs. Embedded pronouns count as unstressed: this seems to work for e.g. "most of them" (and those seem to make up a lot of the examples), though not sure I feel totally comfortable doing this with e.g. "to me, for us," and especially not "ours/mine," but not sure what else to do.
#contFisher$PREC_STRESS2 <- as.factor(contFisher$PREC_STRESS)
contFisher$PREC_STRESS2 <- as.character(contFisher$PREC_STRESS)
contFisher[contFisher$PREC_STRESS == "monosyll", ]$PREC_STRESS2 <- "stressed"
contFisher[contFisher$PREC_STRESS == "monosyll" & contFisher$PREC_GRAMM_CLASS == "emb pronoun", ]$PREC_STRESS2 <- "unstressed"
contFisher$PREC_STRESS2 = as.factor(contFisher$PREC_STRESS2)
contFisher$PREC_STRESS2 = contFisher$PREC_STRESS2[, drop = TRUE]

#Some auxiliaries have context codings that need to be grouped. Make a CONTEXT2 that is by default the same as CONTEXT, unless otherwise specified.
contFisher$CONTEXT2 <- as.character(contFisher$CONTEXT)

#'is' auxiliary vs. copula
contFisher[contFisher$WORD == "is" & contFisher$CONTEXT == "a", ]$CONTEXT2 <- "copula"
contFisher[contFisher$WORD == "is" & contFisher$CONTEXT == "n", ]$CONTEXT2 <- "copula"
contFisher[contFisher$WORD == "is" & contFisher$CONTEXT == "i", ]$CONTEXT2 <- "copula"
contFisher[contFisher$WORD == "is" & contFisher$CONTEXT == "l", ]$CONTEXT2 <- "copula"
contFisher[contFisher$WORD == "is" & contFisher$CONTEXT == "g", ]$CONTEXT2 <- "auxiliary"
contFisher[contFisher$WORD == "is" & contFisher$CONTEXT == "v", ]$CONTEXT2 <- "auxiliary"

#'would' pragmatic contexts
#contFisher[contFisher$WORD == "would" & contFisher$CONTEXT == "s", ]$CONTEXT2 <- "h"
#contFisher[contFisher$WORD == "would" & contFisher$CONTEXT == "t", ]$CONTEXT2 <- "h"
#contFisher[contFisher$WORD == "would" & contFisher$CONTEXT == "r", ]$CONTEXT2 <- "p"
#contFisher[contFisher$WORD == "would" & contFisher$CONTEXT == "l", ]$CONTEXT2 <- "p"
#contFisher[contFisher$WORD == "would" & contFisher$CONTEXT == "b", ]$CONTEXT2 <- "c"

#need to do this after CONTEXT2 is all done
contFisher$CONTEXT2 = as.factor(contFisher$CONTEXT2)

#########################
#contFisher dependent variable#
#########################

#3-level coding
contFisher$THREE <- as.factor(contFisher$OBSERVED)
contFisher[contFisher$OBSERVED == 1, ]$THREE <- "1"
contFisher[contFisher$OBSERVED == 2, ]$THREE <- "1"
#contFisher[contFisher$OBSERVED == 3, ]$THREE <- "3"
contFisher[contFisher$OBSERVED == 4, ]$THREE <- "4"

#Additional 2-level coding where what happens to 3 is dependent on what the auxiliary is
#Assumptions made for this division (these hold for both NPs and pronouns):
#had: yellow forms are full + h-deletion -- have been lumped in with full forms (problematic after NPs, where they're actually hybrid, but works post-pronouns)
#has: yellow forms are full + h-deletion -- have been lumped in with full forms
#have: yellow forms are v + schwa-insertion -- are being called contFisherracted (problematic: they are actually hybrid after NPs; for pronouns I've actually changed this so yellow forms are full + h-deletion) 
#is: yellow forms don't exist so it doesn't matter
#will: yellow forms are l + schwa-insertion -- are being called contFisherracted
#would: yellow forms are d + schwa-insertion -- are being called contFisherracted
#10.11.10: added 'are' -- straightforward (1+2 = 1; 4 = 4). So far there are no level-2 'are' after pronouns, but if there ever are any, I'll need to add a level for that.
contFisher$NEWTWO <- as.numeric(contFisher$OBSERVED)
#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "had" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "had" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "had" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "had" & contFisher$NP == "NP", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "has" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "has" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "has" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "has" & contFisher$NP == "NP", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "have" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "have" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "have" & contFisher$NP == "NP", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "have" & contFisher$NP == "NP", ]$NEWTWO <- 1
contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "is" & contFisher$NP == "NP", ]$NEWTWO <- 0
contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "is" & contFisher$NP == "NP", ]$NEWTWO <- 0
contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "is" & contFisher$NP == "NP", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "will" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "will" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "will" & contFisher$NP == "NP", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "would" & contFisher$NP == "NP", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "would" & contFisher$NP == "NP", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "would" & contFisher$NP == "NP", ]$NEWTWO <- 1

#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "had" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "had" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "had" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "had" & contFisher$NP == "pro", ]$NEWTWO <- 1
##contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "has" & contFisher$NP == "pro", ]$NEWTWO <- 0
##contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "has" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "has" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "has" & contFisher$NP == "pro", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "have" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "have" & contFisher$NP == "pro", ]$NEWTWO <- 0
##there actually is one of these but it was said by Vanessa, who's being excluded for now.
##contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "have" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "have" & contFisher$NP == "pro", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "is" & contFisher$NP == "pro", ]$NEWTWO <- 0
##contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "is" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "is" & contFisher$NP == "pro", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "will" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 2 & contFisher$WORD == "will" & contFisher$NP == "pro", ]$NEWTWO <- 0
#contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "will" & contFisher$NP == "pro", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "will" & contFisher$NP == "pro", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 1 & contFisher$WORD == "would" & contFisher$NP == "pro", ]$NEWTWO <- 0
##contFisher[contFisher$OBSERVED == 3 & contFisher$WORD == "would" & contFisher$NP == "pro", ]$NEWTWO <- 1
#contFisher[contFisher$OBSERVED == 4 & contFisher$WORD == "would" & contFisher$NP == "pro", ]$NEWTWO <- 1

########################
#aux-specific variables#
########################

#hadFisher = subset(contFisher, WORD == "had")
#hasFisher = subset(contFisher, WORD == "has")
#haveFisher = subset(contFisher, WORD == "have")
isFisher = subset(contFisher, WORD == "is")
#willFisher = subset(contFisher, WORD == "will")
#wouldFisher = subset(contFisher, WORD == "would")

################
#drop and clean#
################

#hadFisher$PREC_GRAMM_CLASS = hadFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#hadFisher$CL_ENV = hadFisher$CL_ENV[, drop = TRUE]
#hadFisher$SUBJ_COMPLEXITY = hadFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#hadFisher$FOLLOWING_BROAD = hadFisher$FOLLOWING_BROAD[, drop = TRUE]
#
#hasFisher$PREC_GRAMM_CLASS = hasFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#hasFisher$CL_ENV = hasFisher$CL_ENV[, drop = TRUE]
#hasFisher$SUBJ_COMPLEXITY = hasFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#
#haveFisher$PREC_GRAMM_CLASS = haveFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#haveFisher$CL_ENV = haveFisher$CL_ENV[, drop = TRUE]
#haveFisher$SUBJ_COMPLEXITY = haveFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#haveFisher$PRONOUN = haveFisher$PRONOUN[, drop = TRUE]

isFisher$PREC_GRAMM_CLASS = isFisher$PREC_GRAMM_CLASS[, drop = TRUE]
isFisher$CL_ENV = isFisher$CL_ENV[, drop = TRUE]
isFisher$SUBJ_COMPLEXITY = isFisher$SUBJ_COMPLEXITY[, drop = TRUE]
isFisher$CONTEXT = isFisher$CONTEXT[, drop = TRUE]
isFisher$CONTEXT2 = isFisher$CONTEXT2[, drop = TRUE]

#willFisher$PREC_GRAMM_CLASS = willFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#willFisher$CL_ENV = willFisher$CL_ENV[, drop = TRUE]
#willFisher$SUBJ_COMPLEXITY = willFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#
#wouldFisher$PREC_GRAMM_CLASS = wouldFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#wouldFisher$CL_ENV = wouldFisher$CL_ENV[, drop = TRUE]
#wouldFisher$SUBJ_COMPLEXITY = wouldFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#wouldFisher$CONTEXT = wouldFisher$CONTEXT[, drop = TRUE]
#wouldFisher$CONTEXT2 = wouldFisher$CONTEXT2[, drop = TRUE]
#wouldFisher$FOLLOWING_BROAD = wouldFisher$FOLLOWING_BROAD[, drop = TRUE]
#wouldFisher$PRONOUN = wouldFisher$PRONOUN[, drop = TRUE]

#separate variables for the broad 'would' pragmatic contexts

#levels(wouldFisher$CONTEXT2) = c("conditional", "hedging", "imperfect", "none", "politeness", "quoted")
#
#would_hedgeFisher = subset(wouldFisher, CONTEXT2 ==  "hedging")
#would_hedgeFisher$FOLLOWING_BROAD = would_hedgeFisher$FOLLOWING_BROAD[, drop = TRUE]
#
#would_politeFisher = subset(wouldFisher, CONTEXT2 ==  "politeness")
#would_politeFisher$FOLLOWING_BROAD = would_politeFisher$FOLLOWING_BROAD[, drop = TRUE]
#
#would_impFisher = subset(wouldFisher, CONTEXT2 ==  "imperfect")
#would_impFisher$FOLLOWING_BROAD = would_impFisher$FOLLOWING_BROAD[, drop = TRUE]

#######################################
#aux-specific variables, post-pronouns#
#######################################

#had_pronFisher = subset(hadFisher, PREC_GRAMM_CLASS == "pronoun")
#had_pronFisher$PRONOUN = had_pronFisher$PRONOUN[, drop = TRUE]
#had_pronFisher_N = table(had_pronFisher$CL_ENV)
#levels(had_pronFisher$CL_ENV) = c("it", "others")
#
#has_pronFisher = subset(hasFisher, PREC_GRAMM_CLASS == "pronoun")
#has_pronFisher$CL_ENV = has_pronFisher$CL_ENV[, drop = TRUE]
#levels(has_pronFisher$CL_ENV) = "(all)"

#is_pronFisher = subset(isFisher, PREC_GRAMM_CLASS == "pronoun")
#is_pronFisher$CL_ENV = is_pronFisher$CL_ENV[, drop = TRUE]
#levels(is_pronFisher$CL_ENV) = "(all)"

#have_pronFisher = subset(haveFisher, PREC_GRAMM_CLASS == "pronoun")
#have_pronFisher$CL_ENV = have_pronFisher$CL_ENV[, drop = TRUE]
#levels(have_pronFisher$CL_ENV) = "(all)"
#
#will_pronFisher = subset(willFisher, PREC_GRAMM_CLASS == "pronoun")
#will_pronFisher$PRONOUN = will_pronFisher$PRONOUN[, drop = TRUE]
#will_pronFisher_N = table(will_pronFisher$CL_ENV)
#levels(will_pronFisher$CL_ENV) = c("it", "others")
#will_pronFisher$UNREDUC = will_pronFisher$UNREDUC[, drop = TRUE]
#
#would_pronFisher = subset(wouldFisher, PREC_GRAMM_CLASS == "pronoun")
#would_pronFisher$PRONOUN = would_pronFisher$PRONOUN[, drop = TRUE]
#
##for the 'would have' comparison
#would_pronFisher$UNREDUC = would_pronFisher$UNREDUC[, drop = TRUE]
#would_pronFisher_N = table(would_pronFisher$CL_ENV)
#levels(would_pronFisher$CL_ENV) = c("it", "others")

###################################
#aux-specific variables, post-'it'#
###################################

#had_itFisher = subset(hadFisher, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
#had_itFisher$PREC_GRAMM_CLASS = had_itFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#had_itFisher_N = table(had_itFisher$PREC_GRAMM_CLASS)
#
#has_itFisher = subset(hasFisher, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
#has_itFisher$PREC_GRAMM_CLASS = has_itFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#has_itFisher_N = table(has_itFisher$PREC_GRAMM_CLASS)

#is_itFisher = subset(isFisher, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
#is_itFisher$PREC_GRAMM_CLASS = is_itFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#is_itFisher_N = table(is_itFisher$PREC_GRAMM_CLASS)

#will_itFisher = subset(willFisher, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
#will_itFisher$PREC_GRAMM_CLASS = will_itFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#will_itFisher_N = table(will_itFisher$PREC_GRAMM_CLASS)
#
#would_itFisher = subset(wouldFisher, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
#would_itFisher$PREC_GRAMM_CLASS = would_itFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#would_itFisher_N = table(would_itFisher$PREC_GRAMM_C#LASS)

#####################################
#aux-specific variables, post-'that'#
#####################################

#had_thatFisher = subset(hadFisher, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
#had_thatFisher$PREC_GRAMM_CLASS = had_thatFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#had_thatFisher_N = table(had_thatFisher$PREC_GRAMM_CLASS)
#
#has_thatFisher = subset(hasFisher, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
#has_thatFisher$PREC_GRAMM_CLASS = has_thatFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#has_thatFisher_N = table(has_thatFisher$PREC_GRAMM_CLASS)

#is_thatFisher = subset(isFisher, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
#is_thatFisher$PREC_GRAMM_CLASS = is_thatFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#is_thatFisher_N = table(is_thatFisher$PREC_GRAMM_CLASS)
#
#will_thatFisher = subset(willFisher, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
#will_thatFisher$PREC_GRAMM_CLASS = will_thatFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#will_thatFisher_N = table(will_thatFisher$PREC_GRAMM_CLASS)
#
#would_thatFisher = subset(wouldFisher, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
#would_thatFisher$PREC_GRAMM_CLASS = would_thatFisher$PREC_GRAMM_CLASS[, drop = TRUE]
#would_thatFisher_N = table(would_thatFisher$PREC_GRAMM_CLASS)

#########################################
#aux-specific variables, post-expletives#
#########################################

#had_expFisher = subset(hadFisher, PREC_GRAMM_CLASS == "expletive")
#had_expFisher$PREC_SEG_M = had_expFisher$PREC_SEG_M[, drop = TRUE]
#had_exp_NFisher = table(had_expFisher$PREC_SEG_M)
#
#has_expFisher = subset(hasFisher, PREC_GRAMM_CLASS == "expletive")
#has_expFisher$PREC_SEG_M = has_expFisher$PREC_SEG_M[, drop = TRUE]
#has_exp_NFisher = table(has_expFisher$PREC_SEG_M)
#
#is_expFisher = subset(isFisher, PREC_GRAMM_CLASS == "expletive")
#is_expFisher$PREC_SEG_M = is_expFisher$PREC_SEG_M[, drop = TRUE]
#is_exp_NFisher = table(is_expFisher$PREC_SEG_M)
#
#will_expFisher = subset(willFisher, PREC_GRAMM_CLASS == "expletive")
#will_expFisher$PREC_SEG_M = will_expFisher$PREC_SEG_M[, drop = TRUE]
#will_exp_NFisher = table(will_expFisher$PREC_SEG_M)
#
#would_expFisher = subset(wouldFisher, PREC_GRAMM_CLASS == "expletive")
#would_expFisher$PREC_SEG_M = would_expFisher$PREC_SEG_M[, drop = TRUE]
#would_exp_NFisher = table(would_expFisher$PREC_SEG_M)

##########################
#all auxes after pronouns#
##########################

#after all pronouns, regardless of cliticization capability
#pronFisher = subset(contFisher, PREC_GRAMM_CLASS == "pronoun")
#
##Because close phonology isn't possible after all pronouns (e.g. it + had, it + would, it + will) and sometimes I just want to focus on the ones where it is
#pron_clFisher = subset(pronFisher, CL_ENV=="y")
#pron_clFisher$NEWTWO = pron_clFisher$NEWTWO[, drop = TRUE]
#pron_clFisher$URTWO = pron_clFisher$URTWO[, drop = TRUE]
#pron_clFisher$WORD = pron_clFisher$WORD[, drop = TRUE]
#
##pron_cl_noitFisher just includes the vowel-final pronouns, to be consistent.
#
#pron_cl_noitFisher = subset(pron_clFisher, PRONOUN!="it")
#pron_cl_noitFisher$PRONOUN = pron_cl_noitFisher$PRONOUN[, drop = TRUE]

#####################
#all auxes after NPs#
#####################

#I used to have NPadv folded in with NP. I've since decided that's not the best idea. There are still a few NPs that end in an adverb that get included in NP, but they're ones in which the adverb appears to be DP-attached (as far as I can tell), e.g. 'too, also, here, there, tonight, today'...
NPFisher = subset(contFisher, NP == "NP")

NPFisher$WORD = NPFisher$WORD[, drop = TRUE]
NPFisher$NO_SYLLS = NPFisher$NO_SYLLS[, drop = TRUE]
NPFisher$NO_SYLLS = as.numeric(as.character(NPFisher$NO_SYLLS))

#create a CV variable based on preceding segment
NPFisher$CV <- as.character(NPFisher$PREC_SEG_M)
NPFisher[NPFisher$PREC_SEG_M ==  "a", ]$CV <- "vowel"
NPFisher[NPFisher$PREC_SEG_M !=  "a", ]$CV <- "cons"
NPFisher$CV = as.factor(NPFisher$CV)

#########################
#number-of-syllable bins#
#########################

NPFisher$NO_SYLLS_BIN <- as.character(NPFisher$NO_SYLLS)
NPFisher[NPFisher$NO_SYLLS < 3, ]$NO_SYLLS_BIN <- "1-2"
NPFisher[NPFisher$NO_SYLLS >= 3 & NPFisher$NO_SYLLS < 5, ]$NO_SYLLS_BIN <- "3-4"
NPFisher[NPFisher$NO_SYLLS >= 5 & NPFisher$NO_SYLLS < 7, ]$NO_SYLLS_BIN <- "5-6"
NPFisher[NPFisher$NO_SYLLS >= 7, ]$NO_SYLLS_BIN <- "7+"

#alternative: only bin anything above 10
#NP[NPFisher$NO_SYLLS > 10 & NPFisher$NO_SYLLS <=  15, ]$NO_SYLLS_BIN <- "10-15"
#NP[NPFisher$NO_SYLLS > 15, ]$NO_SYLLS_BIN <- "16+"
NPFisher$NO_SYLLS_BIN = as.factor(NPFisher$NO_SYLLS_BIN)
#NPFisher$NO_SYLLS_BIN = factor(NPFisher$NO_SYLLS_BIN, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10-15", "16+"))

#############################################################################
#all auxes after NPs EXCEPT sibiliant-final ones where the aux is 'is'/'has'#
#############################################################################

#When we do the NP analysis, we don't want to include is/has after sibilant-final NPs because any schwa-initial forms we see there could be due to schwa-insertion for geminate avoidance. So we want to omit such forms, but include all others. The NP_EX(amine) field allows us to do this. (This is the same thing we did with would/had after 'it': cut out the ones where a schwa could be for phonotactic reasons.) All analyses of NPs are done on this body of data. This only ends up affecting is/has and really doesn't cut out that much data.
NPFisher$NP_EXFisher <- as.factor(NPFisher$CL_ENV)
#NPFisher[NPFisher$WORD == "had" & NPFisher$CL_ENV == "y", ]$NP_EXFisher <- "y"
#NPFisher[NPFisher$WORD == "had" & NPFisher$CL_ENV == "n", ]$NP_EXFisher <- "y"
##NPFisher[NPFisher$WORD == "have" & NPFisher$CL_ENV == "y", ]$NP_EXFisher <- "y"
#NPFisher[NPFisher$WORD == "have" & NPFisher$CL_ENV == "n", ]$NP_EXFisher <- "y"
#NPFisher[NPFisher$WORD == "would" & NPFisher$CL_ENV == "y", ]$NP_EXFisher <- "y"
#NPFisher[NPFisher$WORD == "would" & NPFisher$CL_ENV == "n", ]$NP_EXFisher <- "y"
#NPFisher[NPFisher$WORD == "will" & NPFisher$CL_ENV == "y", ]$NP_EXFisher <- "y"
#NPFisher[NPFisher$WORD == "will" & NPFisher$CL_ENV == "n", ]$NP_EXFisher <- "y"
#NPFisher[NPFisher$WORD == "would" & NPFisher$CL_ENV == "n", ]$NP_EXFisher <- "y"
NP_EXFisher = subset(NPFisher, NP_EXFisher == "y")

#do some dropping
NP_EXFisher$NEWTWO = NP_EXFisher$NEWTWO[, drop = TRUE]
NP_EXFisher$WORD = NP_EXFisher$WORD[, drop = TRUE]

#opposing single-word subjects to multi-word subjects without caring about embedding
NP_EXFisher$COMP2 <- as.factor(NP_EXFisher$SUBJ_COMPLEXITY)
NP_EXFisher[NP_EXFisher$SUBJ_COMPLEXITY ==  "emb", ]$COMP2 <- "multi"
NP_EXFisher$COMP2 = NP_EXFisher$COMP2[, drop = TRUE]

######################################
#aux-specific variables, post-NP only#
######################################

#These variables are formed based on NP_EXFisher, which doesn't have any effect on have/had/will/would but omits tokens after sibilants for has/is.
#had_NPFisher = subset(NP_EXFisher, WORD == "had")
#had_NPFisher$SUBJ_COMPLEXITY = had_NPFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#had_NP_NFisher = table(had_NPFisher$SUBJ_COMPLEXITY)
#had_NPFisher$PREC_STRESS = had_NPFisher$PREC_STRESS[, drop = TRUE]
#had_NP_stress_NFisher = table(had_NPFisher$PREC_STRESS)
#
#has_NPFisher = subset(NP_EXFisher, WORD == "has")
#has_NPFisher$SUBJ_COMPLEXITY = has_NPFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#has_NP_NFisher = table(has_NPFisher$SUBJ_COMPLEXITY)
#has_NPFisher$PREC_STRESS = has_NPFisher$PREC_STRESS[, drop = TRUE]
#has_NP_stress_NFisher = table(has_NPFisher$PREC_STRESS)
#
#have_NPFisher = subset(NP_EXFisher, WORD == "have")
#have_NPFisher$SUBJ_COMPLEXITY = have_NPFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#have_NP_NFisher = table(have_NPFisher$SUBJ_COMPLEXITY)
#have_NPFisher$PREC_STRESS = have_NPFisher$PREC_STRESS[, drop = TRUE]
#have_NP_stress_NFisher = table(have_NPFisher$PREC_STRESS)

#to see if h-deletion of 'have' differs after s-final vs. non-s-final nouns
#have_NPFisher$S <- as.character(have_NPFisher$PREC_SEG_M)
#have_NPFisher[have_NPFisher$PREC_SEG_M == "s",]$S <- "s"
##have_NPFisher[have_NPFisher$PREC_SEG_M != "s",]$S <- "other"
#have_NPFisher$S = as.factor(have_NPFisher$S)

is_NPFisher = subset(NP_EXFisher, WORD == "is")
is_NPFisher$SUBJ_COMPLEXITY = is_NPFisher$SUBJ_COMPLEXITY[, drop = TRUE]
is_NP_NFisher = table(is_NPFisher$SUBJ_COMPLEXITY)
is_NPFisher$PREC_STRESS = is_NPFisher$PREC_STRESS[, drop = TRUE]
is_NP_stress_NFisher = table(is_NPFisher$PREC_STRESS)

#will_NPFisher = subset(NP_EXFisher, WORD == "will")
#will_NPFisher$SUBJ_COMPLEXITY = will_NPFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#will_NP_NFisher = table(will_NPFisher$SUBJ_COMPLEXITY)
#will_NPFisher$PREC_STRESS = will_NPFisher$PREC_STRESS[, drop = TRUE]
#will_NP_stress_NFisher = table(will_NPFisher$PREC_STRESS)
#will_NPFisher$UNREDUC = will_NPFisher$UNREDUC[, drop = TRUE]
#
#would_NPFisher = subset(NP_EXFisher, WORD == "would")
#would_NPFisher$SUBJ_COMPLEXITY = would_NPFisher$SUBJ_COMPLEXITY[, drop = TRUE]
#would_NP_NFisher = table(would_NPFisher$SUBJ_COMPLEXITY)
#would_NPFisher$PREC_STRESS = would_NPFisher$PREC_STRESS[, drop = TRUE]
#would_NP_stress_NFisher = table(would_NPFisher$PREC_STRESS)
##for the 'would have' comparison
#would_NPFisher$UNREDUC = would_NPFisher$UNREDUC[, drop = TRUE]

#has, is, and will together -- the three whose surface forms can be unambiguously attributed to underlying forms.
#hiwFisher = subset(NP_EXFisher, WORD=="has"|WORD=="is"|WORD=="will")
#hiwFisher$PREC_GRAMM_CLASS = hiwFisher$PREC_GRAMM_CLASS[, drop = TRUE]

#hiwFisher$RECODE_THREE <- as.numeric(hiwFisher$THREE)
#hiwFisher[hiwFisher$WORD=="is"&hiwFisher$THREE==1,]$RECODE_THREE = 0
#hiwFisher[hiwFisher$WORD=="is"&hiwFisher$THREE==4,]$RECODE_THREE = 1
#hiwFisher[hiwFisher$WORD=="has"&hiwFisher$NEWTWO ==0,]$RECODE_THREE = 0
##hiwFisher[hiwFisher$WORD=="has"&hiwFisher$NEWTWO ==1,]$RECODE_THREE = 1
#hiwFisher[hiwFisher$WORD=="will"&hiwFisher$NEWTWO ==0,]$RECODE_THREE = 0
#hiwFisher[hiwFisher$WORD=="will"&hiwFisher$NEWTWO ==1,]$RECODE_THREE = 1

############
#would have#
############

#wouldhave goes in its own variable because it was measured on a different scale than the others
#wouldhaveFisher = subset(dataFisher, UNREDUC == "not unreducible"&WORD == "wouldhave")

#rename preceding grammatical class 
#wouldhaveFisher$PREC_GRAMM_CLASS = wouldhaveFisher$PREC_GRAMM_CLASS[, drop = TRUE]

#wouldhaveFisher$CL_ENV = wouldhaveFisher$CL_ENV[, drop = TRUE]

#wouldhaveFisher$SUBJ_COMPLEXITY = wouldhaveFisher$SUBJ_COMPLEXITY[, drop = TRUE]

#so I can cheat and have stacked bar graphs even though there's only one level on the x-axis
#wouldhaveFisher$UNREDUC = wouldhaveFisher$UNREDUC[, drop = TRUE]

#wouldhaveFisher$TWOLEVELS <- as.character(wouldhaveFisher$OBSERVED)
#wouldhaveFisher$TWOLEVELS <- "full"
#wouldhave[wouldhaveFisher$OBSERVED == 8|wouldhaveFisher$OBSERVED == 9, ]$TWOLEVELS <- "contracted"
#wouldhaveFisher$TWOLEVELS <- as.factor(wouldhaveFisher$TWOLEVELS)

#wouldhave_pronFisher = subset(wouldhaveFisher, PREC_GRAMM_CLASS == "pronoun")
#levels(wouldhave_pronFisher$CL_ENV) = c("it", "others")

#wouldhave_NPFisher = subset(wouldhaveFisher, NP == "NP")