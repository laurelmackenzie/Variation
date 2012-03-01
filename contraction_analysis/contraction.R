# Load contraction and LING560 data into one big happy frame
contraction_data = read.csv("data/contraction_2.16.11.csv")
l560_data = read.csv("data/560_contractions_2.16.11.csv")
data = rbind(contraction_data, l560_data)

library(xtable)
library(lme4)
library(languageR)
library(ggplot2)

#########################
#rename one-letter codes#
#########################

#unreducible contexts
levels(data$UNREDUC) = c("pseudocleft", "ellipsis", "hesitation", "infinitival", "null subject", "preceding pause", "question", "stressed", "thing-is", "not unreducible", "yes-no question")

#preceding stress
levels(data$PREC_STRESS) = c("monosyll", "stressed", "unstressed")

#preceding grammatical class
levels(data$PREC_GRAMM_CLASS) = c("adjective", "adverb", "auxiliary", "demonstrative", "disfluency", "emb pronoun", "how", "noun phrase", "pronoun", "preposition", "quantifier", "relative", "verb", "wh-word", "where", "who", "why", "expletive")

#subject complexity
levels(data$SUBJ_COMPLEXITY) = c("emb", "multi", "single")

###############################
#various other coding clean-up#
###############################

#group years of birth into decades of birth
data$DOB <- as.numeric(data$YOB)
data[data$YOB < 1930,]$DOB <- 1920
data[data$YOB < 1940 & data$YOB >= 1930,]$DOB <- 1930
data[data$YOB < 1950 & data$YOB >= 1940,]$DOB <- 1940
data[data$YOB < 1960 & data$YOB >= 1950,]$DOB <- 1950
data[data$YOB < 1970 & data$YOB >= 1960,]$DOB <- 1960
data[data$YOB >= 1970,]$DOB <- 1970

#group dialect into south vs. other (what's the difference between south midland and south? I have no idea. The data has twice as much south midland as any other group, including south, so I'm assuming Dallas is in south midland.)

data$BROAD_DIALECT <- "OTHER"
data[data$DIALECT == "SOUTHERN" | data$DIALECT == "SOUTH MIDLAND",]$BROAD_DIALECT = "SOUTH"
data$BROAD_DIALECT = as.factor(data$BROAD_DIALECT)

data$OBSERVED = as.factor(data$OBSERVED)

#############################
#######    cont    ##########
#############################

#cont consists of all contractible data (i.e. not in unreducible contexts) but excludes wouldhave and auxhave. It will also exclude post-pronoun has/have got, but let's keep them in one subset just so we can graph them to show they're categorical.
cont_hasgot = subset(data, UNREDUC == "not unreducible" & WORD !=  "wouldhave" & WORD != "auxhave" & WORD! = "did" & WORD! = "does" & DIALECT != "PHILADELPHIA_AAVE")

#3-level coding
cont_hasgot$THREE <- as.factor(cont_hasgot$OBSERVED)
cont_hasgot[cont_hasgot$OBSERVED == 1, ]$THREE <- "1"
cont_hasgot[cont_hasgot$OBSERVED == 2, ]$THREE <- "1"
cont_hasgot[cont_hasgot$OBSERVED == 3, ]$THREE <- "3"
cont_hasgot[cont_hasgot$OBSERVED == 4, ]$THREE <- "4"

#it also gets rid of all the tokens of "has/have got" after pronouns/"other"s (this is a categorical environment for contraction)
cont = cont_hasgot[!(cont_hasgot$FOLL_WORD == "got" & cont_hasgot$NP != "NP" & !is.na(cont_hasgot$FOLL_WORD)),]

#some level-dropping
cont$WORD = cont$WORD[, drop = TRUE]
cont$PREC_GRAMM_CLASS = cont$PREC_GRAMM_CLASS[, drop = TRUE]

#tense coding for past vs. non-past
cont$TENSE <- as.character(cont$WORD)
cont[cont$WORD == "has"|cont$WORD=="have"|cont$WORD=="is"|cont$WORD=="will", ]$TENSE <- "non-past"
cont[cont$WORD == "had"|cont$WORD=="would", ]$TENSE <- "past"
cont$TENSE = as.factor(cont$TENSE)

#Recoding stress to two levels: stressed vs. unstressed. Default: monosyllables count as stressed. This covers nouns, adjectives ("else," fronted adjectives in wh-questions), adverbs ("now, here, much"), demonstratives (reading through the examples, embedded demonstratives always seem to get stress), prepositions (usually they precede a movement site and as such don't seem to be reduced), quantifiers ("one"), verbs. Embedded pronouns count as unstressed: this seems to work for e.g. "most of them" (and those seem to make up a lot of the examples), though not sure I feel totally comfortable doing this with e.g. "to me, for us," and especially not "ours/mine," but not sure what else to do.
cont$PREC_STRESS2 <- as.factor(cont$PREC_STRESS)
cont[cont$PREC_STRESS == "monosyll", ]$PREC_STRESS2 <- "stressed"
cont[cont$PREC_STRESS == "monosyll" & cont$PREC_GRAMM_CLASS == "emb pronoun", ]$PREC_STRESS2 <- "unstressed"
cont$PREC_STRESS2 = cont$PREC_STRESS2[, drop = TRUE]

#Some auxiliaries have context codings that need to be grouped. Make a CONTEXT2 that is by default the same as CONTEXT, unless otherwise specified.
cont$CONTEXT2 <- as.character(cont$CONTEXT)

#'is' auxiliary vs. copula
cont[cont$WORD == "is" & cont$CONTEXT == "a", ]$CONTEXT2 <- "copula"
cont[cont$WORD == "is" & cont$CONTEXT == "n", ]$CONTEXT2 <- "copula"
cont[cont$WORD == "is" & cont$CONTEXT == "i", ]$CONTEXT2 <- "copula"
cont[cont$WORD == "is" & cont$CONTEXT == "l", ]$CONTEXT2 <- "copula"
cont[cont$WORD == "is" & cont$CONTEXT == "g", ]$CONTEXT2 <- "auxiliary"
cont[cont$WORD == "is" & cont$CONTEXT == "v", ]$CONTEXT2 <- "auxiliary"

#'would' pragmatic contexts
cont[cont$WORD == "would" & cont$CONTEXT == "s", ]$CONTEXT2 <- "h"
cont[cont$WORD == "would" & cont$CONTEXT == "t", ]$CONTEXT2 <- "h"
cont[cont$WORD == "would" & cont$CONTEXT == "r", ]$CONTEXT2 <- "p"
cont[cont$WORD == "would" & cont$CONTEXT == "l", ]$CONTEXT2 <- "p"
cont[cont$WORD == "would" & cont$CONTEXT == "b", ]$CONTEXT2 <- "c"

#need to do this after CONTEXT2 is all done
cont$CONTEXT2 = as.factor(cont$CONTEXT2)

#########################
#cont dependent variable#
#########################

#3-level coding
cont$THREE <- as.factor(cont$OBSERVED)
cont[cont$OBSERVED == 1, ]$THREE <- "1"
cont[cont$OBSERVED == 2, ]$THREE <- "1"
cont[cont$OBSERVED == 3, ]$THREE <- "3"
cont[cont$OBSERVED == 4, ]$THREE <- "4"

#2-level coding, level 3 is folded into level 4
cont$TWO <- as.factor(cont$OBSERVED)
cont[cont$OBSERVED == 1, ]$TWO <- "1"
cont[cont$OBSERVED == 2, ]$TWO <- "1"
cont[cont$OBSERVED == 3, ]$TWO <- "4"
cont[cont$OBSERVED == 4, ]$TWO <- "4"

#Alternative 2-level coding where level 3 is folded into level 1
cont$TWOALT <- as.factor(cont$OBSERVED)
cont[cont$OBSERVED == 1, ]$TWOALT <- "1"
cont[cont$OBSERVED == 2, ]$TWOALT <- "1"
cont[cont$OBSERVED == 3, ]$TWOALT <- "1"
cont[cont$OBSERVED == 4, ]$TWOALT <- "4"

#Weird 2-level coding where 1 and 4 are opposed to 3 (just to see how 3 behaves on its own)
cont$ONEFOUR <- as.factor(cont$OBSERVED)
cont[cont$OBSERVED == 1, ]$ONEFOUR <- "1"
cont[cont$OBSERVED == 2, ]$ONEFOUR <- "1"
cont[cont$OBSERVED == 3, ]$ONEFOUR <- "3"
cont[cont$OBSERVED == 4, ]$ONEFOUR <- "1"

#Additional 2-level coding where what happens to 3 is dependent on what the auxiliary is
#Assumptions made for this division (these hold for both NPs and pronouns):
#had: yellow forms are full + h-deletion -- have been lumped in with full forms (problematic after NPs, where they're actually hybrid, but works post-pronouns)
#has: yellow forms are full + h-deletion -- have been lumped in with full forms
#have: yellow forms are v + schwa-insertion -- are being called contracted (problematic: they are actually hybrid after NPs; for pronouns I've actually changed this so yellow forms are full + h-deletion) 
#is: yellow forms don't exist so it doesn't matter
#will: yellow forms are l + schwa-insertion -- are being called contracted
#would: yellow forms are d + schwa-insertion -- are being called contracted
#10.11.10: added 'are' -- straightforward (1+2 = 1; 4 = 4). So far there are no level-2 'are' after pronouns, but if there ever are any, I'll need to add a level for that.
cont$NEWTWO <- as.numeric(cont$OBSERVED)
cont[cont$OBSERVED == 1 & cont$WORD == "had" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "had" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "had" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 4 & cont$WORD == "had" & cont$NP == "NP", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "has" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "has" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "has" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 4 & cont$WORD == "has" & cont$NP == "NP", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "have" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "have" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "have" & cont$NP == "NP", ]$NEWTWO <- 1
cont[cont$OBSERVED == 4 & cont$WORD == "have" & cont$NP == "NP", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "is" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "is" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 4 & cont$WORD == "is" & cont$NP == "NP", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "will" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "will" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "will" & cont$NP == "NP", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "would" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "would" & cont$NP == "NP", ]$NEWTWO <- 1
cont[cont$OBSERVED == 4 & cont$WORD == "would" & cont$NP == "NP", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "are" & cont$NP == "NP", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "are" & cont$NP == "NP", ]$NEWTWO <- 0

cont[cont$OBSERVED == 1 & cont$WORD == "had" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "had" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "had" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 4 & cont$WORD == "had" & cont$NP == "pro", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "has" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "has" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "has" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 4 & cont$WORD == "has" & cont$NP == "pro", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "have" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "have" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "have" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 4 & cont$WORD == "have" & cont$NP == "pro", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "is" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "is" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 4 & cont$WORD == "is" & cont$NP == "pro", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "will" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 2 & cont$WORD == "will" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "will" & cont$NP == "pro", ]$NEWTWO <- 1
cont[cont$OBSERVED == 4 & cont$WORD == "will" & cont$NP == "pro", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "would" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "would" & cont$NP == "pro", ]$NEWTWO <- 1
cont[cont$OBSERVED == 4 & cont$WORD == "would" & cont$NP == "pro", ]$NEWTWO <- 1
cont[cont$OBSERVED == 1 & cont$WORD == "are" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 4 & cont$WORD == "are" & cont$NP == "pro", ]$NEWTWO <- 1

#Additional 2-level coding where 'have' and 'had' split their ambiguous level-3 forms into both full and contracted at a rate of 25\% contraction for 'have' and 17\% contraction for 'had'. Other auxiliaries remain the same as in NEWTWO.
#This, of course, is no longer up to date... these ratios have since changed. Use at your own risk.
cont$URTWO <- as.factor(cont$NEWTWO)
#39 of the 96 level-3 have's after NPs become 1; the remaining 57 remain 4. The tokens that get recoded as 1 are just the first 39, no selectional criteria. How I arrived at the 39/57 breakdown: the predicted 30\% clitic insert rate predicts 116 intermediate forms of which 68 are from contracted and 48 are from full. That's a 59/41 ratio. I just applied that same ratio to the 96 level-3 have's that were actually observed.
cont[cont$WORD=="have"&cont$NP=="NP"&cont$OBSERVED==3,][1:39,]$URTWO = 1
#21 of the 39 level-3 had's after NPs become 4; the 1 level-4 had after NP stays 4 as do the remaining 18 level-3 had's after NPs. The tokens that get recoded as 4 are just the first 21, no selectional criteria.
cont[cont$WORD=="had"&cont$NP=="NP"&cont$OBSERVED==3,][1,]$URTWO = 4

########################
#aux-specific variables#
########################

had = subset(cont, WORD == "had")
has = subset(cont, WORD == "has")
have = subset(cont, WORD == "have")
is = subset(cont, WORD == "is")
will = subset(cont, WORD == "will")
would = subset(cont, WORD == "would")
are = subset(cont, WORD == "are")
did = subset(cont, WORD == "did")
does = subset(cont, WORD == "does")
d = subset(cont, WORD == "does"|WORD == "did")

##################################
#####     drop and clean     #####
##################################

had$PREC_GRAMM_CLASS = had$PREC_GRAMM_CLASS[, drop = TRUE]
had$CL_ENV = had$CL_ENV[, drop = TRUE]
had$SUBJ_COMPLEXITY = had$SUBJ_COMPLEXITY[, drop = TRUE]
had$FOLLOWING_BROAD = had$FOLLOWING_BROAD[, drop = TRUE]

has$PREC_GRAMM_CLASS = has$PREC_GRAMM_CLASS[, drop = TRUE]
has$CL_ENV = has$CL_ENV[, drop = TRUE]
has$SUBJ_COMPLEXITY = has$SUBJ_COMPLEXITY[, drop = TRUE]

have$PREC_GRAMM_CLASS = have$PREC_GRAMM_CLASS[, drop = TRUE]
have$CL_ENV = have$CL_ENV[, drop = TRUE]
have$SUBJ_COMPLEXITY = have$SUBJ_COMPLEXITY[, drop = TRUE]
have$PRONOUN = have$PRONOUN[, drop = TRUE]

is$PREC_GRAMM_CLASS = is$PREC_GRAMM_CLASS[, drop = TRUE]
is$CL_ENV = is$CL_ENV[, drop = TRUE]
is$SUBJ_COMPLEXITY = is$SUBJ_COMPLEXITY[, drop = TRUE]
is$CONTEXT = is$CONTEXT[, drop = TRUE]
is$CONTEXT2 = is$CONTEXT2[, drop = TRUE]

will$PREC_GRAMM_CLASS = will$PREC_GRAMM_CLASS[, drop = TRUE]
will$CL_ENV = will$CL_ENV[, drop = TRUE]
will$SUBJ_COMPLEXITY = will$SUBJ_COMPLEXITY[, drop = TRUE]

would$PREC_GRAMM_CLASS = would$PREC_GRAMM_CLASS[, drop = TRUE]
would$CL_ENV = would$CL_ENV[, drop = TRUE]
would$SUBJ_COMPLEXITY = would$SUBJ_COMPLEXITY[, drop = TRUE]
would$CONTEXT = would$CONTEXT[, drop = TRUE]
would$CONTEXT2 = would$CONTEXT2[, drop = TRUE]
would$FOLLOWING_BROAD = would$FOLLOWING_BROAD[, drop = TRUE]
would$PRONOUN = would$PRONOUN[, drop = TRUE]

are$PREC_GRAMM_CLASS = are$PREC_GRAMM_CLASS[, drop = TRUE]
are$CL_ENV = are$CL_ENV[, drop = TRUE]
are$SUBJ_COMPLEXITY = are$SUBJ_COMPLEXITY[, drop = TRUE]
are$CONTEXT = are$CONTEXT[, drop = TRUE]
are$FOLLOWING_BROAD = are$FOLLOWING_BROAD[, drop = TRUE]

does$PREC_GRAMM_CLASS = does$PREC_GRAMM_CLASS[, drop = TRUE]
does$CL_ENV = does$CL_ENV[, drop = TRUE]
does$SUBJ_COMPLEXITY = does$SUBJ_COMPLEXITY[, drop = TRUE]
does$CONTEXT = does$CONTEXT[, drop = TRUE]
does$FOLLOWING_BROAD = does$FOLLOWING_BROAD[, drop = TRUE]

did$PREC_GRAMM_CLASS = did$PREC_GRAMM_CLASS[, drop = TRUE]
did$CL_ENV = did$CL_ENV[, drop = TRUE]
did$SUBJ_COMPLEXITY = did$SUBJ_COMPLEXITY[, drop = TRUE]
did$CONTEXT = did$CONTEXT[, drop = TRUE]
did$FOLLOWING_BROAD = did$FOLLOWING_BROAD[, drop = TRUE]

d$WORD = d$WORD[, drop = TRUE]

#separate variables for the broad 'would' pragmatic contexts
 levels(would$CONTEXT2) = c("conditional", "imperfect", "heding", "politeness", "quoted", "none")
 
 would_hedge = subset(would, CONTEXT2 ==  "hedging")
 would_hedge$FOLLOWING_BROAD = would_hedge$FOLLOWING_BROAD[, drop = TRUE]
 
 would_polite = subset(would, CONTEXT2 ==  "politeness")
 would_polite$FOLLOWING_BROAD = would_polite$FOLLOWING_BROAD[, drop = TRUE]
 
 would_imp = subset(would, CONTEXT2 ==  "imperfect")
 would_imp$FOLLOWING_BROAD = would_imp$FOLLOWING_BROAD[, drop = TRUE]

#######################################
#aux-specific variables, post-pronouns#
#######################################

had_pron = subset(had, PREC_GRAMM_CLASS == "pronoun")
had_pron$PRONOUN = had_pron$PRONOUN[, drop = TRUE]
had_pron_N = table(had_pron$CL_ENV)
levels(had_pron$CL_ENV) = c("it", "others")

has_pron = subset(has, PREC_GRAMM_CLASS == "pronoun")
has_pron$CL_ENV = has_pron$CL_ENV[, drop = TRUE]
levels(has_pron$CL_ENV) = "(all)"

is_pron = subset(is, PREC_GRAMM_CLASS == "pronoun")
is_pron$CL_ENV = is_pron$CL_ENV[, drop = TRUE]
levels(is_pron$CL_ENV) = "(all)"

have_pron = subset(have, PREC_GRAMM_CLASS == "pronoun")
have_pron$CL_ENV = have_pron$CL_ENV[, drop = TRUE]
levels(have_pron$CL_ENV) = "(all)"

will_pron = subset(will, PREC_GRAMM_CLASS == "pronoun")
will_pron$PRONOUN = will_pron$PRONOUN[, drop = TRUE]
will_pron_N = table(will_pron$CL_ENV)
levels(will_pron$CL_ENV) = c("it", "others")
will_pron$UNREDUC = will_pron$UNREDUC[, drop = TRUE]

would_pron = subset(would, PREC_GRAMM_CLASS == "pronoun")
would_pron$PRONOUN = would_pron$PRONOUN[, drop = TRUE]

#for the 'would have' comparison
would_pron$UNREDUC = would_pron$UNREDUC[, drop = TRUE]
would_pron_N = table(would_pron$CL_ENV)
levels(would_pron$CL_ENV) = c("it", "others")

###################################
#aux-specific variables, post-'it'#
###################################

had_it = subset(had, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
had_it$PREC_GRAMM_CLASS = had_it$PREC_GRAMM_CLASS[, drop = TRUE]
had_it_N = table(had_it$PREC_GRAMM_CLASS)

has_it = subset(has, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
has_it$PREC_GRAMM_CLASS = has_it$PREC_GRAMM_CLASS[, drop = TRUE]
has_it_N = table(has_it$PREC_GRAMM_CLASS)

is_it = subset(is, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
is_it$PREC_GRAMM_CLASS = is_it$PREC_GRAMM_CLASS[, drop = TRUE]
is_it_N = table(is_it$PREC_GRAMM_CLASS)

will_it = subset(will, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
will_it$PREC_GRAMM_CLASS = will_it$PREC_GRAMM_CLASS[, drop = TRUE]
will_it_N = table(will_it$PREC_GRAMM_CLASS)

would_it = subset(would, PREC_GRAMM_CLASS == "pronoun"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "expletive"&PREC_SEG_M == "t")
would_it$PREC_GRAMM_CLASS = would_it$PREC_GRAMM_CLASS[, drop = TRUE]
would_it_N = table(would_it$PREC_GRAMM_CLASS)

#####################################
#aux-specific variables, post-'that'#
#####################################

had_that = subset(had, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
had_that$PREC_GRAMM_CLASS = had_that$PREC_GRAMM_CLASS[, drop = TRUE]
had_that_N = table(had_that$PREC_GRAMM_CLASS)

has_that = subset(has, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
has_that$PREC_GRAMM_CLASS = has_that$PREC_GRAMM_CLASS[, drop = TRUE]
has_that_N = table(has_that$PREC_GRAMM_CLASS)

is_that = subset(is, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
is_that$PREC_GRAMM_CLASS = is_that$PREC_GRAMM_CLASS[, drop = TRUE]
is_that_N = table(is_that$PREC_GRAMM_CLASS)

will_that = subset(will, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
will_that$PREC_GRAMM_CLASS = will_that$PREC_GRAMM_CLASS[, drop = TRUE]
will_that_N = table(will_that$PREC_GRAMM_CLASS)

would_that = subset(would, PREC_GRAMM_CLASS == "demonstrative"&PREC_SEG_M == "t"|PREC_GRAMM_CLASS == "relative"&PREC_SEG_M == "t")
would_that$PREC_GRAMM_CLASS = would_that$PREC_GRAMM_CLASS[, drop = TRUE]
would_that_N = table(would_that$PREC_GRAMM_CLASS)

#########################################
#aux-specific variables, post-expletives#
#########################################

had_exp = subset(had, PREC_GRAMM_CLASS == "expletive")
had_exp$PREC_SEG_M = had_exp$PREC_SEG_M[, drop = TRUE]
had_exp_N = table(had_exp$PREC_SEG_M)

has_exp = subset(has, PREC_GRAMM_CLASS == "expletive")
has_exp$PREC_SEG_M = has_exp$PREC_SEG_M[, drop = TRUE]
has_exp_N = table(has_exp$PREC_SEG_M)

is_exp = subset(is, PREC_GRAMM_CLASS == "expletive")
is_exp$PREC_SEG_M = is_exp$PREC_SEG_M[, drop = TRUE]
is_exp_N = table(is_exp$PREC_SEG_M)

will_exp = subset(will, PREC_GRAMM_CLASS == "expletive")
will_exp$PREC_SEG_M = will_exp$PREC_SEG_M[, drop = TRUE]
will_exp_N = table(will_exp$PREC_SEG_M)

would_exp = subset(would, PREC_GRAMM_CLASS == "expletive")
would_exp$PREC_SEG_M = would_exp$PREC_SEG_M[, drop = TRUE]
would_exp_N = table(would_exp$PREC_SEG_M)

##########################
#all auxes after pronouns#
##########################

#after all pronouns, regardless of cliticization capability
pron = subset(cont, PREC_GRAMM_CLASS == "pronoun")

#Because close phonology isn't possible after all pronouns (e.g. it + had, it + would, it + will) and sometimes I just want to focus on the ones where it is
#LOOKING AT ARE: Get rid of the anti-'are' restriction here!
pron_cl = subset(pron, CL_ENV=="y"&WORD!="are")
#pron_cl = subset(pron, CL_ENV=="y")
pron_cl$NEWTWO = pron_cl$NEWTWO[, drop = TRUE]
pron_cl$URTWO = pron_cl$URTWO[, drop = TRUE]
pron_cl$WORD = pron_cl$WORD[, drop = TRUE]

#pron_cl_noit just includes the vowel-final pronouns, to be consistent.

pron_cl_noit = subset(pron_cl, PRONOUN!="it")
pron_cl_noit$PRONOUN = pron_cl_noit$PRONOUN[, drop = TRUE]

#####################
#all auxes after NPs#
#####################

#I used to have NPadv folded in with NP. I've since decided that's not the best idea. There are still a few NPs that end in an adverb that get included in NP, but they're ones in which the adverb appears to be DP-attached (as far as I can tell), e.g. 'too, also, here, there, tonight, today'...
NP = subset(cont, NP == "NP" & WORD != "are")
#LOOKING AT ARE: Uncomment the next line and comment out the line above.
#NP = subset(cont, NP == "NP")

NP$WORD = NP$WORD[, drop = TRUE]
NP$NO_SYLLS = NP$NO_SYLLS[, drop = TRUE]
NP$NO_SYLLS = as.numeric(as.character(NP$NO_SYLLS))

#create a CV variable based on preceding segment
NP$CV <- as.character(NP$PREC_SEG_M)
NP[NP$PREC_SEG_M ==  "a", ]$CV <- "vowel"
NP[NP$PREC_SEG_M !=  "a", ]$CV <- "cons"
NP$CV = as.factor(NP$CV)

#########################
#number-of-syllable bins#
#########################

NP$NO_SYLLS_BIN <- as.character(NP$NO_SYLLS)
NP[NP$NO_SYLLS < 3, ]$NO_SYLLS_BIN <- "1-2"
NP[NP$NO_SYLLS >= 3 & NP$NO_SYLLS < 5, ]$NO_SYLLS_BIN <- "3-4"
NP[NP$NO_SYLLS >= 5 & NP$NO_SYLLS < 7, ]$NO_SYLLS_BIN <- "5-6"
NP[NP$NO_SYLLS >= 7, ]$NO_SYLLS_BIN <- "7+"

#alternative: only bin anything above 10
#NP[NP$NO_SYLLS > 10 & NP$NO_SYLLS <=  15, ]$NO_SYLLS_BIN <- "10-15"
#NP[NP$NO_SYLLS > 15, ]$NO_SYLLS_BIN <- "16+"
NP$NO_SYLLS_BIN = as.factor(NP$NO_SYLLS_BIN)
#NP$NO_SYLLS_BIN = factor(NP$NO_SYLLS_BIN, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10-15", "16+"))

################
#subject weight#
################

#"subject weight" metric: number of syllables x 1 for single word, x 2 for multi word, x 3 for embedded clause
NP$SUBJ_WEIGHT = NP$NO_SYLLS
NP[NP$SUBJ_COMPLEXITY=="multi", ]$SUBJ_WEIGHT = NP[NP$SUBJ_COMPLEXITY=="multi", ]$NO_SYLLS * 2
NP[NP$SUBJ_COMPLEXITY=="emb", ]$SUBJ_WEIGHT = NP[NP$SUBJ_COMPLEXITY=="emb", ]$NO_SYLLS * 3

#now bin the subject weight metric
NP$SUBJ_WEIGHT_BIN <- as.character(NP$SUBJ_WEIGHT)
NP[NP$SUBJ_WEIGHT < 6, ]$SUBJ_WEIGHT_BIN <- "1-5"
NP[NP$SUBJ_WEIGHT >= 6 & NP$SUBJ_WEIGHT < 11, ]$SUBJ_WEIGHT_BIN <- "6-10"
NP[NP$SUBJ_WEIGHT >= 11 & NP$SUBJ_WEIGHT < 16, ]$SUBJ_WEIGHT_BIN <- "11-15"
NP[NP$SUBJ_WEIGHT >= 16 & NP$SUBJ_WEIGHT < 21, ]$SUBJ_WEIGHT_BIN <- "16-20"
NP[NP$SUBJ_WEIGHT >= 21 & NP$SUBJ_WEIGHT < 31, ]$SUBJ_WEIGHT_BIN <- "21-30"
NP[NP$SUBJ_WEIGHT >= 31 & NP$SUBJ_WEIGHT < 41, ]$SUBJ_WEIGHT_BIN <- "31-40"
NP[NP$SUBJ_WEIGHT >= 41, ]$SUBJ_WEIGHT_BIN <- "41+"
NP$SUBJ_WEIGHT_BIN = factor(NP$SUBJ_WEIGHT_BIN, levels = c("1-5", "6-10", "11-15", "16-20", "21-30", "31-40"))

#############################################################################
#all auxes after NPs EXCEPT sibiliant-final ones where the aux is 'is'/'has'#
#############################################################################

#When we do the NP analysis, we don't want to include is/has after sibilant-final NPs because any schwa-initial forms we see there could be due to schwa-insertion for geminate avoidance. So we want to omit such forms, but include all others. The NP_EX(amine) field allows us to do this. (This is the same thing we did with would/had after 'it': cut out the ones where a schwa could be for phonotactic reasons.) All analyses of NPs are done on this body of data. This only ends up affecting is/has and really doesn't cut out that much data.
#LOOKING AT ARE: Uncomment the 'are' lines to add 'are' back into the NP plots. Adding 'are' back in will mess up NP/pronoun line graphs and the regressions therein. Uncomment the commented stuff down there to get it to work out.
NP$NP_EX <- as.factor(NP$CL_ENV)
NP[NP$WORD == "had" & NP$CL_ENV == "y", ]$NP_EX <- "y"
NP[NP$WORD == "had" & NP$CL_ENV == "n", ]$NP_EX <- "y"
NP[NP$WORD == "have" & NP$CL_ENV == "y", ]$NP_EX <- "y"
NP[NP$WORD == "have" & NP$CL_ENV == "n", ]$NP_EX <- "y"
NP[NP$WORD == "would" & NP$CL_ENV == "y", ]$NP_EX <- "y"
NP[NP$WORD == "would" & NP$CL_ENV == "n", ]$NP_EX <- "y"
NP[NP$WORD == "will" & NP$CL_ENV == "y", ]$NP_EX <- "y"
NP[NP$WORD == "will" & NP$CL_ENV == "n", ]$NP_EX <- "y"
NP[NP$WORD == "would" & NP$CL_ENV == "n", ]$NP_EX <- "y"
#NP[NP$WORD == "are" & NP$CL_ENV == "y", ]$NP_EX <- "y"
#NP[NP$WORD == "are" & NP$CL_ENV == "n", ]$NP_EX <- "y"
NP_EX = subset(NP, NP_EX == "y")

#do some dropping
NP_EX$NEWTWO = NP_EX$NEWTWO[, drop = TRUE]
NP_EX$URTWO = NP_EX$URTWO[, drop = TRUE]
NP_EX$ONEFOUR = NP_EX$ONEFOUR[, drop = TRUE]
NP_EX$TWOALT = NP_EX$TWOALT[, drop = TRUE]
NP_EX$WORD = NP_EX$WORD[, drop = TRUE]

#opposing single-word subjects to multi-word subjects without caring about embedding
NP_EX$COMP2 <- as.factor(NP_EX$SUBJ_COMPLEXITY)
NP_EX[NP_EX$SUBJ_COMPLEXITY ==  "emb", ]$COMP2 <- "multi"
NP_EX$COMP2 = NP_EX$COMP2[, drop = TRUE]

#make duration numeric
NP_EX$SUBJ_DUR = as.numeric(as.character(NP_EX$SUBJ_DUR))

######################################
#aux-specific variables, post-NP only#
######################################

#These variables are formed based on NP_EX, which doesn't have any effect on have/had/will/would but omits tokens after sibilants for has/is.
had_NP = subset(NP_EX, WORD == "had")
had_NP$SUBJ_COMPLEXITY = had_NP$SUBJ_COMPLEXITY[, drop = TRUE]
had_NP_N = table(had_NP$SUBJ_COMPLEXITY)
had_NP$PREC_STRESS = had_NP$PREC_STRESS[, drop = TRUE]
had_NP_stress_N = table(had_NP$PREC_STRESS)

has_NP = subset(NP_EX, WORD == "has")
has_NP$SUBJ_COMPLEXITY = has_NP$SUBJ_COMPLEXITY[, drop = TRUE]
has_NP_N = table(has_NP$SUBJ_COMPLEXITY)
has_NP$PREC_STRESS = has_NP$PREC_STRESS[, drop = TRUE]
has_NP_stress_N = table(has_NP$PREC_STRESS)

have_NP = subset(NP_EX, WORD == "have")
have_NP$SUBJ_COMPLEXITY = have_NP$SUBJ_COMPLEXITY[, drop = TRUE]
have_NP_N = table(have_NP$SUBJ_COMPLEXITY)
have_NP$PREC_STRESS = have_NP$PREC_STRESS[, drop = TRUE]
have_NP_stress_N = table(have_NP$PREC_STRESS)

#to see if h-deletion of 'have' differs after s-final vs. non-s-final nouns
have_NP$S <- as.character(have_NP$PREC_SEG_M)
have_NP[have_NP$PREC_SEG_M == "s",]$S <- "s"
have_NP[have_NP$PREC_SEG_M != "s",]$S <- "other"
have_NP$S = as.factor(have_NP$S)

is_NP = subset(NP_EX, WORD == "is")
is_NP$SUBJ_COMPLEXITY = is_NP$SUBJ_COMPLEXITY[, drop = TRUE]
is_NP_N = table(is_NP$SUBJ_COMPLEXITY)
is_NP$PREC_STRESS = is_NP$PREC_STRESS[, drop = TRUE]
is_NP_stress_N = table(is_NP$PREC_STRESS)

will_NP = subset(NP_EX, WORD == "will")
will_NP$SUBJ_COMPLEXITY = will_NP$SUBJ_COMPLEXITY[, drop = TRUE]
will_NP_N = table(will_NP$SUBJ_COMPLEXITY)
will_NP$PREC_STRESS = will_NP$PREC_STRESS[, drop = TRUE]
will_NP_stress_N = table(will_NP$PREC_STRESS)
will_NP$UNREDUC = will_NP$UNREDUC[, drop = TRUE]

would_NP = subset(NP_EX, WORD == "would")
would_NP$SUBJ_COMPLEXITY = would_NP$SUBJ_COMPLEXITY[, drop = TRUE]
would_NP_N = table(would_NP$SUBJ_COMPLEXITY)
would_NP$PREC_STRESS = would_NP$PREC_STRESS[, drop = TRUE]
would_NP_stress_N = table(would_NP$PREC_STRESS)
#for the 'would have' comparison
would_NP$UNREDUC = would_NP$UNREDUC[, drop = TRUE]

#has, is, and will together -- the three whose surface forms can be unambiguously attributed to underlying forms.
hiw = subset(NP_EX, WORD=="has"|WORD=="is"|WORD=="will")
hiw$PREC_GRAMM_CLASS = hiw$PREC_GRAMM_CLASS[, drop = TRUE]

hiw$RECODE_THREE <- as.numeric(hiw$THREE)
hiw[hiw$WORD=="is"&hiw$THREE==1,]$RECODE_THREE = 0
hiw[hiw$WORD=="is"&hiw$THREE==4,]$RECODE_THREE = 1
hiw[hiw$WORD=="has"&hiw$NEWTWO ==0,]$RECODE_THREE = 0
hiw[hiw$WORD=="has"&hiw$NEWTWO ==1,]$RECODE_THREE = 1
hiw[hiw$WORD=="will"&hiw$NEWTWO ==0,]$RECODE_THREE = 0
hiw[hiw$WORD=="will"&hiw$NEWTWO ==1,]$RECODE_THREE = 1

##########
#wh-words#
##########

#for the do-aux analyses
how = subset(cont, PREC_GRAMM_CLASS=="how")
how$WORD = how$WORD[, drop = TRUE]

where = subset(cont, PREC_GRAMM_CLASS=="where")
where$WORD = where $WORD[, drop = TRUE]

############
#would have#
############

#wouldhave goes in its own variable because it was measured on a different scale than the others
wouldhave = subset(data, UNREDUC == "not unreducible"&WORD == "wouldhave")

#rename preceding grammatical class 
wouldhave$PREC_GRAMM_CLASS = wouldhave$PREC_GRAMM_CLASS[, drop = TRUE]

wouldhave$CL_ENV = wouldhave$CL_ENV[, drop = TRUE]

wouldhave$SUBJ_COMPLEXITY = wouldhave$SUBJ_COMPLEXITY[, drop = TRUE]

#so I can cheat and have stacked bar graphs even though there's only one level on the x-axis
wouldhave$UNREDUC = wouldhave$UNREDUC[, drop = TRUE]

#for South vs. other contraction vs. full analysis
wouldhave$SOUTHOTHER <- as.character(wouldhave$DIALECT)
wouldhave[wouldhave$DIALECT == "SOUTH MIDLAND", ]$SOUTHOTHER <- "South Midland"
wouldhave[wouldhave$DIALECT != "SOUTH MIDLAND", ]$SOUTHOTHER <- "other"
wouldhave$SOUTHOTHER <- as.factor(wouldhave$SOUTHOTHER)

wouldhave$TWOLEVELS <- as.character(wouldhave$OBSERVED)
wouldhave$TWOLEVELS <- "full"
wouldhave[wouldhave$OBSERVED == 8|wouldhave$OBSERVED == 9, ]$TWOLEVELS <- "contracted"
wouldhave$TWOLEVELS <- as.factor(wouldhave$TWOLEVELS)

wouldhave_pron = subset(wouldhave, PREC_GRAMM_CLASS == "pronoun")
levels(wouldhave_pron$CL_ENV) = c("it", "others")

wouldhave_NP = subset(wouldhave, NP == "NP")

###############################
######      arrange      ######
###############################

#Code creating an arrange() function that simulates layout() for ggplot!

vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
 dots <- list(...)
 n <- length(dots)
 if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
 if(is.null(nrow)) { nrow = ceiling(n/ncol)}
 if(is.null(ncol)) { ncol = ceiling(n/nrow)}
        ## NOTE see n2mfrow in grDevices for possible alternative
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
 ii.p <- 1
 for(ii.row in seq(1, nrow)){
 ii.table.row <- ii.row 
 if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
  for(ii.col in seq(1, ncol)){
   ii.table <- ii.p
   if(ii.p > n) break
   print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
   ii.p <- ii.p + 1
  }
 }
}

# CEL - I commented past here because I don't have the needed files.

################################
######     recontrast     ######
################################

# source("/Users/laurel/Dropbox/Dissertation/Empirical/Contraction/Scripts/recontrast.R")


################################
######   multi speakers   ######
################################

# speakers = read.csv('/Users/laurel/Dropbox/Dissertation/Empirical/Contraction/multi_speakers.csv')
# d = data.frame(unique(speakers[,4]), paste("sw_", unique(speakers[,4]), sep = ""))
# colnames(d)[2] = "PIN"
# 
# multi = subset(cont, SPEAKER %in% d$PIN)
# multi$SPEAKER = multi$SPEAKER[, drop = TRUE]
# 
# multi_ex = rbind(subset(multi, NP=="pro" & (WORD=="has" | WORD=="had" | WORD == "have") & CL_ENV == "y"), subset(multi, WORD == "has" & NP == "NP" & CL_ENV == "y"))
# 
# #word, NP, speaker, sex, YOB, dialect, educ, subject, depvar
# joe = multi_ex[,c(1,14,20,21,22,23,24,37,44)]
# colnames(joe)[c(2,3,9)] = c("SUBJ","SPEAKER_PIN","DEP_VAR")
# joe$MORPH = as.character(joe$DEP_VAR)
# joe$PHON = as.character(joe$DEP_VAR)
# joe[joe$DEP_VAR=="4",]$MORPH = 1
# joe[joe$DEP_VAR=="4",]$PHON = NA
# joe[joe$DEP_VAR=="3",]$MORPH = 0
# joe[joe$DEP_VAR=="3",]$PHON = 1
# joe[joe$DEP_VAR=="1",]$MORPH = 0
# joe[joe$DEP_VAR=="1",]$PHON = 0
# joe$MORPH = as.factor(joe$MORPH)
# joe$PHON = as.factor(joe$PHON)
#write.csv(joe, file = "contraction_speakers.csv", quote = F, row.names = F)