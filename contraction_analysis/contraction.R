contraction_data = read.csv("data/contraction_3.8.12_probs.csv")
l560_data = read.csv("data/560_contractions_3.8.12_probs.csv")
data = rbind(contraction_data, l560_data)

################################
######     recontrast     ######
################################

source("recontrast.R")

#########################
#rename one-letter codes#
#########################

#unreducible contexts
levels(data$UNREDUC) = c("all-cleft", "pseudocleft", "comparative sub", "ellipsis-deletion", "ellipsis-comparative", "hesitation", "infinitival", "inverse specificational", "locative", "null subject", "preceding pause", "parenthetical", "question", "stressed", "specificational", "thing-is", "that-thing", "do-cleft", "not unreducible", "yes-no question")

#preceding stress
levels(data$PREC_STRESS) = c("aux", "subject", "monosyll", "other", "participle", "stressed", "unstressed")

#preceding grammatical class
levels(data$PREC_GRAMM_CLASS) = c("adjective", "adverb", "auxiliary", "demonstrative", "disfluency", "emb pronoun", "how", "noun", "pronoun", "preposition", "quantifier", "relative", "verb", "wh-word", "where", "who", "why", "expletive")

#sex -- this is for readability of regression outputs...
levels(data$SEX) = c(" = F", " = M")

#corpus -- likewise
levels(data$CORPUS) = c(" = Fisher", " = Switchboard", " = PNC")

#auxiliary -- likewise
data$AUXILIARY = data$WORD
levels(data$AUXILIARY) = c(" = are", " = auxhave", " = did", " = does", " = had", " = has", " = have", " = is", " = will", " = would", " = wouldhave")

#############################
####   decade of birth   ####
#############################

#decade of birth
data$DOB <- cut(data$YOB, breaks = seq(from = 1890, to = 1990, by = 10), include.lowest = T, right = F, labels = paste(seq(from = 1890, to = 1980, by = 10)))
#data$DOB = as.numeric(as.character(data$DOB))
data$DOB = as.numeric(as.factor(data$DOB)) #the only way we can get the mixed-effects glms to converge apparently...

#age
data$AGE = data$YEAR_RECORDED - data$YOB

#############################
#######    cont    ##########
#############################

#cont consists of all contractible data (i.e. not in unreducible contexts) but excludes wouldhave and auxhave. Also exclude the AAVE speakers from 560 since their dialect may contract differently.
cont_hasgot = subset(data, (UNREDUC == "not unreducible"|UNREDUC == "inverse specificational" | UNREDUC == "specificational") &WORD!= "wouldhave"&WORD!= "auxhave" & DIALECT != "PHILADELPHIA_AAVE")
cont = subset(cont_hasgot, !(cont_hasgot$NP == "pro" & cont_hasgot$WORD == "has" & cont_hasgot$FOLL_WORD == "got"))

#some level-dropping
cont$WORD = cont$WORD[, drop = TRUE]
cont$PREC_GRAMM_CLASS = cont$PREC_GRAMM_CLASS[, drop = TRUE]
cont$PREC_STRESS = cont$PREC_STRESS[, drop = TRUE]

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
#cont[cont$WORD == "would" & cont$CONTEXT == "s", ]$CONTEXT2 <- "h"
#cont[cont$WORD == "would" & cont$CONTEXT == "t", ]$CONTEXT2 <- "h"
#cont[cont$WORD == "would" & cont$CONTEXT == "r", ]$CONTEXT2 <- "p"
#cont[cont$WORD == "would" & cont$CONTEXT == "l", ]$CONTEXT2 <- "p"
#cont$CONTEXT2 = as.factor(cont$CONTEXT2)
#I commented these guys out because I ended up doing CONTEXT2 in a more principled way in the thesis.

#############################
# let's recontrast some things before we break out into smaller variables
#############################

cont$WORD = recontrast(cont$WORD)
cont$PREC_STRESS = recontrast(cont$PREC_STRESS)
cont$PREC_SEG_M = recontrast(cont$PREC_SEG_M)
cont$PREC_SEG_P = recontrast(cont$PREC_SEG_P)
cont$PRONOUN = recontrast(cont$PRONOUN)
cont$DIALECT = recontrast(cont$DIALECT)
cont$SEX = recontrast(cont$SEX)
cont$CORPUS = recontrast(cont$CORPUS)

#####################################################
#group years of education for compatibility with SWB#
#####################################################

#One way: force specific years of schooling into SWB's coarse categories:
#0 = less than high school
#1 = less than college
#2 = college
#3 = more than college
cont$EDUC_STEP <- cont$EDUC
cont[cont$EDUC == 9 & cont$CORPUS == " = Switchboard",]$EDUC_STEP = "NA"
cont[cont$EDUC < 12 & (cont$CORPUS == " = Fisher" | cont$CORPUS == " = PNC"),]$EDUC_STEP <- 0
cont[cont$EDUC >= 12 & cont$EDUC < 16 & (cont$CORPUS == " = Fisher" | cont$CORPUS == " = PNC"),]$EDUC_STEP <- 1
cont[cont$EDUC == 16 & (cont$CORPUS == " = Fisher" | cont$CORPUS == " = PNC"),]$EDUC_STEP <- 2
cont[cont$EDUC > 16 & (cont$CORPUS == " = Fisher" | cont$CORPUS == " = PNC"),]$EDUC_STEP <- 3
cont$EDUC_STEP = as.numeric(cont$EDUC_STEP)

#Another way: approximate SWB's coarse categories to years of schooling, as follows:
#less than high school = 9
#less than college = 12
#college = 16
#more than college = 18
cont$EDUC_EST <- cont$EDUC
cont[cont$EDUC == 9 & cont$CORPUS == " = Switchboard",]$EDUC_EST = "NA"
cont[cont$EDUC == 0,]$EDUC_EST = 9
cont[cont$EDUC == 1,]$EDUC_EST = 12
cont[cont$EDUC == 2,]$EDUC_EST = 16
cont[cont$EDUC == 3,]$EDUC_EST = 18
cont$EDUC_EST = as.numeric(cont$EDUC_EST)

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

#Additional 2-level coding where what happens to 3 is dependent on auxiliary identity:
#had: 1, 2, 3 vs. 4 (problematic after NPs, where they're actually hybrid, but works post-pronouns with the exception of 'it' where they're hybrid)
#has: 1, 2, 3 vs. 4
#have: NP: 1, 2 vs. 3, 4; pro: 1, 2, 3 vs. 4 (problematic after NPs, where they're actually hybrid, but works post-pronouns)
#is: 1, 2 vs. 4
#will: 1, 2 vs. 3, 4
#would: 1 vs. 3, 4 (no 2)
#are: 1, 2 vs. 4
cont$NEWTWO <- as.numeric(cont$OBSERVED)
cont[cont$OBSERVED == 1, ]$NEWTWO <- 0
cont[cont$OBSERVED == 2, ]$NEWTWO <- 0
cont[cont$OBSERVED == 4, ]$NEWTWO <- 1

cont[cont$OBSERVED == 3 & cont$WORD == "had", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "has", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "have" & cont$NP == "NP", ]$NEWTWO <- 1
cont[cont$OBSERVED == 3 & cont$WORD == "have" & cont$NP == "pro", ]$NEWTWO <- 0
cont[cont$OBSERVED == 3 & cont$WORD == "will", ]$NEWTWO <- 1
cont[cont$OBSERVED == 3 & cont$WORD == "would", ]$NEWTWO <- 1
#cont$NEWTWO = as.factor(cont$NEWTWO) #Making it categorical doesn't allow us to do scatterplots with NEWTWO on the y-axis: ggplot gets mad that it's non-continuous. It doesn't seem to affect the regressions if it's continuous...

#Joe!
cont$JOE = 1.1
cont[cont$NEWTWO == 0,]$JOE = -.1

####################
# relative clauses #
####################

rel_all = subset(cont, PREC_GRAMM_CLASS=="relative")
#Just pulling out is/has/will after who & that for now. We could throw in other auxes in certain environments, but then it would be unbalanced, and we just don't have that much data.
rel_all[rel_all$OBSERVED == 3 & rel_all$WORD == "has", ]$NEWTWO <- 0
rel_all[rel_all$OBSERVED == 3 & rel_all$WORD == "will", ]$NEWTWO <- 1
rel = rbind(subset(rel_all, WORD=="is" & (PREC_SEG_M=="t"|PREC_SEG_M=="a")),
subset(rel_all, WORD=="has" & (PREC_SEG_M=="t"|PREC_SEG_M=="a")),
subset(rel_all, WORD=="will" & (PREC_SEG_M=="t"|PREC_SEG_M=="a")))

rel$REST = 0
rel[!(is.na(rel$NOTES)) & rel$NOTES == "non-restrictive relative",]$REST = 1
rel$REST = as.factor(rel$REST)

#		that		who			which
#had	x			√			x 3 -> 1 but ambig
#has	√			√			x 3 -> 1 but ambig
#have	x			√			x 3 -> 1 but ambig
#is		√			√			x 3 ambig
#will	x			√			x 3 -> 1
#would	x			√			x 3 -> 1

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

levels(would$CONTEXT2) = c("conditional", "hedging", "imperfect", "none", "politeness", "quoted")

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

has_prongot = subset(has, PREC_GRAMM_CLASS == "pronoun")
has_pron = subset(has_prongot, !(has_prongot$FOLL_WORD == "got" & has_prongot$NP != "NP"))
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

#create a CV variable based on preceding segment
NP$CV <- as.character(NP$PREC_SEG_M)
NP[NP$PREC_SEG_M ==  "a", ]$CV <- "vowel"
NP[NP$PREC_SEG_M !=  "a", ]$CV <- "cons"
NP$CV = as.factor(NP$CV)

#############################################################################
#all auxes after NPs EXCEPT sibilant-final ones where the aux is 'is'/'has'#
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
NP_EX$ONEFOUR = NP_EX$ONEFOUR[, drop = TRUE]
NP_EX$TWOALT = NP_EX$TWOALT[, drop = TRUE]
NP_EX$WORD = NP_EX$WORD[, drop = TRUE]

#make duration numeric
NP_EX$SUBJ_DUR = as.numeric(as.character(NP_EX$SUBJ_DUR))

#disfluency count
NP_EX$DIS = NP_EX$NO_WORDS_DIS-NP_EX$NO_WORDS

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
hiw$WORD = hiw$WORD[, drop = TRUE]
hiw$AUXILIARY = hiw$AUXILIARY[, drop = TRUE]

#What is this, my son Tom? Why not just use NEWTWO? Why did I do this...
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

###################
#    corstars1    #
###################

#Makes a nice correlation table, with only the lower triangle of the matrix so no values are redundant, and stars representing significant correlations! Input is a matrix of raw data, the columns that you want correlations between.

corstars <- function(x){
	require(Hmisc)
	x <- as.matrix(x)
	R <- rcorr(x, type = "spearman")$r
	p <- rcorr(x, type = "spearman")$P
	
	## define notions for significance levels; spacing is important
	mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
	
	## truncate the matrix that holds the correlations to two decimal
	R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
	
	## build a new matrix that includes the correlations with their appropriate stars
	Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
	diag(Rnew) <- paste(diag(R), " ", sep = "")
	rownames(Rnew) <- colnames(x)
	colnames(Rnew) <- paste(colnames(x), "", sep = "")
	
	## remove upper triangle
	Rnew <- as.matrix(Rnew)
	Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
	Rnew <- as.data.frame(Rnew)
	
	## remove first row and last column and return the matrix (which is now a data frame)
	Rnew <- cbind(Rnew[1:length(Rnew)-1])
	Rnew <- rbind(Rnew[2:nrow(Rnew),])
	return(Rnew)
	}