library(lme4)
library(ggplot2)

# Load up all the data and preprocessing, etc.
source("contraction.R")
# Reduce to the subset we're looking at.
hiw_NP = subset(hiw, NP == "NP")
# NEWTWO appears to be continuous and not categorial. Recoding here.
hiw_NP$WORD = recontrast(hiw_NP$WORD) #change from treatment to sum contrasts because no one auxiliary is default
# We use decade of birth as the numeric levels of the factor so it's continuous. Coding as the actual years
# or, strangely, as the continuous decades of birth at the original number (e.g., 1950) causes false convergence.
hiw_NP$DOB = as.numeric(as.factor(hiw_NP$DOB))
# Add in number of functional elements, which is orthographic words - phonological words
hiw_NP$NO_FUNC_WORDS = hiw_NP$NO_WORDS - hiw_NP$NO_PHON_WORDS
summary(hiw_NP)

# Get the correlations
hiw_NP_corr = subset(hiw_NP, !is.na(EDUC_STEP), select = c(NO_WORDS, NO_SYLLS, NO_PHON_WORDS, NO_FUNC_WORDS, SPEAKING_RATE, DOB, EDUC_STEP))
# Replace some predictors with their log version, adding one if needed to prevent zeros
hiw_NP_corr$NO_WORDS = log(hiw_NP_corr$NO_WORDS)
hiw_NP_corr$NO_SYLLS = log(hiw_NP_corr$NO_SYLLS)
hiw_NP_corr$NO_PHON_WORDS = log(hiw_NP_corr$NO_PHON_WORDS + 1)
hiw_NP_corr$NO_FUNC_WORDS = log(hiw_NP_corr$NO_FUNC_WORDS + 1)
cor(hiw_NP_corr)
# Check again on the subset that has parses
hiw_NP_corr = subset(hiw_NP_corr, !is.na(SUBJ_DEPTH), select = c(NO_WORDS, NO_SYLLS, NO_PHON_WORDS, NO_FUNC_WORDS, SUBJ_DEPTH, SPEAKING_RATE, DOB, EDUC_STEP))
cor(hiw_NP_corr)

# A base model
hiw_NP_NO_WORDS.lme = lmer(NEWTWO ~ log(NO_WORDS) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_WORDS.lme)

# Different possible NO_WORDS collinear predictors
hiw_NP_NO_SYLLS.lme = lmer(NEWTWO ~ log(NO_SYLLS) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_SYLLS.lme)
hiw_NP_NO_PHON_WORDS.lme = lmer(NEWTWO ~ log(NO_PHON_WORDS + 1) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_WORDS.lme)
hiw_NP_NO_FUNC_WORDS.lme = lmer(NEWTWO ~ log(NO_FUNC_WORDS + 1) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_FUNC_WORDS.lme)

# Something goes wrong with subject duration
hiw_NP_SUBJ_DUR.lme = lmer(NEWTWO ~ SUBJ_DUR + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_SUBJ_DUR.lme)

# Residualization off of N_WORDS
NO_SYLLS_rwords = lm(log(NO_SYLLS) ~ log(NO_WORDS), hiw_NP)$resid
NO_PHON_WORDS_rwords = lm(log(NO_PHON_WORDS + 1) ~ log(NO_WORDS), hiw_NP)$resid
NO_FUNC_WORDS_rwords = lm(log(NO_FUNC_WORDS + 1) ~ log(NO_WORDS), hiw_NP)$resid

# N_WORDS plus each of the resids
hiw_NP_NO_WORDS_SYLLS.lme = lmer(NEWTWO ~ log(NO_WORDS) + NO_SYLLS_rwords + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_WORDS_SYLLS.lme)
hiw_NP_NO_WORDS_PHON.lme = lmer(NEWTWO ~ log(NO_WORDS) + NO_PHON_WORDS_rwords + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_WORDS_PHON.lme)
hiw_NP_NO_WORDS_FUNC.lme = lmer(NEWTWO ~ log(NO_WORDS) + NO_FUNC_WORDS_rwords + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_WORDS_FUNC.lme)

# Residualization of N_WORDS against each
# Sylls
NO_WORDS_rsylls = lm(log(NO_WORDS) ~ log(NO_SYLLS), hiw_NP)$resid
hiw_NP_NO_SYLLS_WORDS.lme = lmer(NEWTWO ~ log(NO_SYLLS) + NO_WORDS_rsylls + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_SYLLS_WORDS.lme)
# Phon words
NO_WORDS_rphon = lm(log(NO_WORDS) ~ log(NO_PHON_WORDS + 1), hiw_NP)$resid
hiw_NP_NO_PHON_WORDS.lme = lmer(NEWTWO ~ log(NO_PHON_WORDS + 1) + NO_WORDS_rphon + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_WORDS.lme)
# Func words
NO_WORDS_rfunc = lm(log(NO_WORDS) ~ log(NO_FUNC_WORDS + 1), hiw_NP)$resid
hiw_NP_NO_FUNC_WORDS.lme = lmer(NEWTWO ~ log(NO_FUNC_WORDS + 1) + NO_WORDS_rfunc + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_FUNC_WORDS.lme)

# Func and phon together
NO_WORDS_FUNC_rphon = lm(log(NO_FUNC_WORDS + 1) ~ log(NO_PHON_WORDS + 1), hiw_NP)$resid
hiw_NP_NO_PHON_FUNC.lme = lmer(NEWTWO ~ log(NO_PHON_WORDS + 1) + NO_WORDS_FUNC_rphon + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_FUNC.lme)
# Reverse order to see coefficient
NO_PHON_WORD_rfunc = lm(log(NO_PHON_WORDS + 1) ~ log(NO_FUNC_WORDS + 1), hiw_NP)$resid
hiw_NP_NO_FUNC_PHON.lme = lmer(NEWTWO ~ log(NO_FUNC_WORDS + 1) + NO_PHON_WORD_rfunc + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_FUNC_PHON.lme)

# WIN
NO_WORDS_rphonfunc = lm(NO_WORDS ~ log(NO_PHON_WORDS + 1) + NO_WORDS_FUNC_rphon, hiw_NP)$resid
hiw_NP_NO_PHON_FUNC_WORDS.lme = lmer(NEWTWO ~ log(NO_PHON_WORDS + 1) + NO_WORDS_FUNC_rphon + NO_WORDS_rphonfunc + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_FUNC_WORDS.lme)

# Subject depth (AKA height) is n.s. Nothing to see here.
hiw_NP_parsed = subset(hiw_NP, !is.na(SUBJ_DEPTH))
DEPTH_rphon = lm(SUBJ_DEPTH ~ log(NO_PHON_WORDS + 1), hiw_NP_parsed)$resid
hiw_NP_DEPTH_NO_PHON_WORDS.lme = lmer(NEWTWO ~ log(NO_PHON_WORDS + 1) + DEPTH_rphon + SPEAKING_RATE + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP_parsed, family = 'binomial')
summary(hiw_NP_DEPTH_NO_PHON_WORDS.lme)
# Throw in some syllables, for a challenge, still significant
NO_SYLLS_rphon = lm(log(NO_SYLLS) ~ log(NO_PHON_WORDS + 1), hiw_NP)$resid
hiw_NP_NO_PHON_SYLL.lme = lmer(NEWTWO ~ log(NO_PHON_WORDS + 1) + NO_SYLLS_rphon + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_SYLL.lme)
# TODO: Try with number of right brackets

# Pile on orth. words for extra challenge
NO_WORDS_rphonsylls = lm(log(NO_WORDS) ~ log(NO_PHON_WORDS + 1) + NO_SYLLS_rphon, hiw_NP)$resid
hiw_NP_NO_PHON_SYLL_WORDS.lme = lmer(NEWTWO ~ log(NO_PHON_WORDS + 1) + NO_SYLLS_rphon + NO_WORDS_rphonsylls + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_SYLL_WORDS.lme)


hiw_NP$JOE = 1.1
hiw_NP[hiw_NP$NEWTWO == 0,]$JOE = -.1

#orthographic words
ortho = ggplot(hiw_NP, aes(NO_WORDS, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(limits = c(1,18), name = "number of orthographic words") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "orthographic words")
#pros words
pros = ggplot(hiw_NP, aes(NO_PHON_WORDS, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(limits = c(1,10), name = "number of prosodic words") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "prosodic words")
#sylls
sylls = ggplot(hiw_NP, aes(NO_SYLLS, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "number of syllables") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "syllables")
#speaking rate (n.s.)
ggplot(hiw_NP, aes(SPEAKING_RATE, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "speaking rate") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "speaking rate (words/sec)")
#subject depth
depth = ggplot(hiw_NP, aes(SUBJ_DEPTH, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "height of parse") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "height")
arrange(ortho, pros, sylls, depth, ncol = 2)
# Also check out function words
func = ggplot(hiw_NP, aes(NO_FUNC_WORDS, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "number of function words") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "function words")
arrange(pros, func)
