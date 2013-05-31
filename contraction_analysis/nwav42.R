library(lme4.0)
library(ggplot2)

# Load up all the data and preprocessing, etc.
source("contraction.R")
# Reduce to the subset we're looking at.
hiw.NP = subset(hiw, NP == "NP")
# Change from treatment to sum contrasts because no one auxiliary is default
hiw.NP$WORD = recontrast(hiw.NP$WORD) 
# Center DOB
hiw.NP$DOB = scale(hiw.NP$DOB, scale = FALSE)
# Add the general code of number of function words as the sum of monosyllabic and multisyllabic
hiw.NP$NO_FUNC_WORDS = hiw.NP$NO_FUNC_WORDS_MONO + hiw.NP$NO_FUNC_WORDS_MULTI
# Compute information metrics
hiw.NP$PFORWARD = -log2(hiw.NP$PFORWARD)
hiw.NP$PBACKWARD = -log2(hiw.NP$PBACKWARD)
hiw.NP$PHOST = log2(hiw.NP$PHOST)
hiw.NP$PAFTER = log2(hiw.NP$PAFTER)
summary(hiw.NP)

# Get the correlations
hiw.NP.corr = subset(hiw.NP, !is.na(EDUC_STEP), select = c(NEWTWO, NO_WORDS, NO_SYLLS, NO_PHON_WORDS, NO_FUNC_WORDS, NO_FUNC_WORDS_MONO, NO_FUNC_WORDS_MULTI, SPEAKING_RATE, PFORWARD, PBACKWARD, PHOST, PAFTER))
# Replace some predictors with their log version, adding one if needed to prevent zeros
hiw.NP.corr$NO_WORDS = log2(hiw.NP.corr$NO_WORDS)
hiw.NP.corr$NO_SYLLS = log2(hiw.NP.corr$NO_SYLLS)
hiw.NP.corr$NO_PHON_WORDS = log2(hiw.NP.corr$NO_PHON_WORDS)
hiw.NP.corr$NO_FUNC_WORDS = log2(hiw.NP.corr$NO_FUNC_WORDS + 1)
hiw.NP.corr$NO_FUNC_WORDS_MONO = log2(hiw.NP.corr$NO_FUNC_WORDS_MONO + 1)
hiw.NP.corr$NO_FUNC_WORDS_MULTI = log2(hiw.NP.corr$NO_FUNC_WORDS_MULTI + 1)
cor(hiw.NP.corr)
write.csv(cor(hiw.NP.corr), "corr.csv")
# Check again on the subset that has parses
hiw.NP.corr2 = subset(hiw.NP, !is.na(SUBJ_DEPTH), select = c(NEWTWO, NO_WORDS, NO_SYLLS, NO_PHON_WORDS, NO_FUNC_WORDS, NO_FUNC_WORDS_MONO, NO_FUNC_WORDS_MULTI, SPEAKING_RATE, PFORWARD, PBACKWARD, PHOST, PAFTER))
cor(hiw.NP.corr2, method="spearman")

# A base model
hiw.NP.NO_WORDS.lme = glmer(NEWTWO ~ log2(NO_WORDS) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + PFORWARD + PBACKWARD + PHOST + PAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_WORDS.lme)

# Model without subject length but with probability/frequency
hiw.NP.INFO.lme = glmer(NEWTWO ~ SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + PFORWARD + PBACKWARD + PHOST + PAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.INFO.lme)

# Different possible NO_WORDS collinear predictors
hiw.NP.NO_SYLLS.lme = glmer(NEWTWO ~ log2(NO_SYLLS) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_SYLLS.lme)
hiw.NP.NO_PHON_WORDS.lme = glmer(NEWTWO ~ log2(NO_PHON_WORDS) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_PHON_WORDS.lme)
hiw.NP.NO_FUNC_WORDS.lme = glmer(NEWTWO ~ log2(NO_FUNC_WORDS + 1) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_FUNC_WORDS.lme)
hiw.NP.NO_FUNC_WORDS_MONO.lme = glmer(NEWTWO ~ log2(NO_FUNC_WORDS_MONO + 1) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_FUNC_WORDS_MONO.lme)

# Nothing goes wrong with subject duration, but we aren't dealing with that.
#hiw.NP.SUBJ_DUR.lme = glmer(NEWTWO ~ SUBJ_DUR + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
#summary(hiw.NP.SUBJ_DUR.lme)

# Residualization off of N_WORDS
NO_SYLLS_rwords = lm(log2(NO_SYLLS) ~ log2(NO_WORDS), hiw.NP)$resid
NO_PHON_WORDS_rwords = lm(log2(NO_PHON_WORDS) ~ log2(NO_WORDS), hiw.NP)$resid
NO_FUNC_WORDS_rwords = lm(log2(NO_FUNC_WORDS + 1) ~ log2(NO_WORDS), hiw.NP)$resid

# N_WORDS plus each of the resids (or func words multi which didn't need to be residualized)
hiw.NP.NO_WORDS_SYLLS.lme = glmer(NEWTWO ~ log2(NO_WORDS) + NO_SYLLS_rwords + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_WORDS_SYLLS.lme)
hiw.NP.NO_WORDS_PHON.lme = glmer(NEWTWO ~ log2(NO_WORDS) + NO_PHON_WORDS_rwords + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_WORDS_PHON.lme)
hiw.NP.NO_WORDS_FUNC.lme = glmer(NEWTWO ~ log2(NO_WORDS) + NO_FUNC_WORDS_rwords + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_WORDS_FUNC.lme)
hiw.NP.NO_WORDS_FUNC_MULTI.lme = glmer(NEWTWO ~ log2(NO_WORDS) + log2(NO_FUNC_WORDS_MULTI + 1) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_WORDS_FUNC_MULTI.lme)

# Residualization of N_WORDS against each
# Sylls
NO_WORDS_rsylls = lm(log2(NO_WORDS) ~ log2(NO_SYLLS), hiw.NP)$resid
hiw.NP.NO_SYLLS_WORDS.lme = glmer(NEWTWO ~ log2(NO_SYLLS) + NO_WORDS_rsylls + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_SYLLS_WORDS.lme)
# Phon words
NO_WORDS_rphon = lm(log2(NO_WORDS) ~ log2(NO_PHON_WORDS), hiw.NP)$resid
hiw.NP.NO_PHON_WORDS.lme = glmer(NEWTWO ~ log2(NO_PHON_WORDS) + NO_WORDS_rphon + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_PHON_WORDS.lme)
# Func words
NO_WORDS_rfunc = lm(log2(NO_WORDS) ~ log2(NO_FUNC_WORDS + 1), hiw.NP)$resid
hiw.NP.NO_FUNC_WORDS.lme = glmer(NEWTWO ~ log2(NO_FUNC_WORDS + 1) + NO_WORDS_rfunc + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_FUNC_WORDS.lme)
hiw.NP.NO_FUNC_WORDS.lme = glmer(NEWTWO ~ log2(NO_FUNC_WORDS + 1) + log2(NO_FUNC_WORDS_MULTI + 1) + NO_WORDS_rfunc + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_FUNC_WORDS.lme)

# Func and phon together
NO_WORDS_FUNC_rphon = lm(log2(NO_FUNC_WORDS + 1) ~ log2(NO_PHON_WORDS), hiw.NP)$resid
hiw.NP.NO_PHON_FUNC.lme = glmer(NEWTWO ~ log2(NO_PHON_WORDS) + NO_WORDS_FUNC_rphon + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_PHON_FUNC.lme)
# Reverse order to see coefficient
NO_PHON_WORD_rfunc = lm(log2(NO_PHON_WORDS) ~ log2(NO_FUNC_WORDS + 1), hiw.NP)$resid
hiw.NP.NO_FUNC_PHON.lme = glmer(NEWTWO ~ log2(NO_FUNC_WORDS + 1) + NO_PHON_WORD_rfunc + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_FUNC_PHON.lme)

# WIN
hiw.NP.NO_PHON_FUNC_WORDS.lme = glmer(NEWTWO ~ log2(NO_PHON_WORDS) + NO_WORDS_FUNC_rphon + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_PHON_FUNC_WORDS.lme)

# Word model for comparison
hiw.NP.NO_WORDS_BASIC.lme = glmer(NEWTWO ~ log2(NO_WORDS) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_WORDS_BASIC.lme)
anova(hiw.NP.NO_WORDS_BASIC.lme, hiw.NP.NO_PHON_FUNC_WORDS.lme)

# Subject depth (AKA height) is n.s. Nothing to see here.
hiw.NP.parsed = subset(hiw.NP, !is.na(SUBJ_DEPTH))
hiw.NP.DEPTH.lme = glmer(NEWTWO ~ SUBJ_DEPTH + SPEAKING_RATE + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.parsed, family = 'binomial')
summary(hiw.NP.DEPTH.lme)
DEPTH_rphon = lm(SUBJ_DEPTH ~ log2(NO_PHON_WORDS), hiw.NP.parsed)$resid
hiw.NP.DEPTH_NO_PHON_WORDS.lme = glmer(NEWTWO ~ log2(NO_PHON_WORDS) + DEPTH_rphon + SPEAKING_RATE + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.parsed, family = 'binomial')
summary(hiw.NP.DEPTH_NO_PHON_WORDS.lme)
# Throw in some syllables, for a challenge, still significant
NO_SYLLS_rphon = lm(log2(NO_SYLLS) ~ log2(NO_PHON_WORDS), hiw.NP)$resid
hiw.NP.NO_PHON_SYLL.lme = glmer(NEWTWO ~ log2(NO_PHON_WORDS) + NO_SYLLS_rphon + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_PHON_SYLL.lme)
# TODO: Try with number of right brackets

# Pile on orth. words for extra challenge
NO_WORDS_rphonsylls = lm(log2(NO_WORDS) ~ log2(NO_PHON_WORDS) + NO_SYLLS_rphon, hiw.NP)$resid
hiw.NP.NO_PHON_SYLL_WORDS.lme = glmer(NEWTWO ~ log2(NO_PHON_WORDS) + NO_SYLLS_rphon + NO_WORDS_rphonsylls + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(hiw.NP.NO_PHON_SYLL_WORDS.lme)

# Way to jitter points off axis
hiw.NP$JOE = 1.1
hiw.NP[hiw.NP$NEWTWO == 0,]$JOE = -.1

# PLOT PARTY
#orthographic words
ortho = ggplot(hiw.NP, aes(log2(NO_WORDS), NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "number of orthographic words") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "orthographic words")
#pros words
pros = ggplot(hiw.NP, aes(NO_PHON_WORDS, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(limits = c(1,10), name = "number of prosodic words") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "prosodic words")
#sylls
sylls = ggplot(hiw.NP, aes(NO_SYLLS, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "number of syllables") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "syllables")
#speaking rate (n.s.)
ggplot(hiw.NP, aes(SPEAKING_RATE, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "speaking rate") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "speaking rate (words/sec)")
#subject depth
depth = ggplot(hiw.NP, aes(SUBJ_DEPTH, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "height of parse") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "height")
pdf("gurt_predictor_plots.pdf", width = 9, height = 6.5)
arrange(ortho, pros, sylls, depth, ncol = 2)
dev.off()
# Also check out function words
func = ggplot(hiw.NP, aes(NO_FUNC_WORDS, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "number of function words") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "function words")
arrange(pros, func)

phost = ggplot(hiw.NP, aes(PHOST, NEWTWO)) + geom_point(aes(y = JOE), position=position_jitter(width = 0.2, height = .1), alpha = .5) + stat_smooth(method="glm", family ="binomial", fullrange=TRUE, colour = "black") + scale_x_continuous(name = "log2 preceding word frequency") + scale_y_continuous(breaks = (0:5)/5, name = "") + theme_bw(base_size=14) + opts(legend.position = "none", title = "frequency")
