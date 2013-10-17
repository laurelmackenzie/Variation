library(lme4.0)
library(ggplot2)

# Load up all the data and preprocessing, etc.
source("contraction.R")

# Reduce to the subset we're looking at.
hiw.NP = subset(hiw, NP == "NP")
# Code response as logical in a separate column
hiw.NP$NEWTWO.log = as.factor(hiw.NP$NEWTWO)
# Change from treatment to sum contrasts because no one auxiliary is default
hiw.NP$WORD = recontrast(hiw.NP$WORD) 
# Center DOB
hiw.NP$DOB = scale(hiw.NP$DOB, scale = FALSE)
# Add the general code of number of function words as the sum of monosyllabic and multisyllabic
hiw.NP$NO_FUNC_WORDS = hiw.NP$NO_FUNC_WORDS_MONO + hiw.NP$NO_FUNC_WORDS_MULTI
# Log duration
hiw.NP$SUBJ_DUR = log2(hiw.NP$SUBJ_DUR)
# NA out unreliable indicators
hiw.NP$PFORWARD[hiw.NP$PFORWARD_COUNT < 5 | hiw.NP$PFORWARD == 0.0] <- NA
hiw.NP$PBACKWARD[hiw.NP$PBACKWARD_COUNT < 5 | hiw.NP$PBACKWARD == 0.0] <- NA
hiw.NP$PHOST[hiw.NP$PHOST == 0.0] <- NA
hiw.NP$PAFTER[hiw.NP$PAFTER == 0.0] <- NA
# Log transform probabilities/frequencies
hiw.NP$PFORWARD = log2(hiw.NP$PFORWARD)
hiw.NP$PBACKWARD = log2(hiw.NP$PBACKWARD)
hiw.NP$PHOST = log2(hiw.NP$PHOST)
hiw.NP$PAFTER = log2(hiw.NP$PAFTER)
hiw.NP$FREQHOST = log2(hiw.NP$FREQHOST)
# Subset again to remove any NAs from exclusions
hiw.NP <- subset(hiw.NP, !is.na(PFORWARD) & !is.na(PBACKWARD) & !is.na(PHOST) & !is.na(PAFTER) & !is.na(FREQHOST))
summary(hiw.NP)

# Make subset with duration
hiw.NP.dur <- subset(hiw.NP, !is.na(SUBJ_DUR))
# Residualize, as the correlation is ~ .8
hiw.NP.dur$rSUBJ_DUR = resid(lm(SUBJ_DUR ~ NO_WORDS, hiw.NP.dur))

# Look at the strength of individual predictors
# We should use number of words, not its log
nwords.m <- glm(as.logical(NEWTWO) ~ NO_WORDS, hiw.NP, family = 'binomial')
summary(nwords.m)
nwordslog.m <- glm(NEWTWO ~ log2(NO_WORDS), hiw.NP, family = 'binomial')
summary(nwordslog.m)

# Compare global versus local frequency
# Global is *much* weaker, although both are nonsig in this analysis
freqhost.m <- glm(NEWTWO ~ NO_WORDS + FREQHOST, hiw.NP, family = 'binomial')
summary(freqhost.m)
phost.m <- glm(NEWTWO ~ NO_WORDS + PHOST, hiw.NP, family = 'binomial')
summary(phost.m)

# Look at correlations between predictors
hiw.NP.corr = subset(hiw.NP, !is.na(EDUC_STEP), select = c(NEWTWO, NO_WORDS, NO_SYLLS, NO_PHON_WORDS, NO_FUNC_WORDS, NO_FUNC_WORDS_MONO, NO_FUNC_WORDS_MULTI, SPEAKING_RATE, PFORWARD, PBACKWARD, PHOST, PAFTER, FREQHOST))
# Replace some predictors with their log version, adding one if needed to prevent zeros
hiw.NP.corr$NO_WORDS = log2(hiw.NP.corr$NO_WORDS)
hiw.NP.corr$NO_SYLLS = log2(hiw.NP.corr$NO_SYLLS)
hiw.NP.corr$NO_PHON_WORDS = log2(hiw.NP.corr$NO_PHON_WORDS)
hiw.NP.corr$NO_FUNC_WORDS = log2(hiw.NP.corr$NO_FUNC_WORDS + 1)
hiw.NP.corr$NO_FUNC_WORDS_MONO = log2(hiw.NP.corr$NO_FUNC_WORDS_MONO + 1)
hiw.NP.corr$NO_FUNC_WORDS_MULTI = log2(hiw.NP.corr$NO_FUNC_WORDS_MULTI + 1)
cor(hiw.NP.corr, use = "complete.obs")
write.csv(cor(hiw.NP.corr), "corr.csv")
# Check again on the subset that has parses
hiw.NP.corr2 = subset(hiw.NP, !is.na(SUBJ_DEPTH), select = c(NEWTWO, NO_WORDS, NO_SYLLS, NO_PHON_WORDS, NO_FUNC_WORDS, NO_FUNC_WORDS_MONO, NO_FUNC_WORDS_MULTI, SPEAKING_RATE, PFORWARD, PBACKWARD, PHOST, PAFTER, FREQHOST))
cor(hiw.NP.corr2, method="spearman")

# A base model
nwords.base.m = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nwords.base.m)

# Check with subject duration, note that we have to remove CORPUS as only one corpus has durations.
nwords.dur.base.m = glmer(NEWTWO ~ NO_WORDS + rSUBJ_DUR + SPEAKING_RATE + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial', na.action = na.omit)
# Duration adds nothing on top of number of words!
summary(nwords.dur.base.m)

# Add in a random slope for NO_WORDS, see if it matters
nwords.base.sm = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (NO_WORDS | SPEAKER), hiw.NP, family = 'binomial', na.action = na.omit)
summary(nwords.base.sm)
# It doesn't help here
anova(nwords.base.m, nwords.base.sm)

# Check again about whether we should log NO_WORDS. Again we find that we should not.
lnwords.base.sm = glmer(NEWTWO ~ log2(NO_WORDS) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (log2(NO_WORDS) | SPEAKER), hiw.NP, family = 'binomial', na.action = na.omit)
summary(lnwords.base.sm)

# Which is more useful: global or local frequency?
nwords.globfreq.m = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial', na.action = na.omit)
nwords.locfreq.m = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + PHOST + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial', na.action = na.omit)
# Global frequency is actually significant (unlike local) but barely improves fit.
summary(nwords.globfreq.m)
summary(nwords.locfreq.m)
anova(nwords.globfreq.m, nwords.locfreq.m)

# Look at correlations among predictability predictors
cor(subset(hiw.NP, select = c(NO_WORDS, FREQHOST, PFORWARD, PBACKWARD, PHOST, PAFTER)))
# Residualize PHOST (local frequency) and FREQHOST (global frequency). Since FREQHOST performs better, we give it primacy.
# It's best to not use this predictor at all; it's basically impossible to make sense of.
hiw.NP$rPHOST = resid(lm(PHOST ~ FREQHOST, hiw.NP))
# To be safe, we residualize all predictability measures off the frequency of the host. It isn't strictly necessary to do it to all of them, but it makes things more consistent.
hiw.NP$rPFORWARD = resid(lm(PFORWARD ~ FREQHOST, hiw.NP))
hiw.NP$rPBACKWARD = resid(lm(PBACKWARD ~ FREQHOST, hiw.NP))
hiw.NP$rPAFTER = resid(lm(PAFTER ~ FREQHOST, hiw.NP))
# Sanity check
cor(subset(hiw.NP, select = c(NO_WORDS, FREQHOST, rPFORWARD, rPBACKWARD, rPHOST, rPAFTER)))

# Plots of predictors
cdplot(hiw.NP$NO_WORDS, hiw.NP$NEWTWO.log)
cdplot(hiw.NP$FREQHOST, hiw.NP$NEWTWO.log)
cdplot(hiw.NP$PHOST, hiw.NP$NEWTWO.log)
cdplot(hiw.NP$PFORWARD, hiw.NP$NEWTWO.log)
cdplot(hiw.NP$rPFORWARD, hiw.NP$NEWTWO.log)
cdplot(hiw.NP$PBACKWARD, hiw.NP$NEWTWO.log)
cdplot(hiw.NP$rPBACKWARD, hiw.NP$NEWTWO.log)
cdplot(hiw.NP$PAFTER, hiw.NP$NEWTWO.log)
cdplot(hiw.NP$rPAFTER, hiw.NP$NEWTWO.log)
# Plots on data with duration
cdplot(hiw.NP.dur$SUBJ_DUR, hiw.NP.dur$NEWTWO.log)
cdplot(hiw.NP.dur$rSUBJ_DUR, hiw.NP.dur$NEWTWO.log)

# Add in predicability
nwords.pred.m = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + rPAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial', na.action = na.omit)
summary(nwords.pred.m)

# Fit with increasing numbers of random slopes
nwords.pred.sm1 = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + rPAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (NO_WORDS | SPEAKER), hiw.NP, family = 'binomial')
nwords.pred.sm2 = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + rPAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (NO_WORDS + FREQHOST | SPEAKER), hiw.NP, family = 'binomial')
# This cannot converge with current lme4.0
nwords.pred.sm3 = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + rPAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (NO_WORDS + FREQHOST + rPFORWARD + rPBACKWARD + rPAFTER | SPEAKER), hiw.NP, family = 'binomial')
# The random slopes are not useful, so we do not need to purse them further
anova(nwords.pred.m, nwords.pred.sm1, nwords.pred.sm2)

# Model without subject length but with probability/frequency
pred.m = glmer(NEWTWO ~ SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + rPAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(pred.m)
anova(pred.m, nwords.pred.m)

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

# Density plots
hiw.NP$NEWTWO_FACTOR <- as.factor(hiw.NP$NEWTWO)
density.length <- ggplot(hiw.NP, aes(NO_WORDS)) + geom_density(aes(fill = NEWTWO_FACTOR), alpha = 0.75, position = "fill", adjust = 3) + scale_fill_grey(name = "Contraction") + xlab("Subject length (orthographic words)") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom")
density.pforward <- ggplot(hiw.NP, aes(PFORWARD)) + geom_density(aes(fill = NEWTWO_FACTOR), alpha = 0.75, position = "fill", adjust = 3) + scale_fill_grey(name = "Contraction") + xlab("Forward probability of auxiliary") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom")
