library(lme4.0)
library(ggplot2)

# Load up all the data and preprocessing, etc.
source("hiw.R")

# Show what we're working with
summary(hiw.NP)

# Look at the strength of individual predictors
# We should use number of words, not its log
nwords.m <- glm(NEWTWO ~ NO_WORDS, hiw.NP, family = 'binomial')
summary(nwords.m)
nwordslog.m <- glm(NEWTWO ~ log2(NO_WORDS), hiw.NP, family = 'binomial')
summary(nwordslog.m)

# A base model
nwords.base.m = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nwords.base.m)

# Compare global versus local frequency
# Make a subset where there are no FREQLOWHOST NAs
hiw.NP.freqlow <- subset(hiw.NP, !is.na(FREQLOWHOST))
# Compare global, global with only lowercase tokens counted, and local
freqhost.m <- glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.freqlow, family = 'binomial')
# Marginal significance for global freq
summary(freqhost.m)
freqlowhost.m <- glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQLOWHOST + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.freqlow, family = 'binomial')
summary(freqlowhost.m)
# Lowercase is nowhere near significant
anova(freqhost.m, freqlowhost.m)
phost.m <- glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + PHOST + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.freqlow, family = 'binomial')
# Local freq is nowhere near significant
summary(phost.m)
anova(freqhost.m, phost.m)

# Check with subject duration, note that we have to remove CORPUS as only one corpus has durations.
nwords.dur.base.m = glmer(NEWTWO ~ NO_WORDS + rSUBJ_DUR + SPEAKING_RATE + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.dur, family = 'binomial')
# Duration does add on top of number of words
summary(nwords.dur.base.m)
# Do another base model to compare against
nwords.nodur.base.m = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.dur, family = 'binomial')
summary(nwords.nodur.base.m)
# Yes, duration is useful
anova(nwords.nodur.base.m, nwords.dur.base.m)

# Is duration still useful if there's predictive info?
nwords.nodur.pred.m = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.dur, family = 'binomial')
summary(nwords.nodur.pred.m)
nwords.dur.pred.m = glmer(NEWTWO ~ NO_WORDS + rSUBJ_DUR + SPEAKING_RATE + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.dur, family = 'binomial')
# Important: note that the predictive information is much weaker in this model,
# but that's because there's less data. These effects are weak.
summary(nwords.dur.pred.m)
# Yes, duration helps the model
anova(nwords.nodur.pred.m, nwords.dur.pred.m)

# Check whether N_WORDS still adds on top of duration
dur.nwords.pred.m = glmer(NEWTWO ~ rNO_WORDS + SUBJ_DUR + SPEAKING_RATE + DOB + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.dur, family = 'binomial')
# Hmm, this is marginal
summary(dur.nwords.pred.m)
# Not nest

# Back to the basics
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

# Plots of predictors

cdplot(hiw.NP$NO_WORDS, hiw.NP$NEWTWO.factor)
ggplot(hiw.NP, aes(NO_WORDS)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.75, position = "fill") + scale_fill_grey(name = "Form") + theme_bw()

cdplot(hiw.NP$FREQHOST, hiw.NP$NEWTWO.factor)
cdplot(hiw.NP$PHOST, hiw.NP$NEWTWO.factor)
cdplot(hiw.NP$PFORWARD, hiw.NP$NEWTWO.factor)
cdplot(hiw.NP$rPFORWARD, hiw.NP$NEWTWO.factor)
cdplot(hiw.NP$PBACKWARD, hiw.NP$NEWTWO.factor)
cdplot(hiw.NP$rPBACKWARD, hiw.NP$NEWTWO.factor)
cdplot(hiw.NP$PAFTER, hiw.NP$NEWTWO.factor)
cdplot(hiw.NP$FREQAFTER, hiw.NP$NEWTWO.factor)
# Plots on data with duration
cdplot(hiw.NP.dur$SUBJ_DUR, hiw.NP.dur$NEWTWO.factor)
cdplot(hiw.NP.dur$rSUBJ_DUR, hiw.NP.dur$NEWTWO.factor)

# Add in predicability
nwords.pred.m = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial', na.action = na.omit)
summary(nwords.pred.m)

# Fit with increasing numbers of random slopes
nwords.pred.sm1 = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (NO_WORDS | SPEAKER), hiw.NP, family = 'binomial')
nwords.pred.sm2 = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (NO_WORDS + FREQHOST | SPEAKER), hiw.NP, family = 'binomial')
# This cannot converge with current lme4.0
# nwords.pred.sm3 = glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (NO_WORDS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER | SPEAKER), hiw.NP, family = 'binomial')
# The random slopes are not useful, so we do not need to purse them further
anova(nwords.pred.m, nwords.pred.sm1, nwords.pred.sm2)

# Model without subject length but with probability/frequency
pred.m = glmer(NEWTWO ~ SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(pred.m)
anova(pred.m, nwords.pred.m)

# Different possible NO_WORDS collinear predictors
# Look at correlations between predictors
hiw.NP.corr = subset(hiw.NP, !is.na(EDUC_STEP), select = c(NEWTWO, NO_WORDS, NO_SYLLS, NO_PHON_WORDS, NO_NOT_FUNC_MONO_WORDS, NO_FUNC_WORDS, NO_FUNC_WORDS_MONO, NO_FUNC_WORDS_MULTI, SPEAKING_RATE, PFORWARD, PBACKWARD, PHOST, PAFTER, FREQHOST))
cor(hiw.NP.corr, use = "complete.obs")
write.csv(cor(hiw.NP.corr), "corr.csv")
# Check again on the subset that has parses
hiw.NP.corr2 = subset(hiw.NP, !is.na(SUBJ_DEPTH), select = c(NEWTWO, NO_WORDS, NO_SYLLS, NO_PHON_WORDS, NO_FUNC_WORDS, NO_FUNC_WORDS_MONO, NO_FUNC_WORDS_MULTI, SPEAKING_RATE, PFORWARD, PBACKWARD, PHOST, PAFTER, FREQHOST))
cor(hiw.NP.corr2, method="spearman")

# Syllables
nsyll.pred.m = glmer(NEWTWO ~ NO_SYLLS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
lnsyll.pred.m = glmer(NEWTWO ~ log2(NO_SYLLS) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nsyll.pred.m)
summary(lnsyll.pred.m)
# non-log is better
anova(nsyll.pred.m, lnsyll.pred.m)

# Number of phon words
nphon.pred.m = glmer(NEWTWO ~ NO_PHON_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
lnphon.pred.m = glmer(NEWTWO ~ log2(NO_PHON_WORDS) + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nphon.pred.m)
summary(lnphon.pred.m)
# non-log is better
anova(nphon.pred.m, lnphon.pred.m)

# Based on the fact that logs are never helping for discrete length measures,
# I stop checking it from here.

# Function words
funcmono.pred.m = glmer(NEWTWO ~ NO_FUNC_WORDS_MONO + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(funcmono.pred.m)

func.pred.m = glmer(NEWTWO ~ NO_FUNC_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(func.pred.m)

# "Heavy" words: everything but monosyllabic function words
nonfuncmono.pred.m = glmer(NEWTWO ~ NO_NOT_FUNC_MONO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nonfuncmono.pred.m)

# Try both func/func_mono and phon
# Phon + func
nphon.nfunc.pred.m = glmer(NEWTWO ~ NO_PHON_WORDS + NO_FUNC_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nphon.nfunc.pred.m)

# Phon + func_mono
nphon.nfuncmono.pred.m = glmer(NEWTWO ~ NO_PHON_WORDS + NO_FUNC_WORDS_MONO + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nphon.nfuncmono.pred.m)

# Heavy + func_mono
nonfuncmono.funcmono.pred.m = glmer(NEWTWO ~ NO_NOT_FUNC_MONO_WORDS + NO_FUNC_WORDS_MONO + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nonfuncmono.funcmono.pred.m)

# Compare models
# These are not nested model comparisons, just checking out the differences
anova(nwords.pred.m, nsyll.pred.m)
anova(nwords.pred.m, nphon.pred.m)
anova(nwords.pred.m, func.pred.m)
anova(nwords.pred.m, funcmono.pred.m)

# Decomposition of no_words, this is a nested comparison. Does not imrpove.
anova(nwords.pred.m, nonfuncmono.pred.m)

# Is adding func on top of number of phon words useful? Yes.
anova(nphon.pred.m, nphon.nfunc.pred.m)
# But is it better than just number of words?
# This is nested because of the way the counts of func/phono work:
# n_words = n_phon + n_func
anova(nwords.pred.m, nphon.nfunc.pred.m)
# This isn't properly nested, but worth checking for kicks
anova(nwords.pred.m, nphon.nfuncmono.pred.m)
# Just to compare whether mono was useful.
anova(nphon.nfunc.pred.m, nphon.nfuncmono.pred.m)

# Heavies and non-heavies
# Yes, this is useful!
anova(nonfuncmono.pred.m, nonfuncmono.funcmono.pred.m)
# Was it better than number of words?
anova(nwords.pred.m, nonfuncmono.funcmono.pred.m)

# Residualize predictors and add to NO_WORDS
# *********************************************

# Residualization off of N_WORDS
rNO_SYLLS = resid(lm(log2(NO_SYLLS) ~ log2(NO_WORDS), hiw.NP))
rNO_PHON_WORDS = resid(lm(log2(NO_PHON_WORDS) ~ log2(NO_WORDS), hiw.NP))
rNO_FUNC_WORDS = resid(lm(log2(NO_FUNC_WORDS + 1) ~ log2(NO_WORDS), hiw.NP))

# N_WORDS plus each of the resids
nwords.nsylls.pred.m = glmer(NEWTWO ~ NO_WORDS + rNO_SYLLS+ SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nwords.nsylls.pred.m)

nwords.nphon.pred.m = glmer(NEWTWO ~ NO_WORDS + rNO_PHON_WORDS+ SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nwords.nphon.pred.m)

nwords.nfunc.pred.m = glmer(NEWTWO ~ NO_WORDS + rNO_FUNC_WORDS+ SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + FREQHOST + rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial')
summary(nwords.nfunc.pred.m)

# Do adding these to n_words help? Nope.
anova(nwords.pred.m, nwords.nsylls.pred.m)
anova(nwords.pred.m, nwords.nphon.pred.m)
anova(nwords.pred.m, nwords.nfunc.pred.m)

# Syntax
(density.depth <- ggplot(hiw.NP, aes(SUBJ_DEPTH)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("Subject parse height") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("depth_density.pdf", width = 6, height = 4)

# TODO: Try with number of right brackets

# Way to jitter points off axis
hiw.NP$JOE = 1.1
hiw.NP[hiw.NP$NEWTWO == 0,]$JOE = -.1

# Density plots
# It would make sense to make this a function, but the way ggplot binds
# the column names makes this difficult. So this is ridiculous copy/paste
(density.length <- ggplot(hiw.NP, aes(NO_WORDS)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2, kernel = "gaussian") + scale_fill_grey(name = "Contraction") + xlab("Subject length (orthographic words)") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("nword_density.pdf", width = 6, height = 4)

(density.pforward <- ggplot(hiw.NP, aes(PFORWARD)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("Forward probability of auxiliary (log base 2)") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("pforward_density.pdf", width = 6, height = 4)

(density.pbackward <- ggplot(hiw.NP, aes(PBACKWARD)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("Backward probability of auxiliary (log base 2)") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("pbackward_density.pdf", width = 6, height = 4)

(density.freqhost <- ggplot(hiw.NP, aes(FREQHOST)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("SUBTLEX frequency of host (log base 2)") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("freqhost_density.pdf", width = 6, height = 4)

(density.freqafter <- ggplot(hiw.NP, aes(FREQAFTER)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("SUBTLEX frequency of following word (log base 2)") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("freqafter_density.pdf", width = 6, height = 4)

(density.nsyll <- ggplot(hiw.NP, aes(NO_SYLLS)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("Number of syllables") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("nsyll_density.pdf", width = 6, height = 4)

(density.nphon <- ggplot(hiw.NP, aes(NO_PHON_WORDS)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("Number of prosodic words") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("nphon_density.pdf", width = 6, height = 4)

(density.nfunc <- ggplot(hiw.NP, aes(NO_FUNC_WORDS)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("Number of function words") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("nfunc_density.pdf", width = 6, height = 4)

# Predictor impact analysis
# Odds ratio of NO_WORDS, useful range of 1-7
1 / (exp(fixef(nwords.pred.m)['NO_WORDS'])^6)
# From "Communism is" to "work is", taking inverse because we want
# the opposite direction of the predictor (high to low, not low to high)
exp(fixef(nwords.pred.m)['rPFORWARD'])^log2(0.363636363636364/0.007878447)

# Interaction between head final and not
hiw.NP$headfinal <- hiw.NP$WORDS_BTWN_HEAD_AUX == 0
summary(hiw.NP$headfinal)
nwords.pred.headfinal.m <- glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + headfinal:FREQHOST + headfinal:rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP, family = 'binomial', na.action = na.omit)
summary(nwords.pred.headfinal.m)

# Look at subsets based on position of head
hiw.NP.headfinal <- subset(hiw.NP, WORDS_BTWN_HEAD_AUX == 0)
hiw.NP.headnonfinal <- subset(hiw.NP, WORDS_BTWN_HEAD_AUX > 0)

# Exploratory plots for each
(density.headend.pforward <- ggplot(hiw.NP.headfinal, aes(PFORWARD)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("Forward probability of auxiliary (log base 2), head-final subject") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("pforward_headend_density.pdf", width = 6, height = 4)

(density.headearly.pforward <- ggplot(hiw.NP.headnonfinal, aes(PFORWARD)) + geom_density(aes(fill = NEWTWO.factor), alpha = 0.5, position = "fill", adjust = 2) + scale_fill_grey(name = "Contraction") + xlab("Forward probability of auxiliary (log base 2), non-head-final subject") + ylab("Probability of contraction") + theme_bw() + theme(legend.position = "bottom"))
ggsave("pforward_headearly_density.pdf", width = 6, height = 4)

# Check whether it's a power issue by matching number of items
hiw.NP.headbalanced <- rbind(hiw.NP.headfinal[sample(nrow(hiw.NP.headfinal), nrow(hiw.NP.headnonfinal)), ], hiw.NP.headnonfinal)
# Verify balance
summary(hiw.NP.headbalanced$headfinal)
nwords.pred.headfinal.headbalanced.m <- glmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + headfinal:FREQHOST + headfinal:rPFORWARD + rPBACKWARD + FREQAFTER + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw.NP.headbalanced, family = 'binomial', na.action = na.omit)
summary(nwords.pred.headfinal.headbalanced.m)
