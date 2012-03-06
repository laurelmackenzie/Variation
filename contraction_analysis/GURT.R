library(lme4)
library(ggplot2)

# Load up all the data and preprocessing, etc.
source("contraction.R")
# Reduce to the subset we're looking at.
hiw_NP = subset(hiw, NP == "NP")
# NEWTWO appears to be continuous and not categorial. Recoding here.
hiw_NP$NEWTWO = as.logical(hiw_NP$NEWTWO)
hiw_NP$WORD = recontrast(hiw_NP$WORD) #change from treatment to sum contrasts because no one auxiliary is default
# We use decade of birth as the numeric levels of the factor so it's continuous. Coding as the actual years
# or, strangely, as the continuous decades of birth at the original number (e.g., 1950) causes false convergence.
hiw_NP$DOB = as.numeric(as.factor(hiw_NP$DOB))
# Add in number of functional elements, which is orthographic words - phonological words
hiw_NP$NO_FUNC_WORDS = hiw_NP$NO_WORDS - hiw_NP$NO_PHON_WORDS
summary(hiw_NP)

# Get the correlations
hiw_NP_corr = subset(hiw_NP, !is.na(EDUC_STEP), select = c(NO_WORDS, NO_SYLLS, NO_PHON_WORDS, NO_FUNC_WORDS, SPEAKING_RATE, DOB, EDUC_STEP))
cor(hiw_NP_corr)
summary(hiw_NP$EDUC_STEP)

# A base model
hiw_NP_NO_WORDS.lme = lmer(NEWTWO ~ NO_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_WORDS.lme)

# Different possible NO_WORDS colinear predictors
hiw_NP_NO_SYLLS.lme = lmer(NEWTWO ~ NO_SYLLS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_SYLLS.lme)
hiw_NP_NO_PHON_WORDS.lme = lmer(NEWTWO ~ NO_PHON_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_WORDS.lme)
hiw_NP_NO_FUNC_WORDS.lme = lmer(NEWTWO ~ NO_FUNC_WORDS + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_FUNC_WORDS.lme)

# Something goes wrong with subject duration
#hiw_NP_SUBJ_DUR.lme = lmer(NEWTWO ~ SUBJ_DUR + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
#summary(hiw_NP_SUBJ_DUR.lme)

# Residualization off of N_WORDS
NO_SYLLS_rwords = lm(NO_SYLLS ~ NO_WORDS, hiw_NP)$resid
NO_PHON_WORDS_rwords = lm(NO_PHON_WORDS ~ NO_WORDS, hiw_NP)$resid
NO_FUNC_WORDS_rwords = lm(NO_FUNC_WORDS ~ NO_WORDS, hiw_NP)$resid

# N_WORDS plus each of the resids
hiw_NP_NO_WORDS_SYLLS.lme = lmer(NEWTWO ~ NO_WORDS + NO_SYLLS_rwords + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_WORDS_SYLLS.lme)
hiw_NP_NO_WORDS_PHON.lme = lmer(NEWTWO ~ NO_WORDS + NO_PHON_WORDS_rwords + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_WORDS_PHON.lme)
hiw_NP_NO_WORDS_FUNC.lme = lmer(NEWTWO ~ NO_WORDS + NO_FUNC_WORDS_rwords + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_WORDS_FUNC.lme)

# Residualization of N_WORDS against each
# Sylls
NO_WORDS_rsylls = lm(NO_WORDS ~ NO_SYLLS, hiw_NP)$resid
hiw_NP_NO_SYLLS_WORDS.lme = lmer(NEWTWO ~ NO_SYLLS + NO_WORDS_rsylls + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_SYLLS_WORDS.lme)
# Phon words
NO_WORDS_rphon = lm(NO_WORDS ~ NO_PHON_WORDS, hiw_NP)$resid
hiw_NP_NO_PHON_WORDS.lme = lmer(NEWTWO ~ NO_PHON_WORDS + NO_WORDS_rphon + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_WORDS.lme)
# Func words
NO_WORDS_rfunc = lm(NO_WORDS ~ NO_FUNC_WORDS, hiw_NP)$resid
hiw_NP_NO_FUNC_WORDS.lme = lmer(NEWTWO ~ NO_FUNC_WORDS + NO_WORDS_rfunc + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_FUNC_WORDS.lme)

# Func and phone together
NO_WORDS_FUNC_rphon = lm(NO_FUNC_WORDS ~ NO_PHON_WORDS, hiw_NP)$resid
hiw_NP_NO_PHON_FUNC.lme = lmer(NEWTWO ~ NO_PHON_WORDS + NO_WORDS_FUNC_rphon + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | PREC_WORD) + (1 | FOLL_WORD) + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_FUNC.lme)
# Right now, won't converge
NO_WORDS_rphonfunc = lm(NO_WORDS ~ NO_PHON_WORDS + NO_WORDS_FUNC_rphon, hiw_NP)$resid
hiw_NP_NO_PHON_FUNC_WORDS.lme = lmer(NEWTWO ~ NO_PHON_WORDS + NO_WORDS_FUNC_rphon + NO_WORDS_rphonfunc + SPEAKING_RATE + DOB + CORPUS + SEX + EDUC_STEP + WORD + CV + PREC_STRESS + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, family = 'binomial')
summary(hiw_NP_NO_PHON_FUNC_WORDS.lme)

