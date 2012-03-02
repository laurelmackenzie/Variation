library(lme4)

# Load up all the data and preprocessing, etc.
source("contraction.R")
# Reduce to the subset we're looking at.
hiw_NP <- subset(hiw, NP == "NP")
# NEWTWO appears to be continuous and not categorial. Recoding here. Is something wrong?
hiw_NP$NEWTWO <- as.logical(hiw_NP$NEWTWO)
hiw_NP$WORD = recontrast(hiw_NP$WORD) #change from treatment to sum contrasts because no one auxiliary is default
summary(hiw_NP)

# A base model
# I wanted to also have YOB, but it causes false convergence, probably because it's too correlated with speaker
hiw_NP_base.lme <- lmer(NEWTWO ~ NO_WORDS + SEX + EDUC + WORD + CV + PREC_STRESS + (1 | DIALECT) + (1 | SPEAKER), hiw_NP, verbose = TRUE, family = 'binomial')
summary(hiw_NP_base.lme)
