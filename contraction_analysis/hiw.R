source("contraction.R")

# Reduce to the subset we're looking at.
hiw.NP = subset(hiw, NP == "NP")
# Code response as logical in a separate column
hiw.NP$NEWTWO.factor = factor(hiw.NP$NEWTWO, levels = c(0, 1), labels = c("Full", "Contracted"))
# Change from treatment to sum contrasts because no one auxiliary is default
hiw.NP$WORD = recontrast(hiw.NP$WORD) 
# Center DOB
hiw.NP$DOB = scale(hiw.NP$DOB, scale = FALSE)
# Add the general code of number of function words as the sum of
# monosyllabic and multisyllabic
hiw.NP$NO_FUNC_WORDS = hiw.NP$NO_FUNC_WORDS_MONO + hiw.NP$NO_FUNC_WORDS_MULTI
# Compute number of (phon_words + multisyllabic function words), which is just
# all the words minus monosyllabic function words
hiw.NP$NO_NOT_FUNC_MONO_WORDS = hiw.NP$NO_WORDS - hiw.NP$NO_FUNC_WORDS_MONO
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
hiw.NP$FREQLOWHOST = log2(hiw.NP$FREQLOWHOST)
hiw.NP$FREQAFTER = log2(hiw.NP$FREQAFTER)
hiw.NP$FREQLOWAFTER = log2(hiw.NP$FREQLOWAFTER)
# Subset again to remove any NAs from exclusions
hiw.NP <- subset(hiw.NP, !is.na(PFORWARD) & !is.na(PBACKWARD) & !is.na(PHOST) & !is.na(PAFTER) & !is.na(FREQHOST) & !is.na(FREQAFTER))

# Look at correlations among predictability predictors
# cor(subset(hiw.NP, select = c(NO_WORDS, FREQHOST, PFORWARD, PBACKWARD, PHOST, PAFTER, FREQAFTER)))
# Residualize PHOST (local frequency) and FREQHOST (global frequency). Since FREQHOST performs better, we give it primacy.
# It's best to not use these predictors at all; it's basically impossible to make sense of.
hiw.NP$rPHOST = resid(lm(PHOST ~ FREQHOST, hiw.NP))
hiw.NP$rPAFTER = resid(lm(PAFTER ~ FREQAFTER, hiw.NP))
# To be safe, we residualize all predictability measures off the frequency of the host. It isn't strictly necessary to do it to all of them, but it makes things more consistent.
hiw.NP$rPFORWARD = resid(lm(PFORWARD ~ FREQHOST, hiw.NP))
hiw.NP$rPBACKWARD = resid(lm(PBACKWARD ~ FREQHOST, hiw.NP))
# Sanity check
# cor(subset(hiw.NP, select = c(NO_WORDS, FREQHOST, rPFORWARD, rPBACKWARD, rPHOST, FREQAFTER)))

# Make subset with duration
hiw.NP.dur <- subset(hiw.NP, !is.na(SUBJ_DUR))
# Residualize, as the correlation is ~ .8
hiw.NP.dur$rSUBJ_DUR = resid(lm(SUBJ_DUR ~ NO_WORDS, hiw.NP.dur))
hiw.NP.dur$rNO_WORDS = resid(lm(NO_WORDS ~ SUBJ_DUR, hiw.NP.dur))

# Pull out PFORWARDs for analysis
hiw.NP.probs <- subset(hiw.NP, select = c(PREC_WORD, WORD, FOLL_WORD, PFORWARD, PBACKWARD, NO_WORDS, COMPLETE_SUBJ))
# Need to reverse logs here
hiw.NP.probs$PFORWARD <- 2^hiw.NP.probs$PFORWARD
hiw.NP.probs$PBACKWARD <- 2^hiw.NP.probs$PBACKWARD
# Write it out if you like
write.csv(hiw.NP.probs, "hiw_probs.csv", row.names = FALSE)