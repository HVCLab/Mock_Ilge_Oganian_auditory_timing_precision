# Experiment 1: Effect of cue sequence length
# C. M. Mock

rm(list=ls())

library(tidyverse)
library(lmerTest)
library(parameters)
library(effectsize)
library(BayesFactor)


# load participant-level data
DATA = readRDS(".../Exp1_summary_behavioral_data.rds") # path to file
str(DATA)


# group-level summary ptp by condition
ptp_summary = DATA %>%
  group_by(condition) %>%
  summarise(
    grandMean_ptp = mean(ptp, na.rm = TRUE),
    se = sd(ptp, na.rm = TRUE)/sqrt(n()), 
    n = sum(!is.na(ptp)),
    .groups = "drop"
  )

ptp_summary



###################################
### linear mixed-effects model ###

levels(DATA$condition)

# m1: condition x musicality interaction
m1 = lmer(ptp ~ condition * musicality_zscore + (1|subjectID), data = DATA)
summary(m1)


# CI
model_parameters(m1, effects = "fixed", ci = 0.95, ci_method = "satterthwaite")

# partial R2
tab = coef(summary(m1))

eta2_m1 = t_to_eta2(
  t = tab[, "t value"],
  df_error = tab[, "df"],
  ci = 0.95,
  alternative = "two.sided"
)

eta2_m1$Parameter = rownames(tab)
eta2_m1


######################################
### effect of cue sequence length ###

# restrict analysis to conditions with SH cues
SH_data = DATA %>%
  filter(cueSoundIdentity != "FB") %>%
  droplevels()

levels(SH_data$cueSequenceLength)


# model cue sequence length
m_cueLength= lmer(ptp ~ cueSequenceLength  + (1|subjectID), data = SH_data)
summary(m_cueLength)


# CI
model_parameters(m_cueLength, effects = "fixed", ci = 0.95, ci_method = "satterthwaite")


# partial R2
tab = coef(summary(m_cueLength))

eta2_m_cueLength = t_to_eta2(
  t = tab[, "t value"],
  df_error = tab[, "df"],
  ci = 0.95,
  alternative = "two.sided"
)

eta2_m_cueLength$Parameter = rownames(tab)
eta2_m_cueLength



#########################################################
### bayesian analysis: effect of cue sequence length ###

# construct complete df
SH_data_BF = SH_data %>%
  filter(is.finite(ptp), !is.na(cueSequenceLength), !is.na(subjectID)) %>%
  mutate(
    cueSequenceLength = factor(cueSequenceLength, levels = c("4", "7", "10")),
    subjectID = factor(subjectID)
  ) %>%
  as.data.frame()

# check that frequentist and Bayesian analyses use the same N
c(LMM = nobs(m_cueLength), BF = nrow(SH_data_BF))

set.seed(1234)

bf_cueLength = anovaBF(
  ptp ~ cueSequenceLength + subjectID,
  data = SH_data_BF,
  whichRandom = "subjectID",
  rscaleFixed = "medium",
  iterations = 100000
)

bf_cueLength

# evidence for length effect (BF10) vs. null effect (BF01)
BF10 = extractBF(bf_cueLength, onlybf = TRUE)[1]
BF01 = 1/BF10

c(BF10 = BF10, BF01 = BF01)



################
### figures ###

# figure 1J 
fig_1J = ggplot(ptp_summary, aes(x = condition, y = grandMean_ptp)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = grandMean_ptp - se, ymax = grandMean_ptp + se), width = 0.05) +
  labs(
    x = "Cue Condition",
    y = "PTP (SD in ms)"
  ) +
  scale_x_discrete(labels = c("FB4", "SH4", "SH7", "SH10")) +   
  scale_y_continuous(breaks = seq(25, 50, 5)) +
  coord_cartesian(ylim = c(23, 50)) + 
  theme_classic()

fig_1J


# figure 1K
data_music = DATA %>%
  group_by(subjectID) %>%
  summarise(
    mean_ptp = mean(ptp, na.rm = TRUE),
    musicality_score = first(musicality_zscore), 
    .groups = "drop"
  )

fig_1K = ggplot(data_music, aes(musicality_score, y = mean_ptp)) +
  geom_point(size = 1, alpha = 1, color = "black", fill = "black") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "black", fill = "black", alpha = 0.2) +
  labs(
    x = "Musicality (z-scored)",
    y = "PTP (SD in ms)",
  ) + 
  theme_classic()

fig_1K
