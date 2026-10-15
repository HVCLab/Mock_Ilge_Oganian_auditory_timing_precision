# Experiment 2a/b: Cue-target match
# C. M. Mock

rm(list=ls())

library(tidyverse)
library(lmerTest)
library(parameters)
library(effectsize)


# load participant-level data
DATA = readRDS(".../Exp2a_summary_behavioral_data.rds") # path to file (either Exp2a or Exp2b)


###################################
### linear mixed-effects model ###

# sum coding 
contrasts(DATA$cueSoundIdentity) = contr.sum(levels(DATA$cueSoundIdentity))
contrasts(DATA$cueTargetMatch) = contr.sum(levels(DATA$cueTargetMatch))

model_similarity = lmer(ptp ~ cueSoundIdentity * cueTargetMatch *musicality_zscore + (1|subjectID), data = DATA)
summary(model_similarity)


# CI
model_parameters(model_similarity, effects = "fixed", ci = 0.95, ci_method = "satterthwaite")


# partial R2
tab = coef(summary(model_similarity))

eta2_model_similarity = t_to_eta2(
  t = tab[, "t value"],
  df_error = tab[, "df"],
  ci = 0.95,
  alternative = "two.sided"
)

eta2_model_similarity$Parameter = rownames(tab)
eta2_model_similarity



####################
### figure 2B/C ###

summary_stats = DATA %>%
  group_by(cueSoundIdentity, cueTargetMatch) %>%
  summarise(
    grandMean_ptp = mean(ptp, na.rm = TRUE),
    se = sd(ptp, na.rm = TRUE)/sqrt(n()), 
    .groups = "drop"
  )


fig_2B_C = ggplot(summary_stats, aes(x = cueSoundIdentity, y = grandMean_ptp, 
                                                        group = cueTargetMatch, color = cueTargetMatch)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = grandMean_ptp - se, ymax = grandMean_ptp + se), width = 0.05) +
  labs(
    x = "Cue sound",
    y = "PTP (SD in ms)",
    color = "Cue-target match"
  ) +
  scale_color_manual(
    values = c("same" = "#DC3220", "different" = "#005AB5")
  ) +
  scale_y_continuous(breaks = c(20, 25, 30, 35, 40, 45)) +
  coord_cartesian(ylim = c(20, 45)) +
  theme_classic()


fig_2B_C

