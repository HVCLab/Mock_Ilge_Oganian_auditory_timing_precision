# Experiment 3: Spectral and temporal cue-target continuity
# C. M. Mock

rm(list=ls())

library(tidyverse)
library(lmerTest)
library(parameters)
library(effectsize)


# load participant-level data
DATA = readRDS(".../Exp3_summary_behavioral_data.rds") # path to file


###################################
### linear mixed-effects model ###

# contrasts  
contrasts(DATA$spectrum) 
contrasts(DATA$envelope)

model_continuity = lmer(ptp ~ spectrum * envelope * musicality_zscore + (1|subjectID), data = DATA)
summary(model_continuity)


# CI
model_parameters(model_continuity, effects = "fixed", ci = 0.95, ci_method = "satterthwaite")


# partial R2
tab = coef(summary(model_continuity))

eta2_model_continuity = t_to_eta2(
  t = tab[, "t value"],
  df_error = tab[, "df"],
  ci = 0.95,
  alternative = "two.sided"
)

eta2_model_continuity$Parameter = rownames(tab)
eta2_model_continuity



#####################
### figure 3 D+E ###

summary_stats = DATA %>%
  group_by(spectrum, envelope) %>%
  summarise(
    grandMean_ptp = mean(ptp, na.rm = TRUE),
    se = sd(ptp, na.rm = TRUE)/sqrt(n()), 
    .groups = "drop"
  )


fig_3D = ggplot(summary_stats, aes(x = spectrum, y = grandMean_ptp, group = envelope)) +
  geom_errorbar(aes(ymin = grandMean_ptp - se, ymax = grandMean_ptp + se, linetype = envelope),
    width = 0.05,
    linewidth = 0.5,
    colour = "black") +
  geom_line(aes(linetype = envelope), linewidth = 0.8, colour = "black") +
  geom_point(aes(shape = spectrum, fill = envelope),
    size = 3,
    stroke = 0.8,
    colour = "black") +
  labs(
    x = "Cue-target spectrum",
    y = "PTP (SD in ms)",
    shape = "Cue-target\nspectrum",
    fill = "Cue-target\nenvelope",
    linetype = "Cue-target\nenvelope") +
  scale_shape_manual(
    values = c("same" = 21, "different" = 24),   # circle, triangle
    labels = c("same" = "Same", "different" = "Different")) +
  scale_fill_manual(
    values = c("same" = "black", "different" = "white"),
    labels = c("same" = "Same", "different" = "Different")) +
  scale_linetype_manual(
    values = c("same" = "solid", "different" = "dashed"),
    labels = c("same" = "Same", "different" = "Different")) +
  scale_x_discrete(
    labels = c("same" = "Same", "different" = "Different")) +
  scale_y_continuous(breaks = c(20, 25, 30, 35, 40)) +
  coord_cartesian(ylim = c(20, 42)) +
  theme_classic()

fig_3D



data_interaction_plot = DATA %>%
  group_by(subjectID, spectrum, envelope) %>%
  summarise(
    grandMean_ptp = mean(ptp, na.rm = TRUE),
    musicality_score = first(musicality_zscore),
    .groups = "drop") %>%
  mutate(spectrum = fct_relevel(factor(spectrum), "same", "different"),
         envelope = fct_relevel(factor(envelope), "same", "different"))


fig_3E = ggplot(data_interaction_plot, aes(musicality_score, grandMean_ptp)) +
  geom_smooth(aes(linetype = envelope, fill = envelope),
    method = "lm", formula = y ~ x, se = TRUE,
    color = "black", linewidth = 0.8, alpha = 0.55) +
  geom_point(data = filter(data_interaction_plot, envelope == "same"),
    aes(shape = spectrum),
    size = 1.6, stroke = 0.5, color = "black", fill = "black") +
  geom_point(
    data = filter(data_interaction_plot, envelope == "different"),
    aes(shape = spectrum),
    size = 1.6, stroke = 0.5, color = "black", fill = "white") +
  facet_wrap(~ spectrum, nrow = 1, 
             labeller = labeller(spectrum = c(same = "Same", different = "Different"))) +
  scale_shape_manual(values = c(same = 21, different = 24)) +
  scale_fill_manual(values = c(same = "grey70", different = "grey88")) +
  scale_linetype_manual(values = c(same = "solid", different = "dashed")) +
  scale_x_continuous(breaks = -1:2) +
  scale_y_continuous(breaks = seq(0, 75, 15)) +
  coord_cartesian(ylim = c(0, 76)) +
  labs(
    title = "Cue-target spectrum",
    x = "Musicality (z-scored)",
    y = "PTP (SD in ms)"
  ) +
  theme_classic()

fig_3E


