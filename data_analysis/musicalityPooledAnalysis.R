# Pooled analysis: effect of musicality on PTP
# C. M. Mock

rm(list=ls())

library(tidyverse)
library(lmerTest)
library(emmeans)
library(parameters)
library(effectsize)


# load data
exp_files = c(
  Exp1  = "Exp1_summary_behavioral_data.rds",
  Exp2a = "Exp2a_summary_behavioral_data.rds",
  Exp2b = "Exp2b_summary_behavioral_data.rds",
  Exp3  = "Exp3_summary_behavioral_data.rds"
)

dat_all = imap_dfr(exp_files, ~
                      readRDS(file.path("data", .x)) %>% # path
                      transmute(
                        experiment = .y,
                        subjectID = as.character(subjectID),
                        condition = as.character(condition),
                        ptp,
                        musicality_zscore
                      )
) %>%
  mutate(experiment = factor(experiment, levels = names(exp_files)))

str(dat_all)


##############################################
### select conditions for pooled analysis ###

# identical sequences: FB-FB and SH-SH
# different sequences: cue and target differ in spectrum and envelope
# other conditions are excluded (see manuscript Table 1)

dat_q3 = dat_all %>%
  mutate(
    condition3 = case_when(
      condition %in% c("MM_short", "MM")  ~ "FB-FB",
      condition %in% c("CC", "S4-S4")    ~ "SH-SH",
      condition %in% c("CM_short", "CM", "S4-S5") ~ "different",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(condition3),
         is.finite(ptp),
         is.finite(musicality_zscore)) %>%
  mutate(
    condition3 = factor(condition3,
                        levels = c("different", "FB-FB", "SH-SH")),
    subjectID = factor(subjectID),
    experiment = droplevels(experiment)
  )



# treatment coding: "different" is the reference
contrasts(dat_q3$condition3) = contr.treatment(levels(dat_q3$condition3), base = 1)

# check included observations and participants
dat_q3 %>% count(experiment, condition3)



###################################
### linear mixed-effects model ###

m_q3 = lmer(ptp ~ condition3 * musicality_zscore + experiment + (1 | experiment:subjectID), data = dat_q3, REML = FALSE)
summary(m_q3)

# CI
model_parameters(m_q3, effects="fixed", ci=0.95, ci_method="satterthwaite")

# partial R2
tab = coef(summary(m_q3))

eta2_m_q3 = t_to_eta2(
  t = tab[, "t value"],
  df_error = tab[, "df"],
  ci = 0.95,
  alternative = "two.sided"
)

eta2_m_q3$Parameter = rownames(tab)
eta2_m_q3




########################################
### musicality slopes and contrasts ###

# musicality slopes separately for each condition
slopes_q3 = emtrends(m_q3, ~ condition3, var = "musicality_zscore")
slopes_q3

# test each slope against zero to get musicality association for each condition (different, MM, CC)
simple_slope_tests = test(slopes_q3, null = 0, adjust = "holm")
simple_slope_tests


# planned tests 
planned_tests = contrast(
  slopes_q3,
  method = list(
    "Average identical slope"         = c(0,  0.5,  0.5),
    "FB-FB versus SH-SH"              = c(0,  1,   -1),
    "Different slope"                 = c(1,  0,    0),
    "Identical versus different"      = c(-1, 0.5,  0.5)
  )
)

# estimates, unadjusted 95% CIs, Holm-adjusted p-values and partial eta2
planned_tests_results = as_tibble(
  summary(planned_tests, infer = c(TRUE, TRUE), adjust = "none")
) %>%
  mutate(
    p_holm = p.adjust(p.value, method = "holm"),
    eta2_partial = t.ratio^2 / (t.ratio^2 + df)
  )

planned_tests_results %>%
  select(contrast, estimate, SE, df, lower.CL, upper.CL,
         t.ratio, p_holm, eta2_partial)




##########################
### figures 5A and 5B ###

# Figure 5A: Musicality and PTP for identical sequences
fig_5A = dat_q3 %>%
  filter(condition3 != "different") %>%
  ggplot(aes(musicality_zscore, ptp, color = condition3, fill = condition3)) +
  geom_point(size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values = c("FB-FB" = "darkgreen", "SH-SH" = "magenta4")) + 
  scale_fill_manual(values = c("FB-FB" = "darkgreen", "SH-SH" = "magenta4")) +
  labs(x = "Musicality (z-scored)", y = "PTP (SD in ms)",
       color = "Condition") +
  theme_classic()

fig_5A


# Figure 5B: Musicality and PTP for identical vs. different sequences
data_match = dat_q3 %>%
  mutate(match = if_else(condition3 == "different", "Different", "Identical")) %>%
  group_by(experiment, subjectID, match) %>%
  summarise(
    ptp = mean(ptp),
    musicality_zscore = first(musicality_zscore),
    .groups = "drop"
  )

fig_5B = ggplot(data_match,
                 aes(musicality_zscore, ptp, color = match, fill = match)) +
  geom_point(size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.5, alpha = 0.3) +
  scale_color_manual(values = c("Identical" = "orangered2", "Different" = "royalblue3")) + 
  scale_fill_manual(values = c("Identical" = "orangered2", "Different" = "royalblue3")) +
  labs(x = "Musicality (z-scored)", y = "PTP (SD in ms)",
       color = "Condition") +
  theme_classic()

fig_5B

