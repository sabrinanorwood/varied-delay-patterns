library(tidyverse)
library(flextable)
library(officer)

dir.create("results_rq5", showWarnings = FALSE)

# RQ5 is descriptive only — no condition breakdown (preregistered)
df_post <- readRDS("data_clean/df_post.rds")

cat("Post-survey N:", nrow(df_post), "\n")

# ------------------------------------------------------------------------------
# RQ5.1: Prefer to set own delay length?
# ------------------------------------------------------------------------------

df_own_delay <- df_post %>%
  count(prefer_own_delay) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

cat("\nPrefer to set own delay?\n"); print(df_own_delay)

# Among those who said yes: preferred duration in seconds
df_pref_secs <- df_post %>%
  filter(prefer_own_delay == "Yes", !is.na(pref_delay_secs)) %>%
  summarise(
    n      = n(),
    mean   = round(mean(pref_delay_secs), 1),
    median = median(pref_delay_secs),
    sd     = round(sd(pref_delay_secs), 1),
    min    = min(pref_delay_secs),
    max    = max(pref_delay_secs)
  )

cat("\nPreferred delay duration (among 'Yes' respondents):\n"); print(df_pref_secs)

# ------------------------------------------------------------------------------
# RQ5.2: Prefer increasing delay?
# ------------------------------------------------------------------------------

df_increasing <- df_post %>%
  count(prefer_increasing) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

cat("\nPrefer increasing delay?\n"); print(df_increasing)

# Among those who said yes: which pattern?
df_pref_pattern <- df_post %>%
  filter(prefer_increasing == "Yes", !is.na(pref_pattern)) %>%
  count(pref_pattern) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

cat("\nPreferred pattern (among those who want increasing delay):\n")
print(df_pref_pattern)

# Custom weekly pattern responses (Q36_1 to Q36_9 — open-ended)
df_custom <- df_post %>%
  filter(!is.na(pref_week1)) %>%
  select(participant_id, starts_with("pref_week")) %>%
  pivot_longer(-participant_id, names_to = "week",
               names_prefix = "pref_week",
               values_to = "seconds") %>%
  mutate(week = as.integer(week)) %>%
  group_by(week) %>%
  summarise(
    n      = sum(!is.na(seconds)),
    mean   = round(mean(seconds, na.rm = TRUE), 1),
    median = median(seconds, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nCustom preferred weekly pattern (among those who entered one):\n")
print(df_custom)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

write_csv(df_own_delay,      "results_rq5/prefer_own_delay.csv")
write_csv(df_pref_secs,      "results_rq5/preferred_delay_seconds.csv")
write_csv(df_increasing,     "results_rq5/prefer_increasing.csv")
write_csv(df_pref_pattern,   "results_rq5/preferred_pattern.csv")
write_csv(df_custom,         "results_rq5/custom_weekly_pattern.csv")

doc <- read_docx() %>%
  body_add_par("RQ5: Preferred Delay Pattern (Descriptive)", style = "heading 1") %>%
  body_add_par("Prefer to set own delay length?", style = "heading 2") %>%
  body_add_flextable(flextable(df_own_delay) %>% autofit()) %>%
  body_add_par("Preferred delay duration in seconds (among 'Yes' respondents)",
               style = "heading 2") %>%
  body_add_flextable(flextable(df_pref_secs) %>% autofit()) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Prefer an increasing delay?", style = "heading 2") %>%
  body_add_flextable(flextable(df_increasing) %>% autofit()) %>%
  body_add_par("Preferred pattern (among 'Yes' respondents)", style = "heading 2") %>%
  body_add_flextable(flextable(df_pref_pattern) %>% autofit())

print(doc, target = "results_rq5/results_rq5.docx")
cat("RQ5 complete. Results saved to results_rq5/\n")
