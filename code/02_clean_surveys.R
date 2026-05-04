library(tidyverse)

# ------------------------------------------------------------------------------
# 02_clean_surveys.R
# Cleans the Qualtrics pre- and post-survey exports and joins them to the
# behavioral dataset. Run after 01_load_and_clean.R.
#
# OPEN QUESTIONS:
#   1. 93 post-survey completers have no behavioral CSV. Investigate before
#      using post-survey data for RQ3 (may be missing app data, not true
#      completers, or accessed the survey link without finishing 6 weeks).
#   2. "Never started" (N=1,275): pre-survey finishers with no behavioral data.
#      Decide if these count as immediate dropouts in RQ1.
# ------------------------------------------------------------------------------

pre_path  <- "data/raw/Pre-survey, gradually increasing friction, one sec_April 24, 2026_05.00.csv"
post_path <- "data/raw/Post-survey, gradually increasing friction, one sec_April 24, 2026_04.58.csv"

# Qualtrics exports have 3 header rows:
#   row 1 = column names, row 2 = question labels, row 3 = import instructions
# skip = 1 + slice(-1) removes the label and import rows while keeping col names.

# ------------------------------------------------------------------------------
# Pre-survey
# ------------------------------------------------------------------------------

df_pre_raw <- read_csv(pre_path,
                       col_types = cols(.default = col_character()),
                       show_col_types = FALSE) %>%
  slice(-c(1, 2))  # remove question-label row and import-instructions row

df_pre <- df_pre_raw %>%
  filter(
    str_starts(uuid, "intervention-duration-study-spring-2025"),
    Finished == "True"
  ) %>%
  mutate(
    participant_id = str_extract(
      uuid,
      "[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"
    ) %>% str_to_upper(),
    condition = recode(group,
      control       = "CCC",
      experimental1 = "ABC",
      experimental2 = "ACC",
      experimental3 = "BCC"
    ),
    # Baseline social media measures (shared with post-survey for pre-post comparison)
    sm_problematic_pre  = as.integer(str_extract(Q6, "^[0-9]")),
    sm_happiness_pre    = as.integer(str_extract(Q9, "^[0-9]")),
    sm_general_prob_pre = as.integer(str_extract(Q8, "^[0-9]")),
    sm_comparison_pre   = as.integer(str_extract(Q5, "^[0-9]")),
    problems_sm         = Q2,
    goals_sm            = Q3
  ) %>%
  select(participant_id, condition, ResponseId, StartDate, EndDate,
         starts_with("sm_"), problems_sm, goals_sm, isNewUser, appSetupTimeStamp)

cat("Pre-survey finishers (this study):", nrow(df_pre), "\n")
cat("Pre-survey condition distribution:\n")
print(count(df_pre, condition))

# Document "never started": pre-survey finishers with no behavioral CSV
behavioral_ids <- readRDS("data_clean/dat_clean.rds") %>%
  distinct(participant_id) %>%
  pull()

df_pre <- df_pre %>%
  mutate(has_behavioral = participant_id %in% behavioral_ids)

never_started <- df_pre %>% filter(!has_behavioral)
cat("\n'Never started' (pre-survey only, no behavioral data):", nrow(never_started), "\n")
cat("Never started by condition:\n")
print(count(never_started, condition))

# ------------------------------------------------------------------------------
# Post-survey
# ------------------------------------------------------------------------------

df_post_raw <- read_csv(post_path,
                        col_types = cols(.default = col_character()),
                        show_col_types = FALSE) %>%
  slice(-c(1, 2))

df_post <- df_post_raw %>%
  filter(Finished == "True") %>%
  mutate(
    participant_id = str_extract(
      uuid,
      "[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"
    ) %>% str_to_upper(),
    # Likert responses are stored as "4 - neutral" — extract the leading integer
    across(c(Q19_1, Q19_2, Q37, Q38, Q40, Q39, Q17, Q9, Q8, Q5),
           ~ as.integer(str_extract(.x, "^[0-9]"))),
    # RQ3: satisfaction items
    solved_problems  = Q19_1,
    achieved_goals   = Q19_2,
    helpful_problems = Q37,
    helpful_goals    = Q38,
    satisfied_interv = Q40,
    satisfied_progr  = Q39,
    # Post-survey social media measures (for pre-post comparison)
    sm_problematic_post  = Q17,
    sm_happiness_post    = Q9,
    sm_general_prob_post = Q8,
    sm_comparison_post   = Q5,
    # RQ5: delay preference items
    prefer_own_delay  = Q27,
    pref_delay_secs   = as.numeric(Q28_4),
    prefer_increasing = Q29,
    pref_pattern      = Q31,
    # RQ5: custom weekly pattern (if participant chose "other")
    pref_week1 = as.numeric(Q36_1),
    pref_week2 = as.numeric(Q36_2),
    pref_week3 = as.numeric(Q36_3),
    pref_week4 = as.numeric(Q36_7),
    pref_week5 = as.numeric(Q36_8),
    pref_week6 = as.numeric(Q36_9)
  ) %>%
  select(participant_id, ResponseId, StartDate, EndDate,
         solved_problems:pref_week6, appSetupTimeStamp)

# Join condition from pre-survey (post-survey has no group column)
df_post <- df_post %>%
  left_join(df_pre %>% select(participant_id, condition), by = "participant_id")

# Flag the 93 post-survey completers who have no behavioral data
df_post <- df_post %>%
  mutate(has_behavioral = participant_id %in% behavioral_ids)

cat("\nPost-survey completers:", nrow(df_post), "\n")
cat("  With behavioral data:", sum(df_post$has_behavioral), "\n")
cat("  Without behavioral data (investigate before RQ3):",
    sum(!df_post$has_behavioral), "\n")
cat("Post-survey condition distribution:\n")
print(count(df_post, condition))

# ------------------------------------------------------------------------------
# RQ4: Dropout reasons from behavioral CSVs
# Some CSV files include a terminationReason column.
# ------------------------------------------------------------------------------

df_termination <- readRDS("data_clean/dat_clean.rds") %>%
  filter(!is.na(terminationReason) & terminationReason != "") %>%
  distinct(participant_id, terminationReason) %>%
  left_join(df_pre %>% select(participant_id, condition), by = "participant_id")

cat("\nParticipants with a recorded termination reason:", nrow(df_termination), "\n")
cat("Termination reasons:\n")
print(count(df_termination, terminationReason, sort = TRUE))

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

saveRDS(df_pre,         "data_clean/df_pre.rds")
saveRDS(df_post,        "data_clean/df_post.rds")
saveRDS(never_started,  "data_clean/df_never_started.rds")
saveRDS(df_termination, "data_clean/df_termination.rds")

write_csv(df_pre,        "data_clean/df_pre.csv")
write_csv(df_post,       "data_clean/df_post.csv")
write_csv(df_termination,"data_clean/df_termination.csv")

cat("\nAll survey files saved to data_clean/\n")
