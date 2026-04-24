library(tidyverse)

# ------------------------------------------------------------------------------
# 01_load_and_clean.R
# Combines 883 individual participant CSVs into a single cleaned behavioral
# dataset and produces analytical subsets for RQ1 and RQ2.
#
# OPEN QUESTIONS (resolve before finalising analyses):
#   1. dismissedByStructuredIntervention: included as attempt + dismissal
#      (per Hanna's code). Confirm if participants using structured
#      interventions should remain in the main analysis.
#   2. "Never started" participants (N=1,275): completed pre-survey but
#      generated no behavioral data. Decide whether these count as immediate
#      dropouts in RQ1 or are excluded entirely.
# ------------------------------------------------------------------------------

base_path  <- "data/raw/sorted_by_condition"
label_map  <- c(control      = "CCC",
                experimental1 = "ABC",
                experimental2 = "ACC",
                experimental3 = "BCC")

# ------------------------------------------------------------------------------
# Step 1: Load and combine all CSVs
# Column order varies across files; read everything as character to avoid
# type-guessing failures on inconsistent columns.
# ------------------------------------------------------------------------------

# Load pre-survey condition map (authoritative source for condition assignment)
pre_survey_path <- list.files("data/raw", pattern = "Pre-survey", full.names = TRUE)
condition_map <- read_csv(pre_survey_path,
                          col_types = cols(.default = col_character()),
                          show_col_types = FALSE) %>%
  slice(-c(1, 2)) %>%
  filter(str_starts(uuid, "intervention-duration-study-spring-2025"),
         Finished == "True") %>%
  transmute(
    participant_id = str_extract(
      uuid,
      "[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"
    ) %>% str_to_upper(),
    condition_presurvey = recode(group,
      control       = "CCC",
      experimental1 = "ABC",
      experimental2 = "ACC",
      experimental3 = "BCC"
    )
  ) %>%
  distinct(participant_id, .keep_all = TRUE)

dat_raw <- map_dfr(names(label_map), function(cond) {
  folder <- file.path(base_path, cond)
  files  <- list.files(folder, full.names = TRUE, pattern = "\\.csv$")

  map_dfr(files, function(f) {
    read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
      mutate(source_file = basename(f))
  }) %>%
    mutate(condition_folder = label_map[[cond]])
}) %>%
  mutate(
    participant_id = str_extract(
      source_file,
      "[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"
    ) %>% str_to_upper()
  ) %>%
  # Join authoritative condition from pre-survey
  left_join(condition_map, by = "participant_id") %>%
  # For participants appearing in multiple folders, keep only rows matching
  # their pre-survey condition (resolves 7 duplicated CSVs)
  filter(condition_folder == condition_presurvey | is.na(condition_presurvey)) %>%
  mutate(condition = coalesce(condition_presurvey, condition_folder)) %>%
  select(-condition_folder, -condition_presurvey)

cat("Rows loaded:", nrow(dat_raw), "\n")

# Flag and remove rows where UUID extraction failed (filename didn't match pattern)
na_uuid_rows <- sum(is.na(dat_raw$participant_id))
if (na_uuid_rows > 0) {
  cat("WARNING: rows with NA participant_id (bad filename):", na_uuid_rows, "\n")
  na_uuid_files <- dat_raw %>% filter(is.na(participant_id)) %>% distinct(source_file)
  print(na_uuid_files)
  dat_raw <- dat_raw %>% filter(!is.na(participant_id))
}

cat("Unique participants:", n_distinct(dat_raw$participant_id), "\n")
cat("Conditions:\n"); print(table(dat_raw$condition, useNA = "always"))

# ------------------------------------------------------------------------------
# Step 2: Coerce column types
# ------------------------------------------------------------------------------

dat_raw <- dat_raw %>%
  mutate(
    timestamp            = as.numeric(timestamp),
    interventionDuration = as.numeric(interventionDuration),
    isReIntervention_clean = case_when(
      isReIntervention %in% c("1", "True")  ~ TRUE,
      isReIntervention %in% c("0", "False") ~ FALSE,
      TRUE ~ NA
    )
  )

# ------------------------------------------------------------------------------
# Step 3: Parse timestamp → datetime + study week
# study_week: floor(day_since_start / 7) + 1  → day 0 = week 1
# ------------------------------------------------------------------------------

dat_raw <- dat_raw %>%
  mutate(datetime = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"),
         date     = as.Date(datetime)) %>%
  group_by(participant_id) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  mutate(
    user_start      = min(timestamp, na.rm = TRUE),
    day_since_start = as.integer(difftime(
      datetime,
      as.POSIXct(user_start, origin = "1970-01-01", tz = "UTC"),
      units = "days"
    )),
    study_week = floor(day_since_start / 7) + 1
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Step 4: Flag and remove invalid data
# - Non-2025 timestamps (Hanna found one participant with year-2001 data)
# - Weeks > 6 (should disappear once non-2025 rows are removed)
# - Missing condition
# ------------------------------------------------------------------------------

dat_raw <- dat_raw %>%
  mutate(year = format(date, "%Y"))

invalid_year_ids <- dat_raw %>%
  filter(year != "2025") %>%
  distinct(participant_id) %>%
  pull()

cat("\nParticipants with non-2025 timestamps:", length(invalid_year_ids), "\n")
if (length(invalid_year_ids) > 0) print(invalid_year_ids)

missing_condition_ids <- dat_raw %>%
  filter(is.na(condition)) %>%
  distinct(participant_id) %>%
  pull()
cat("Participants with missing condition:", length(missing_condition_ids), "\n")

dat_clean <- dat_raw %>%
  filter(year == "2025",
         study_week <= 6,
         !is.na(condition))

cat("\nParticipants after cleaning:", n_distinct(dat_clean$participant_id), "\n")
cat("Rows after cleaning:", nrow(dat_clean), "\n")

# ------------------------------------------------------------------------------
# Step 5: Assign delay type from condition + study week
# interventionDuration values in the CSVs are unreliable (per developer).
# Delay type is derived entirely from condition + week.
#   A = 3s (preregistered), B = 6s, C = 10s
# ------------------------------------------------------------------------------

dat_clean <- dat_clean %>%
  mutate(delay_type = case_when(
    condition == "CCC"                         ~ "C",
    condition == "ABC" & study_week <= 2       ~ "A",
    condition == "ABC" & study_week <= 4       ~ "B",
    condition == "ABC"                         ~ "C",
    condition == "ACC" & study_week <= 2       ~ "A",
    condition == "ACC"                         ~ "C",
    condition == "BCC" & study_week <= 2       ~ "B",
    condition == "BCC"                         ~ "C",
    TRUE ~ NA_character_
  ))

cat("\nDelay type × condition:\n")
print(table(dat_clean$delay_type, dat_clean$condition, useNA = "always"))

# ------------------------------------------------------------------------------
# Step 6: Event classification variables
# initial_attempt: all intervention-triggered attempts (excludes closedApp)
# dismissed: user did not open the app after the intervention
# executed: user opened the app after the intervention
#
# NOTE: dismissedByStructuredIntervention is included as both attempt and
# dismissal, following Hanna's approach. Confirm if correct.
# ------------------------------------------------------------------------------

dat_clean <- dat_clean %>%
  mutate(
    initial_attempt = if_else(
      resolution %in% c("openedApp", "dismissedAppOpening",
                         "dismissedByStructuredIntervention"),
      1L, 0L, missing = 0L
    ),
    dismissed = if_else(
      resolution %in% c("dismissedAppOpening",
                         "dismissedByStructuredIntervention"),
      1L, 0L, missing = 0L
    ),
    executed = if_else(resolution == "openedApp", 1L, 0L, missing = 0L)
  )

cat("\nResolution value counts:\n")
print(dat_clean %>% count(resolution, sort = TRUE))

# ------------------------------------------------------------------------------
# Step 7: Dropout status
# dropout = 1 if participant has no activity in week 6
# ------------------------------------------------------------------------------

dat_dropout <- dat_clean %>%
  group_by(participant_id) %>%
  summarise(
    last_week = max(study_week, na.rm = TRUE),
    condition = first(condition),
    .groups   = "drop"
  ) %>%
  mutate(dropout = if_else(last_week < 6, 1L, 0L))

dat_clean <- dat_clean %>%
  left_join(dat_dropout %>% select(participant_id, dropout), by = "participant_id")

cat("\nDropout counts per condition:\n")
print(dat_dropout %>% count(condition, dropout))

# ------------------------------------------------------------------------------
# Step 8: Flag missing resolution rows
# ------------------------------------------------------------------------------

dat_clean <- dat_clean %>%
  mutate(flag_resolution_missing = is.na(resolution) | resolution == "")

cat("\nMissing resolution rows:", sum(dat_clean$flag_resolution_missing), "\n")
cat("Missing resolution by condition:\n")
print(dat_clean %>% filter(flag_resolution_missing) %>% count(condition))

# ------------------------------------------------------------------------------
# Step 9: Analytical subsets
# ------------------------------------------------------------------------------

# RQ1: one row per participant
df_rq1 <- dat_dropout

# RQ2 base: initial events only, completers only, no missing resolution
df_rq2 <- dat_clean %>%
  filter(
    isReIntervention_clean == FALSE | is.na(isReIntervention_clean),
    !flag_resolution_missing,
    dropout == 0
  )

cat("\nRQ2 base — unique participants:", n_distinct(df_rq2$participant_id), "\n")

# RQ2.1 model: one row per participant, week 1 + week 6 dismissed/attempted
df_rq2_1_model <- df_rq2 %>%
  filter(study_week %in% c(1, 6)) %>%
  group_by(participant_id, condition, study_week) %>%
  summarise(
    dismissed = sum(dismissed),
    attempted = sum(initial_attempt),
    .groups   = "drop"
  ) %>%
  pivot_wider(
    names_from  = study_week,
    values_from = c(dismissed, attempted),
    names_glue  = "{.value}_w{study_week}"
  ) %>%
  drop_na(dismissed_w6, attempted_w6) %>%
  filter(attempted_w6 > 0)  # offset = log(attempted_w6); log(0) undefined

# RQ2.2 model: one row per participant, week 1 + week 6 initial attempts
df_rq2_2_model <- df_rq2 %>%
  filter(study_week %in% c(1, 6)) %>%
  group_by(participant_id, condition, study_week) %>%
  summarise(initial = sum(initial_attempt), .groups = "drop") %>%
  pivot_wider(
    names_from  = study_week,
    values_from = initial,
    names_glue  = "initial_w{study_week}"
  ) %>%
  drop_na(initial_w6)

cat("RQ2.1 model N:", nrow(df_rq2_1_model), "\n")
cat("RQ2.2 model N:", nrow(df_rq2_2_model), "\n")

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

dir.create("data_clean", showWarnings = FALSE)

saveRDS(dat_clean,      "data_clean/dat_clean.rds")
saveRDS(df_rq1,         "data_clean/df_rq1.rds")
saveRDS(df_rq2,         "data_clean/df_rq2.rds")
saveRDS(df_rq2_1_model, "data_clean/df_rq2_1_model.rds")
saveRDS(df_rq2_2_model, "data_clean/df_rq2_2_model.rds")

write_csv(dat_clean,      "data_clean/dat_clean.csv")
write_csv(df_rq1,         "data_clean/df_rq1.csv")
write_csv(df_rq2_1_model, "data_clean/df_rq2_1_model.csv")
write_csv(df_rq2_2_model, "data_clean/df_rq2_2_model.csv")

cat("\nAll files saved to data_clean/\n")

# ------------------------------------------------------------------------------
# Verification checks (after saving so files exist regardless)
# ------------------------------------------------------------------------------

multi_cond <- dat_clean %>%
  distinct(participant_id, condition) %>%
  count(participant_id) %>%
  filter(n > 1)
cat("\nParticipants in multiple conditions:", nrow(multi_cond), "\n")
if (nrow(multi_cond) > 0) print(multi_cond)

cat("delay_type NAs:", sum(is.na(dat_clean$delay_type)), "\n")
cat("df_rq2_1_model NA in dismissed_w6:", sum(is.na(df_rq2_1_model$dismissed_w6)), "\n")
cat("df_rq2_1_model zero attempted_w6:", sum(df_rq2_1_model$attempted_w6 == 0), "\n")

cat("\nData cleaning complete.\n")
