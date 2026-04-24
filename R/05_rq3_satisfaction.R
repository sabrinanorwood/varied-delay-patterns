library(tidyverse)
library(broom)
library(emmeans)
library(flextable)
library(officer)

dir.create("results_rq3", showWarnings = FALSE)

df_post <- readRDS("data_clean/df_post.rds") %>%
  mutate(condition = relevel(factor(condition), ref = "CCC"))

# Use all post-survey completers with a condition assignment.
# Note: 79 respondents have no behavioral CSV (open question — see memory).
# They still have a condition from the pre-survey so are included here.
# Flag these for sensitivity analysis if needed.
cat("Post-survey N:", nrow(df_post), "\n")
cat("With behavioral data:", sum(df_post$has_behavioral), "\n")
cat("Without behavioral data:", sum(!df_post$has_behavioral), "\n")

# ------------------------------------------------------------------------------
# Satisfaction items (preregistered, each modelled separately)
# ------------------------------------------------------------------------------

satisfaction_items <- c(
  solved_problems  = "Have you been able to solve your consumption problems?",
  achieved_goals   = "Have you been able to achieve your consumption goals?",
  helpful_problems = "How helpful to solve your consumption problems?",
  helpful_goals    = "How helpful to achieve your consumption goals?",
  satisfied_interv = "How satisfied with one sec's intervention?",
  satisfied_progr  = "How satisfied with your progress through one sec?"
)

# Descriptive summary per condition per item
df_desc <- map_dfr(names(satisfaction_items), function(item) {
  df_post %>%
    group_by(condition) %>%
    summarise(
      item     = item,
      label    = satisfaction_items[[item]],
      n        = sum(!is.na(.data[[item]])),
      mean     = round(mean(.data[[item]], na.rm = TRUE), 2),
      sd       = round(sd(.data[[item]], na.rm = TRUE), 2),
      .groups  = "drop"
    )
})

print(df_desc)
write_csv(df_desc, "results_rq3/satisfaction_descriptives.csv")

# Linear regression: each item ~ condition
all_results <- map_dfr(names(satisfaction_items), function(item) {
  formula <- as.formula(paste(item, "~ condition"))
  model   <- lm(formula, data = df_post)

  tidy(model, conf.int = TRUE) %>%
    mutate(
      item    = item,
      label   = satisfaction_items[[.env$item]],
      B_CI    = paste0(round(estimate, 2), " [", round(conf.low, 2), ", ",
                       round(conf.high, 2), "]"),
      p       = case_when(p.value < .001 ~ "< .001",
                          p.value < .01  ~ "< .01",
                          p.value < .05  ~ "< .05",
                          TRUE           ~ as.character(round(p.value, 3)))
    ) %>%
    select(item, label, term, B_CI, p)
})

print(all_results)
write_csv(all_results, "results_rq3/model_rq3_all_items.csv")

# Pairwise comparisons for each item
pairwise_rq3 <- map_dfr(names(satisfaction_items), function(item) {
  formula <- as.formula(paste(item, "~ condition"))
  model   <- lm(formula, data = df_post)

  emmeans(model, ~ condition) %>%
    contrast("pairwise", adjust = "none") %>%
    summary(infer = TRUE) %>%
    as_tibble() %>%
    mutate(
      item  = item,
      label = satisfaction_items[[.env$item]],
      across(c(estimate, SE, t.ratio, lower.CL, upper.CL), ~ round(.x, 3)),
      p     = case_when(p.value < .001 ~ "< .001",
                        p.value < .01  ~ "< .01",
                        p.value < .05  ~ "< .05",
                        TRUE           ~ as.character(round(p.value, 3)))
    ) %>%
    select(item, contrast, estimate, SE, lower.CL, upper.CL, p)
})

print(pairwise_rq3)
write_csv(pairwise_rq3, "results_rq3/pairwise_rq3.csv")

# ------------------------------------------------------------------------------
# Word table
# ------------------------------------------------------------------------------

doc <- read_docx() %>%
  body_add_par("RQ3: Satisfaction — Linear Regression Results", style = "heading 1") %>%
  body_add_par("Descriptive Statistics", style = "heading 2") %>%
  body_add_flextable(flextable(df_desc) %>% autofit()) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Regression Coefficients", style = "heading 2") %>%
  body_add_flextable(flextable(all_results) %>% autofit()) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Pairwise Comparisons", style = "heading 2") %>%
  body_add_flextable(flextable(pairwise_rq3) %>% autofit())

print(doc, target = "results_rq3/results_rq3.docx")
cat("RQ3 complete. Results saved to results_rq3/\n")
