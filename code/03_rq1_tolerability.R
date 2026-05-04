
# deviations from prereg
# - tolerability is a 0/1 variable, and is analyzed appropriately as a logistic regression rather than negative binomial as misspecified in the prereg


library(tidyverse)
library(broom)
library(emmeans)
library(flextable)
library(officer)

dir.create("results_rq1", showWarnings = FALSE)

df_rq1 <- readRDS("data_clean/df_rq1.rds") %>%
  mutate(condition = relevel(factor(condition), ref = "CCC"))

# ------------------------------------------------------------------------------
# Descriptive: dropout counts and rates per condition
# ------------------------------------------------------------------------------

df_rq1_summary <- df_rq1 %>%
  group_by(condition) %>%
  summarise(
    n_total   = n(),
    n_dropout = sum(dropout),
    n_complete = sum(dropout == 0),
    dropout_rate = round(n_dropout / n_total * 100, 1),
    .groups = "drop"
  )

print(df_rq1_summary)
write_csv(df_rq1_summary, "results_rq1/dropout_summary.csv")

# ------------------------------------------------------------------------------
# Weekly dropout: how many dropped out in each week
# ------------------------------------------------------------------------------

df_weekly_dropout <- df_rq1 %>%
  filter(dropout == 1) %>%
  count(condition, last_week, name = "n_dropouts")

write_csv(df_weekly_dropout, "results_rq1/weekly_dropout_counts.csv")

# ------------------------------------------------------------------------------
# RQ1 model: binary logistic regression (dropout ~ condition)
# Preregistration specified negative binomial but dropout is binary — logistic
# is the correct choice (confirmed by Hanna's thesis).
# ------------------------------------------------------------------------------

model_rq1 <- glm(dropout ~ condition, 
                 data = df_rq1, 
                 family = binomial(link = "logit"))

summary(model_rq1)

# Tidy results with odds ratios and 95% CIs
results_rq1 <- tidy(model_rq1, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    OR     = round(estimate, 3),
    CI     = paste0("[", round(conf.low, 3), ", ", round(conf.high, 3), "]"),
    p      = case_when(p.value < .001 ~ "< .001",
                       p.value < .01  ~ "< .01",
                       p.value < .05  ~ "< .05",
                       TRUE           ~ as.character(round(p.value, 3))),
    B_SE   = paste0(round(log(estimate), 3), " (", round(std.error, 3), ")")
  ) %>%
  select(term, B_SE, OR, CI, p) %>%
  rename(Predictor = term, `B (SE)` = B_SE, `OR` = OR, `95% CI` = CI, `p` = p)

print(results_rq1)
write_csv(results_rq1, "results_rq1/model_rq1_results.csv")

# Model fit
chi_sq  <- model_rq1$null.deviance - model_rq1$deviance
df_diff <- model_rq1$df.null - model_rq1$df.residual
p_fit   <- pchisq(chi_sq, df = df_diff, lower.tail = FALSE)
r2_mcf  <- 1 - (logLik(model_rq1) / logLik(update(model_rq1, . ~ 1)))

cat(sprintf("\nModel fit: χ²(%d) = %.2f, p = %.3f, McFadden R² = %.3f\n",
            df_diff, chi_sq, p_fit, as.numeric(r2_mcf)))

# ------------------------------------------------------------------------------
# Pairwise comparisons (all pairs)
# ------------------------------------------------------------------------------

pairwise_rq1 <- emmeans(model_rq1, ~ condition) %>%
  contrast("pairwise", adjust = "none") %>%
  summary(infer = TRUE, type = "response") %>%
  as_tibble() %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3)),
    p = case_when(p.value < .001 ~ "< .001",
                  p.value < .01  ~ "< .01",
                  p.value < .05  ~ "< .05",
                  TRUE           ~ as.character(round(p.value, 3)))
  )

print(pairwise_rq1)
write_csv(pairwise_rq1, "results_rq1/pairwise_rq1.csv")

# ------------------------------------------------------------------------------
# Word table
# ------------------------------------------------------------------------------

doc <- read_docx() %>%
  body_add_par("RQ1: Logistic Regression — Dropout by Condition", style = "heading 1") %>%
  body_add_flextable(flextable(results_rq1) %>% autofit()) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Pairwise Comparisons (odds ratios)", style = "heading 2") %>%
  body_add_flextable(flextable(pairwise_rq1 %>%
                                 select(contrast, odds.ratio, SE, asymp.LCL, asymp.UCL, p)) %>%
                       autofit())

print(doc, target = "results_rq1/results_rq1.docx")
cat("RQ1 complete. Results saved to results_rq1/\n")
