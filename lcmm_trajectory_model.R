#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

data_file <- if (length(args) >= 1) args[1] else "traj_df_lcmm.csv"
out_file  <- if (length(args) >= 2) args[2] else "lcmm_class_assignments.csv"

cat("Input file :", data_file, "\n")
cat("Output file:", out_file, "\n")

if (!file.exists(data_file)) {
  stop(paste("Input file not found:", data_file))
}

if (!requireNamespace("lcmm", quietly = TRUE)) {
  stop("Package 'lcmm' is not installed. Install it first in R with: install.packages('lcmm', repos='https://cloud.r-project.org')")
}

library(lcmm)

traj_df <- read.csv(data_file)

traj_df$subject_id <- as.factor(traj_df$subject_id)
traj_df$window_90d <- as.numeric(traj_df$window_90d)

cat("\nData preview:\n")
print(head(traj_df))

cat("\nData summary:\n")
print(summary(traj_df))

traj_df_model <- transform(
  traj_df,
  subject_id = as.numeric(as.character(subject_id))
)

cat("\nFitting m1 (1 class)...\n")
m1 <- hlme(
  fixed = engagement_score ~ window_90d + I(window_90d^2),
  random = ~ window_90d,
  subject = "subject_id",
  ng = 1,
  data = traj_df_model,
  verbose = TRUE
)

cat("\nSummary: m1\n")
print(summary(m1))

cat("\nFitting m2 (2 classes)...\n")
m2 <- hlme(
  fixed = engagement_score ~ window_90d + I(window_90d^2),
  mixture = ~ window_90d + I(window_90d^2),
  random = ~ window_90d,
  subject = "subject_id",
  ng = 2,
  data = traj_df_model,
  B = m1,
  verbose = TRUE
)

cat("\nFitting m3 (3 classes)...\n")
m3 <- hlme(
  fixed = engagement_score ~ window_90d + I(window_90d^2),
  mixture = ~ window_90d + I(window_90d^2),
  random = ~ window_90d,
  subject = "subject_id",
  ng = 3,
  data = traj_df_model,
  B = m1,
  verbose = TRUE
)

cat("\nFitting m4 (4 classes)...\n")
m4 <- hlme(
  fixed = engagement_score ~ window_90d + I(window_90d^2),
  mixture = ~ window_90d + I(window_90d^2),
  random = ~ window_90d,
  subject = "subject_id",
  ng = 4,
  data = traj_df_model,
  B = m1,
  verbose = TRUE
)

cat("\nFitting m5 (5 classes)...\n")
m5 <- hlme(
  fixed = engagement_score ~ window_90d + I(window_90d^2),
  mixture = ~ window_90d + I(window_90d^2),
  random = ~ window_90d,
  subject = "subject_id",
  ng = 5,
  data = traj_df_model,
  B = m1,
  verbose = TRUE
)

cat("\nModel comparison table:\n")
print(summarytable(m1, m2, m3, m4, m5, which = c("G", "loglik", "AIC", "BIC", "entropy")))

cat("\nPosterior probabilities for m4:\n")
post4 <- postprob(m4)
print(head(post4))
print(summary(post4))

class_assign <- postprob(m4)
write.csv(class_assign, out_file, row.names = FALSE)

cat("\nSaved class assignments to:", out_file, "\n")
