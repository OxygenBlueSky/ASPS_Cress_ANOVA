# s4b_cress_transformation_check.r
#
# Pipeline step 4b: compare three response-variable transformations
# (raw, Box-Cox, INT) on each of the four ASPS cress response variables,
# at seedling level, using ANOVA residuals from y ~ potency * experiment_number
# as the diagnostic target.
#
# Why this script exists: s4 fixes a left tail by truncation, but does not
# tell us whether the residuals from the ANOVA in s5 are normal enough for
# Type-III inference to be trustworthy. This script picks the transformation
# that brings residuals closest to Gaussian + homoscedastic across the 6
# potencies, so the choice of transformation downstream is evidence-based
# rather than defaulted to "raw".
#
# Independent of s4: reads the same s3 v1v2 combined file, never the s4
# truncated columns -- the question here is what a transformation alone can
# do for the distribution, before any cutoff is applied.
#
# Inputs : s3 v1v2 combined xlsx (cress_combine_files/), filtered by
#          DATASET_VER via in_v1_analysis / in_v2_analysis columns.
# Outputs: per-variable overview plot (3 transformations x 3 diagnostic
#          panels), per-variable per-transformation Q-Q faceted by potency
#          (3x2 portrait), and one summary CSV of Shapiro / Levene stats.


library(readxl)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(MASS)        # boxcox()
library(car)         # leveneTest()


#===== CONFIG ===============================================================

SCRIPT_TAG     <- "s4b"
SCRIPT_PURPOSE <- "transform_check"
DATASET_VER    <- "v2"             # "v1" | "v2" | "v1v2"
ANALYSIS_LEVEL <- "seedling"       # fixed; documented for parallelism with s5
RUN_DATE       <- format(Sys.Date(), "%Y%m%d")

# Same four response variables s5 analyses. root_sprout_ratio is included;
# Box-Cox needs strictly positive values, asserted below.
response_vars <- c("seedling_length", "sprout_length",
                   "root_length", "root_sprout_ratio")

# Sub-sample size for Shapiro-Wilk. At seedling level n is typically several
# thousand and Shapiro becomes overpowered (rejects on cosmetic deviations).
# Fixed seed for reproducibility.
SHAPIRO_SUBSAMPLE <- 5000
SHAPIRO_SEED      <- 1

INPUT_S3_BASENAME <- "cress_length_ASPS_1-10_alldata_decoded_v1v2.xlsx"
INPUT_S3_PARENT   <- "cress_combine_files"


#===== DERIVED PATHS (don't edit) ===========================================

out_dir <- file.path(
  "outputs",
  paste(RUN_DATE, SCRIPT_TAG, SCRIPT_PURPOSE, DATASET_VER, sep = "_")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_path <- function(suffix, ext) {
  file.path(
    out_dir,
    paste0(RUN_DATE, "_", SCRIPT_TAG, "_", DATASET_VER, "_", suffix, ".", ext)
  )
}


#===== RESOLVE INPUT ========================================================

# Same pattern as s4 / s5: most-recent <date>_cress_combined/ folder under
# cress_combine_files/, lexicographic sort = chronological because the date
# prefix is YYYYMMDD.
resolve_s3_input <- function() {
  combined_root <- here(INPUT_S3_PARENT)
  candidates <- sort(
    list.dirs(combined_root, full.names = FALSE, recursive = FALSE),
    decreasing = TRUE
  )
  candidates <- candidates[grepl("_cress_combined$", candidates)]
  for (d in candidates) {
    p <- file.path(combined_root, d, INPUT_S3_BASENAME)
    if (file.exists(p)) return(p)
  }
  stop("No ", INPUT_S3_BASENAME, " found under ", combined_root,
       "/*_cress_combined/. Run s3_cress_combine_files_v2.r first.")
}

input_path <- resolve_s3_input()
cat("Reading: ", input_path, "\n", sep = "")

df_raw <- read_excel(input_path, sheet = "Sheet 1")

# Filter by dataset version using the membership flags written by s3.
df_raw <- switch(DATASET_VER,
  "v1"   = df_raw[df_raw$in_v1_analysis, ],
  "v2"   = df_raw[df_raw$in_v2_analysis, ],
  "v1v2" = df_raw,
  stop("DATASET_VER must be 'v1', 'v2', or 'v1v2'; got: ", DATASET_VER)
)
cat("Rows after dataset_ver filter (", DATASET_VER, "): ",
    nrow(df_raw), "\n", sep = "")


#===== PARSE FACTORS ========================================================

# exp_no is "<experiment_number>_<potency_code>" (e.g. "3_A"). Same parse
# s5 uses around line 237. We need potency (decoded name, 6 levels) and
# experiment_number (integer 1-10) as factors for the lm() below.
df <- df_raw %>%
  tidyr::separate(exp_no, into = c("experiment_number", "potency_code"),
                  sep = "_", remove = FALSE) %>%
  mutate(
    experiment_number = factor(as.integer(experiment_number)),
    potency           = factor(potency)
  )

cat("Potency levels   : ", paste(levels(df$potency), collapse = ", "),
    " (n=", nlevels(df$potency), ")\n", sep = "")
cat("Experiment levels: ", paste(levels(df$experiment_number), collapse = ", "),
    " (n=", nlevels(df$experiment_number), ")\n", sep = "")

# Sum-to-zero contrasts so Box-Cox / residuals don't depend on which level
# happens to be the reference under default treatment coding.
contrasts(df$potency)           <- contr.sum(nlevels(df$potency))
contrasts(df$experiment_number) <- contr.sum(nlevels(df$experiment_number))


#===== HELPERS ==============================================================

# Inverse normal transform (Blom). Apply to the response, then refit the model
# -- applying it to residuals would force them normal by construction.
int_transform <- function(y) {
  n_ok <- sum(!is.na(y))
  qnorm(rank(y, ties.method = "average", na.last = "keep") / (n_ok + 1))
}

# Pick Box-Cox lambda at the max-likelihood point. We refit a plain lm so the
# boxcox() profile is built on the same design we'll use for residuals later.
# Returns the lambda; the caller does the actual transformation so the
# log-special-case is visible in one place.
estimate_boxcox_lambda <- function(y, df_sub) {
  if (any(y <= 0, na.rm = TRUE)) {
    stop("Box-Cox requires strictly positive y; ",
         "min(y) = ", min(y, na.rm = TRUE),
         ". Filter zeros/negatives upstream or pick a different transform.")
  }
  # Use the formula form. boxcox.lm() calls update() on a fitted model and
  # re-evaluates data = ... in its own frame, which fails for locals like
  # df_sub. The formula form sidesteps that by accepting (formula, data)
  # directly. Stuff y onto a local copy of the frame so the formula resolves.
  df_bc <- df_sub
  df_bc$.y <- y
  # Wide search range. MASS::boxcox only evaluates the lambda values you pass,
  # so a default [-2, 2] grid silently truncates an optimum that lies outside.
  # We scan [-3, 5] at 0.05 steps; if the max still hits an endpoint we warn
  # in the caller. Returns both the chosen lambda and the full profile so the
  # caller can save the log-likelihood plot.
  lambda_grid <- seq(-3, 5, by = 0.05)
  bc <- MASS::boxcox(.y ~ potency * experiment_number, data = df_bc,
                     plotit = FALSE, lambda = lambda_grid)
  list(
    lambda  = bc$x[which.max(bc$y)],
    profile = data.frame(lambda = bc$x, loglik = bc$y),
    grid    = range(lambda_grid)
  )
}

# Box-Cox with the log special case when lambda is essentially zero. Standard
# (y^lambda - 1)/lambda parameterisation keeps the transform continuous at
# lambda = 0.
apply_boxcox <- function(y, lambda) {
  if (abs(lambda) < 1e-4) log(y) else (y^lambda - 1) / lambda
}

# Shapiro-Wilk on residuals. Sub-sample if n exceeds SHAPIRO_SUBSAMPLE so the
# test is not overpowered into rejecting trivial deviations.
shapiro_safe <- function(resid_vec) {
  resid_vec <- resid_vec[is.finite(resid_vec)]
  n <- length(resid_vec)
  if (n > SHAPIRO_SUBSAMPLE) {
    set.seed(SHAPIRO_SEED)
    resid_vec <- sample(resid_vec, SHAPIRO_SUBSAMPLE)
  }
  st <- shapiro.test(resid_vec)
  list(W = unname(st$statistic), p = st$p.value, n_used = length(resid_vec))
}

# Levene's test of homoscedasticity across the 6 potencies. Returns F and p.
levene_safe <- function(resid_vec, group_vec) {
  ok <- is.finite(resid_vec) & !is.na(group_vec)
  lv <- car::leveneTest(resid_vec[ok] ~ factor(group_vec[ok]))
  list(F = lv[1, "F value"], p = lv[1, "Pr(>F)"])
}


#===== FIT ONE (VARIABLE x TRANSFORMATION) ==================================

# For each transform: produce a frame with residuals + fitted + potency, plus
# the Shapiro / Levene summary and (for Box-Cox) the lambda. Caller assembles
# the plots and summary CSV from the returned list.
fit_one <- function(var, transform_name, df_in) {
  # Keep only rows with a non-missing response. Box-Cox / INT need to know n.
  df_sub <- df_in[!is.na(df_in[[var]]), ]
  y_raw  <- df_sub[[var]]

  # Compute lambda first so the local binding is updated (using <<- inside
  # switch() would write to the lexical parent, not fit_one's frame, and
  # leave lambda as NA here).
  lambda      <- NA_real_
  bc_profile  <- NULL
  bc_grid     <- NULL
  if (transform_name == "boxcox") {
    bc_out     <- estimate_boxcox_lambda(y_raw, df_sub)
    lambda     <- bc_out$lambda
    bc_profile <- bc_out$profile
    bc_grid    <- bc_out$grid
    # Warn loudly if the optimum sits at (or essentially at) the search
    # boundary -- that's the symptom of a too-narrow lambda grid. The plot
    # we save next will make it obvious; the warning is so it does not
    # silently slip past in the console.
    if (abs(lambda - bc_grid[1]) < 1e-6 || abs(lambda - bc_grid[2]) < 1e-6) {
      warning("Box-Cox lambda landed on the search boundary (", lambda,
              ", grid [", bc_grid[1], ", ", bc_grid[2],
              "]). Widen lambda_grid in estimate_boxcox_lambda().",
              call. = FALSE)
    }
  }
  y_t <- switch(transform_name,
    "raw"    = y_raw,
    "boxcox" = apply_boxcox(y_raw, lambda),
    "int"    = int_transform(y_raw),
    stop("Unknown transform: ", transform_name)
  )

  df_sub$y_t <- y_t
  model      <- lm(y_t ~ potency * experiment_number, data = df_sub)

  resid_df <- data.frame(
    potency  = df_sub$potency,
    fitted   = fitted(model),
    residual = residuals(model)
  )

  sh <- shapiro_safe(resid_df$residual)
  lv <- levene_safe(resid_df$residual, resid_df$potency)

  list(
    var        = var,
    transform  = transform_name,
    lambda     = lambda,
    bc_profile = bc_profile,
    resid_df   = resid_df,
    shapiro_W  = sh$W, shapiro_p = sh$p, shapiro_n = sh$n_used,
    levene_F   = lv$F, levene_p  = lv$p,
    n          = nrow(resid_df)
  )
}


# Profile log-likelihood plot for Box-Cox. Shows the loglik curve over the
# search grid, with vertical lines at the chosen lambda and at lambda = 0/1
# for reference, plus a horizontal line at the 95% CI cutoff (max loglik
# minus qchisq(0.95, 1)/2 -- the usual likelihood-ratio interval).
plot_boxcox_profile <- function(fit) {
  prof <- fit$bc_profile
  if (is.null(prof)) return(NULL)
  ci_cutoff <- max(prof$loglik) - qchisq(0.95, df = 1) / 2
  ggplot(prof, aes(x = lambda, y = loglik)) +
    geom_line() +
    geom_vline(xintercept = fit$lambda, colour = "red", linewidth = 0.6) +
    geom_vline(xintercept = c(0, 1), colour = "grey60",
               linetype = "dashed", linewidth = 0.4) +
    geom_hline(yintercept = ci_cutoff, colour = "blue",
               linetype = "dotted", linewidth = 0.4) +
    labs(title    = paste0("Box-Cox profile log-likelihood: ", fit$var),
         subtitle = sprintf(paste0("Chosen lambda = %.3f (red).  Dashed = 0 ",
                                   "and 1.  Dotted = 95%% LR cutoff."),
                            fit$lambda),
         x = "lambda", y = "log-likelihood") +
    theme_bw(base_size = 10)
}


#===== PLOT BUILDERS ========================================================

# Row subtitle: Shapiro W / p, Levene F / p, lambda when present. Kept short
# so it fits above each row of the 3x3 overview without wrapping.
row_subtitle <- function(fit) {
  lam_part <- if (is.na(fit$lambda)) ""
              else sprintf("  lambda=%.2f", fit$lambda)
  sprintf("Shapiro W=%.3f p=%.2g | Levene F=%.2f p=%.2g%s",
          fit$shapiro_W, fit$shapiro_p,
          fit$levene_F,  fit$levene_p,
          lam_part)
}

# Three small diagnostic panels for one (variable, transformation) row of the
# overview plot: pooled Q-Q of residuals, residuals vs fitted, histogram.
panel_qq <- function(fit) {
  ggplot(fit$resid_df, aes(sample = residual)) +
    stat_qq(size = 0.4, alpha = 0.4) +
    stat_qq_line(colour = "red") +
    labs(x = "Theoretical", y = "Residual") +
    theme_bw(base_size = 9)
}

panel_resid_fitted <- function(fit) {
  ggplot(fit$resid_df, aes(x = fitted, y = residual)) +
    geom_point(size = 0.4, alpha = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_smooth(method = "loess", se = FALSE,
                colour = "red", linewidth = 0.5) +
    labs(x = "Fitted", y = "Residual") +
    theme_bw(base_size = 9)
}

panel_hist <- function(fit) {
  ggplot(fit$resid_df, aes(x = residual)) +
    geom_histogram(bins = 50, colour = "black",
                   fill = "grey80", linewidth = 0.2) +
    labs(x = "Residual", y = "Count") +
    theme_bw(base_size = 9)
}

# Per-potency Q-Q plot: 6 facets (3x2 portrait). Used once per variable and
# transformation so the user can see whether normality is a per-group issue
# or pooled-only.
plot_qq_by_potency <- function(fit) {
  ggplot(fit$resid_df, aes(sample = residual)) +
    stat_qq(size = 0.5, alpha = 0.5) +
    stat_qq_line(colour = "red") +
    facet_wrap(~ potency, nrow = 3, ncol = 2) +
    labs(title = paste0(fit$var, " -- ", fit$transform,
                        " (residuals by potency)"),
         subtitle = row_subtitle(fit),
         x = "Theoretical", y = "Residual") +
    theme_bw(base_size = 10)
}


#===== MAIN LOOP ============================================================

transform_names <- c("raw", "boxcox", "int")
transform_labels <- c(raw = "raw", boxcox = "Box-Cox", int = "INT")

summary_rows <- list()

for (var in response_vars) {

  cat("\n---- ", var, " ----\n", sep = "")

  fits <- lapply(transform_names, function(tn) fit_one(var, tn, df))
  names(fits) <- transform_names

  # Per-row plots, assembled as a 3 x 3 patchwork (rows = transforms,
  # cols = Q-Q | resid-vs-fitted | histogram). Each row gets a strip on the
  # left identifying the transform + its stats subtitle.
  rows <- lapply(transform_names, function(tn) {
    fit <- fits[[tn]]
    label <- paste0(transform_labels[[tn]],
                    if (tn == "boxcox") sprintf(" (lambda=%.2f)", fit$lambda)
                    else "")
    row_plot <- (panel_qq(fit) |
                 panel_resid_fitted(fit) |
                 panel_hist(fit)) +
      plot_annotation(title = label, subtitle = row_subtitle(fit),
                      theme = theme(plot.title = element_text(size = 10,
                                                              face = "bold"),
                                    plot.subtitle = element_text(size = 8)))
    wrap_elements(row_plot)
  })

  overview <- rows[[1]] / rows[[2]] / rows[[3]] +
    plot_annotation(
      title    = paste0("Transformation comparison: ", var),
      subtitle = paste0("Seedling level, all 6 potencies, residuals from ",
                        "lm(y ~ potency * experiment_number).  n = ",
                        fits[[1]]$n),
      theme    = theme(plot.title    = element_text(size = 12, face = "bold"),
                       plot.subtitle = element_text(size = 9))
    )

  ggsave(out_path(paste0("overview_", var), "png"),
         plot   = overview,
         width  = 24, height = 28, units = "cm", dpi = 300)

  # Per-transform per-potency Q-Q: 3 PNGs per variable.
  for (tn in transform_names) {
    ggsave(out_path(paste0("qq_by_potency_", var, "_", tn), "png"),
           plot   = plot_qq_by_potency(fits[[tn]]),
           width  = 16, height = 20, units = "cm", dpi = 300)
  }

  # Box-Cox profile log-likelihood plot. Lets the user see whether the chosen
  # lambda is a clean interior maximum or a boundary value (in which case the
  # search grid in estimate_boxcox_lambda needs to be widened).
  bc_profile_plot <- plot_boxcox_profile(fits[["boxcox"]])
  if (!is.null(bc_profile_plot)) {
    ggsave(out_path(paste0("boxcox_profile_", var), "png"),
           plot   = bc_profile_plot,
           width  = 16, height = 10, units = "cm", dpi = 300)
  }

  # Stash summary rows; bound after the loop into one CSV.
  for (tn in transform_names) {
    f <- fits[[tn]]
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      variable      = f$var,
      transformation = f$transform,
      lambda        = f$lambda,
      n             = f$n,
      shapiro_n     = f$shapiro_n,
      shapiro_W     = f$shapiro_W,
      shapiro_p     = f$shapiro_p,
      levene_F      = f$levene_F,
      levene_p      = f$levene_p
    )
    cat(sprintf("  %-8s  Shapiro W=%.3f p=%.2g  Levene F=%.2f p=%.2g%s\n",
                tn, f$shapiro_W, f$shapiro_p,
                f$levene_F, f$levene_p,
                if (is.na(f$lambda)) ""
                else sprintf("  lambda=%.2f", f$lambda)))
  }
}


#===== EXPORT SUMMARY =======================================================

summary_df <- do.call(rbind, summary_rows)
write.csv(summary_df, out_path("summary", "csv"), row.names = FALSE)

cat("\nWrote summary: ", out_path("summary", "csv"), "\n", sep = "")
cat("Output folder: ", out_dir, "\n", sep = "")
