##############################################################################
# File-Name: 11_l2_validation.R
# author: Tiago Ventura
# Purpose: validate the paper's ideology cutoffs against voter-file party
#          registration for the matched voters. 
# Data in: data/matched_voters_ideology_registration.csv
#          (cols: user_id, Parties_Description [raw registration],
#           0_norm [ideology score], imputation_party_final,
#           imputation_probability)
# Data out: Table 8 (output/tables/l2_cutoff_accuracy.tex)
##############################################################################

# basics: path, packages -----------------------------------------------------
data_path <- "data"
out_dir   <- "output/tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pacman::p_load(tidyverse, here, knitr, kableExtra)

csv_path <- file.path(data_path, "matched_voters_ideology_registration.csv")

# ---------------------------------------------------------------------------
# 0. Load. 
# ---------------------------------------------------------------------------
df <- read_csv(csv_path, show_col_types = FALSE) %>%
  rename(ideology = `0_norm`, party = Parties_Description) %>%
  filter(!is.na(ideology))

# quick sanity: mean ideology by party ---------------------------------------
df %>%
  group_by(party) %>%
  summarise(mean_ideo = mean(ideology), n = n(), .groups = "drop") %>%
  arrange(mean_ideo) %>%
  print(n = Inf)


OTHER_LAB <- "Other / Non-Partisan"
grp_lv_h  <- c("Democrat", "Republican", OTHER_LAB)

# collapse registration, then relabel the residual ---------------------------
dfh <- df %>%
  mutate(truth = case_when(party == "Democratic" ~ "Democrat",
                           party == "Republican" ~ "Republican",
                           TRUE                  ~ OTHER_LAB))

classify_cut_h <- function(ideo, cutoffs) {
  case_when(ideo <  cutoffs[[1]] ~ "Democrat",
            ideo >  cutoffs[[2]] ~ "Republican",
            TRUE                 ~ OTHER_LAB)
}

cutoff_specs_h <- list(
  "Single split at 0" = c(0, 0),
  "80th pct (paper)"  = c(-0.351, 0.045),
  "75th pct"          = c(-0.527, 0.393),
  "70th pct"          = c(-0.673, 0.637)
)

metrics_h <- imap_dfr(cutoff_specs_h, function(cuts, name) {
  pred  <- classify_cut_h(dfh$ideology, cuts)
  truth <- dfh$truth
  acc   <- mean(pred == truth)
  is_p  <- truth %in% c("Democrat", "Republican")
  lab_p <- is_p & pred %in% c("Democrat", "Republican")
  per <- map_dfr(grp_lv_h, function(g) {
    tp <- sum(pred == g & truth == g)
    fp <- sum(pred == g & truth != g)
    fn <- sum(pred != g & truth == g)
    prec <- tp / (tp + fp); rec <- tp / (tp + fn)
    tibble(group = g, n_group = sum(truth == g), precision = prec, recall = rec,
           f1 = 2 * prec * rec / (prec + rec))
  })
  per %>% mutate(
    spec = name, dem_cut = cuts[[1]], rep_cut = cuts[[2]],
    accuracy = acc, macro_f1 = mean(f1, na.rm = TRUE),
    n_partisan = sum(is_p),
    coverage_partisan  = sum(lab_p) / sum(is_p),
    precision_partisan = sum(pred[lab_p] == truth[lab_p]) / sum(lab_p))
}) %>%
  mutate(spec  = factor(spec,  levels = names(cutoff_specs_h)),
         group = factor(group, levels = grp_lv_h)) %>%
  arrange(spec, group)

cat(sprintf("\n== matched voters with an ideology score: n = %d ==\n", nrow(dfh)))
print(metrics_h %>% distinct(spec, dem_cut, rep_cut, accuracy, macro_f1,
                             n_partisan, coverage_partisan, precision_partisan))
print(metrics_h %>% select(spec, group, n_group, precision, recall, f1), n = Inf)

acc_tab_h <- metrics_h %>%
  transmute(
    spec_lab = sprintf("%s (%.3f / %+.3f)", as.character(spec), dem_cut, rep_cut),
    Group     = as.character(group),
    N         = n_group,
    Precision = round(precision, 3),
    Recall    = round(recall, 3),
    F1        = round(f1, 3),
    Accuracy  = round(accuracy, 3),
    `Partisan Precision` = round(precision_partisan, 3),
    `Partisan Coverage`  = round(coverage_partisan, 3))

write_csv(acc_tab_h, file.path(out_dir, "l2_cutoff_accuracy.csv"))
write_csv(metrics_h %>% distinct(spec, dem_cut, rep_cut, accuracy, macro_f1,
                                 n_partisan, coverage_partisan, precision_partisan),
          file.path(out_dir, "l2_cutoff_accuracy_summary.csv"))

# rle, NOT count(): count() re-sorts labels alphabetically and would silently
# mis-align pack_rows against a body ordered by the spec factor.
blocks_h  <- rle(as.character(acc_tab_h$spec_lab))

# "NaN" must never reach a published table: split-at-0 never leaves a voter
# unlabelled, so its Other/Non-Partisan precision is genuinely undefined (0/0).
# Render as an em-dash. Display only -- the CSV keeps the numeric values.
fmt3_h <- function(x) ifelse(is.na(x), "---", sprintf("%.3f", x))

kbl_body_h <- acc_tab_h %>%
  transmute(Group,
            N = formatC(N, format = "d", big.mark = ","),
            Precision = fmt3_h(Precision), Recall = fmt3_h(Recall),
            F1 = fmt3_h(F1), Accuracy = fmt3_h(Accuracy),
            `Partisan Precision` = fmt3_h(`Partisan Precision`),
            `Partisan Coverage`  = fmt3_h(`Partisan Coverage`))

# --- build the table, mirroring 10_yougov_validation.R ----------------------

acc_tbl_h <- kbl(kbl_body_h,
                 format = "latex",
                 booktabs = TRUE, linesep = "",
                 label = "l2_accuracy",
                 caption = "Accuracy of Twitter-based ideology cutoffs against voter-file party registration (matched voters)",
                 col.names = c("Group", "N", "Precision", "Recall", "F1",
                               "Accuracy", "Accuracy", "Coverage"),
                 align = "lrrrrrrr") %>%
  add_header_above(c(" " = 1, "Per registered party" = 4,
                     "All matched voters" = 1, "Partisans only" = 2)) %>%
  pack_rows(index = setNames(blocks_h$lengths, blocks_h$values)) %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  footnote(general_title = "Note: ", threeparttable = TRUE,
           general = paste(c(
             "Matched voters carrying a Twitter-based ideology score, scored against their voter-file party registration.",
             "Ideology is the paper's own score (identical to the values used throughout the analysis).",
             "Party is the voter's registration in the L2 file; 'Other / Non-Partisan' pools non-partisan, third-party and unknown registrations and has no counterpart in the survey table.",
             "Block headings give the Democratic and Republican cutoffs.",
             "Because a single split at zero has no middle band it never leaves a voter unlabelled."
           ), collapse = " "))

writeLines(as.character(acc_tbl_h), file.path(out_dir, "l2_cutoff_accuracy.tex"))
