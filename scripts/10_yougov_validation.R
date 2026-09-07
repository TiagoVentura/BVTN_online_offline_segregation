##############################################################################
# File-Name: 10_yougov_validation.R
# author: Tiago Ventura
# Purpose: validate the paper's ideology cutoffs against self-reported party
#          identification in the YouGov donated-Twitter panel. The panel
#          carries the paper's own score (pablo_2019_norm), so the cutoffs
#          apply directly with no rescaling.
# Data in: data/yougov_validation_panel.csv
# Data out: Table 7 (output/tables/yougov_cutoff_accuracy.tex)
##############################################################################

# basics: path, packages -----------------------------------------------------
data_path <- "data"
out_dir   <- "output/tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pacman::p_load(tidyverse, here, knitr, kableExtra)

# YouGov panel (user_id + pablo_2019_norm + all 11 pid waves)
exp_path <- file.path(data_path, "yougov_validation_panel.csv")

groups <- c("Democrat", "Republican", "Independent")

# same four schemes as 11_l2_validation.R (Dem cut, Rep cut) ----------------
cutoff_specs <- list(
  "Single split at 0" = c(0, 0),
  "80th pct (paper)"  = c(-0.351, 0.045),
  "75th pct"          = c(-0.527, 0.393),
  "70th pct"          = c(-0.673, 0.637)
)

# classify a paper-oriented score under a (Dem cut, Rep cut) scheme ----------
classify_cut <- function(ideo, cutoffs) {
  case_when(ideo <  cutoffs[[1]] ~ "Democrat",
            ideo >  cutoffs[[2]] ~ "Republican",
            TRUE                 ~ "Independent")
}

# 3-class metrics: overall accuracy + per-group precision / recall / F1 ------
class_metrics3 <- function(pred, truth) {
  acc <- mean(pred == truth)
  per <- map_dfr(groups, function(g) {
    tp <- sum(pred == g & truth == g)
    fp <- sum(pred == g & truth != g)
    fn <- sum(pred != g & truth == g)
    prec <- tp / (tp + fp)             # NaN if the class is never predicted
    rec  <- tp / (tp + fn)
    tibble(group = g, precision = prec, recall = rec,
           f1 = 2 * prec * rec / (prec + rec), tp = tp, fp = fp, fn = fn)
  })
  per %>% mutate(accuracy = acc, macro_f1 = mean(f1, na.rm = TRUE))
}

## -- Build the sample on the PAPER's own scale ------------------------------
expd <- read_csv(exp_path, show_col_types = FALSE,
                 col_types = cols(user_id = col_character(),
                                  nagler_id = col_character(),
                                  .default = col_guess())) %>%
  mutate(across(starts_with("pid7_w"), as.numeric))

# modal party across every wave the respondent answered; unresolved ties out --
pid_modal <- expd %>%
  select(nagler_id, starts_with("pid7_w")) %>%
  pivot_longer(starts_with("pid7_w"), names_to = "wave", values_to = "pid7") %>%
  filter(!is.na(pid7), pid7 >= 1, pid7 <= 7) %>%      # code 8 ("not sure") out
  mutate(g = case_when(pid7 <= 3 ~ "Democrat",
                       pid7 == 4 ~ "Independent",
                       TRUE      ~ "Republican")) %>%
  count(nagler_id, g, name = "n_waves") %>%
  group_by(nagler_id) %>%
  slice_max(n_waves, n = 1, with_ties = TRUE) %>%
  filter(n() == 1) %>%                                # a tie = no modal party
  ungroup() %>%
  select(nagler_id, pid = g)

lab_paper <- expd %>%
  select(user_id, nagler_id, pablo_paper = pablo_2019_norm, pablo_2023) %>%
  filter(!is.na(pablo_paper)) %>%
  distinct(user_id, .keep_all = TRUE) %>%             # 5 accounts carry 2-3 ids
  inner_join(pid_modal, by = "nagler_id")

cat(sprintf(
  "\n== sample ==\n  respondents in panel          : %d\n  with a 2019 (paper-scale) score: %d\n  unique accounts               : %d\n  + modal pid across 11 waves   : %d\n",
  nrow(expd), sum(!is.na(expd$pablo_2019_norm)),
  n_distinct(expd$user_id[!is.na(expd$pablo_2019_norm)]), nrow(lab_paper)))
print(lab_paper %>% count(pid) %>% mutate(share = round(n / sum(n), 3)))

# orientation sanity: paper scale -> Dem LOWEST, Rep HIGHEST (no flip needed)
lab_paper %>%
  group_by(pid) %>%
  summarise(mean_pablo_paper = mean(pablo_paper), n = n(), .groups = "drop") %>%
  arrange(mean_pablo_paper) %>%
  print()

## -- Metrics for all four schemes -------------------------------------------
truth_paper <- lab_paper$pid

cutoff_metrics <- imap_dfr(cutoff_specs, function(cuts, name) {
  pred <- classify_cut(lab_paper$pablo_paper, cuts)
  # 3-class metrics
  m <- class_metrics3(pred, truth_paper) %>%
    mutate(spec = name, dem_cut = cuts[[1]], rep_cut = cuts[[2]])
  # partisan-only view: among respondents who call themselves D or R, how many
  # does the scheme LABEL (coverage) and how many of those are right (precision)
  is_p   <- truth_paper %in% c("Democrat", "Republican")
  lab_p  <- is_p & pred %in% c("Democrat", "Republican")
  m %>% mutate(
    n_partisan        = sum(is_p),
    coverage_partisan = sum(lab_p) / sum(is_p),
    precision_partisan = sum(pred[lab_p] == truth_paper[lab_p]) / sum(lab_p))
}) %>%
  mutate(spec = factor(spec, levels = names(cutoff_specs))) %>%
  left_join(count(lab_paper, pid, name = "n_group"), by = c("group" = "pid")) %>%
  select(spec, dem_cut, rep_cut, group, n_group, precision, recall, f1,
         accuracy, macro_f1, n_partisan, coverage_partisan, precision_partisan,
         tp, fp, fn)

## PAPER TABLE ---------------------------------------------------------
grp_lv <- c("Democrat", "Republican", "Independent")

acc_tab <- cutoff_metrics %>%
  mutate(group = factor(group, levels = grp_lv)) %>%
  arrange(spec, group) %>%
  transmute(
    # as.character(): sprintf("%s", <factor>) can emit the integer level code
    spec_lab = sprintf("%s (%.3f / %+.3f)", as.character(spec), dem_cut, rep_cut),
    Group     = as.character(group),
    N         = n_group,
    Precision = round(precision, 3),
    Recall    = round(recall, 3),
    F1        = round(f1, 3),
    Accuracy  = round(accuracy, 3),
    `Partisan Precision` = round(precision_partisan, 3),
    `Partisan Coverage`  = round(coverage_partisan, 3))

write_csv(acc_tab, file.path(out_dir, "yougov_cutoff_accuracy.csv"))

# compact spec-level summary (handy for quoting a single number in prose) ----
acc_summary <- cutoff_metrics %>%
  distinct(spec, dem_cut, rep_cut, accuracy, macro_f1,
           n_partisan, coverage_partisan, precision_partisan) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
write_csv(acc_summary, file.path(out_dir, "yougov_cutoff_accuracy_summary.csv"))
cat("\n== spec-level summary ==\n"); print(acc_summary)

blocks <- rle(as.character(acc_tab$spec_lab))

fmt3 <- function(x) ifelse(is.na(x), "---", sprintf("%.3f", x))

kbl_body <- acc_tab %>%
  transmute(Group,
            N = formatC(N, format = "d", big.mark = ","),
            Precision = fmt3(Precision), Recall = fmt3(Recall), F1 = fmt3(F1),
            Accuracy  = fmt3(Accuracy),
            `Partisan Precision` = fmt3(`Partisan Precision`),
            `Partisan Coverage`  = fmt3(`Partisan Coverage`))

# Gen table
acc_tbl <- kbl(kbl_body,
               format = "latex",
               booktabs = TRUE, linesep = "",
               label = "yougov_accuracy",
               caption = "Accuracy of Twitter-based ideology cutoffs against self-reported party identification (YouGov donated-Twitter panel)",
               col.names = c("Group", "N", "Precision", "Recall", "F1",
                             "Accuracy", "Accuracy", "Coverage"),
               align = "lrrrrrrr") %>%
  add_header_above(c(" " = 1, "Per stated party ID" = 4,
                     "All respondents" = 1, "Partisans only" = 2)) %>%
  pack_rows(index = setNames(blocks$lengths, blocks$values)) %>%
  kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
  footnote(general_title = "Note: ", threeparttable = TRUE,
           general = paste(c(
             "Nationally representative YouGov panel of respondents who donated their Twitter data.",
             "Ideology is the paper's own score (identical to the values used throughout the analysis).",
             "Party ID is the respondent's modal answer across the eleven panel waves (1-3 Democrat, 4 Independent, 5-7 Republican).",
             "Block headings give the Democratic and Republican cutoffs.",
             "Because a single split at zero has no middle band it never predicts Independent."
           ), collapse = " "))

writeLines(as.character(acc_tbl), file.path(out_dir, "yougov_cutoff_accuracy.tex"))
