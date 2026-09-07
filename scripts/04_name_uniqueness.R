##############################################################################
# File-Name: 04_name_uniqueness.R
# author: Tiago Ventura
# Purpose: compare the matched (unique-name) sample against duplicated-name
#          voters found on Twitter: demographics and offline isolation
#          percentiles. Measure = unweighted_offline_isolation, the paper's
#          headline offline isolation.
# Data in: data/matched_voters_segregation.json,
#          data/duplicated_voters_demographics.json,
#          data/duplicated_voters_offline_isolation.json
# Data out: Table 2 (output/tables/demographic_comparison.tex),
#           Table 3 (output/tables/offline_isolation_quantiles_matched_vs_dup.tex)
##############################################################################

# basics: paths, packages ----------------------------------------------------
data_path <- "data"
out_dir   <- "output/tables"

pacman::p_load(tidyverse, jsonlite, janitor, kableExtra, scales)

# JSONL reader
read_jsonl <- function(f) as_tibble(stream_in(file(f), verbose = FALSE))

# consistent group labels used in BOTH tables
lab_matched <- "Matched Voters (L2-Twitter)"
lab_dup     <- "Duplicated Matched Voters (L2-Twitter)"
FRIENDS_MIN <- -1


# Load MATCHED (canonical paper source) --------------------------------------
matched <- read_jsonl(file.path(data_path, "matched_voters_segregation.json"))
if (FRIENDS_MIN >= 0) matched <- matched %>% filter(n_friends_w_ideology > FRIENDS_MIN)

# Load DUP demographics ------------------------------------------------------
dup_demo <- read_jsonl(file.path(data_path, "duplicated_voters_demographics.json"))

# Load DUP offline isolation (all 50 states) ---------------------------------
dup_iso <- read_jsonl(file.path(data_path, "duplicated_voters_offline_isolation.json"))


# ==========================================================================
# TABLE 1 -- demographics ---------------------------------------------------
# ==========================================================================
summarise_group <- function(df, label){
  # Race from L2 EthnicGroups_EthnicGroup1Desc, 4-way grouping (Asian folded into
  # Other). nan / missing ethnicity -> NA, so it is dropped from the race
  # denominator (shares are among voters with a coded ethnicity and the four
  # categories sum to 100%). Matches the nan handling in
  # 03_descriptive_table.R.
  df %>%
    transmute(
      race   = case_when(
        EthnicGroups_EthnicGroup1Desc == "European"                          ~ "White",
        EthnicGroups_EthnicGroup1Desc == "Likely African-American"           ~ "Black",
        EthnicGroups_EthnicGroup1Desc == "Hispanic and Portuguese"           ~ "Hispanic",
        EthnicGroups_EthnicGroup1Desc %in% c("East and South Asian", "Other") ~ "Other",
        TRUE                                                                  ~ NA_character_
      ),
      age    = suppressWarnings(as.numeric(Voters_Age)),
      female = Voters_Gender == "F",
      dem    = Parties_Description == "Democratic"
    ) %>%
    summarise(
      group        = label,
      n            = n(),
      pct_white    = mean(race == "White",    na.rm = TRUE),
      pct_black    = mean(race == "Black",    na.rm = TRUE),
      pct_hispanic = mean(race == "Hispanic", na.rm = TRUE),
      pct_other    = mean(race == "Other",    na.rm = TRUE),
      mean_age     = mean(age,      na.rm = TRUE),
      pct_female   = mean(female,   na.rm = TRUE),
      pct_dem      = mean(dem,      na.rm = TRUE)
    )
}

demo_tab <- bind_rows(
  summarise_group(matched,  lab_matched),
  summarise_group(dup_demo, lab_dup)
)

print(demo_tab)

write_csv(demo_tab, file.path(out_dir, "demographic_comparison.csv"))

# variables in rows, samples in columns: format each cell, then pivot long -> wide
var_levels <- c("N", "% White", "% Black", "% Hispanic", "% Other",
                "Mean age", "% Female", "% Dem")

demo_disp <- demo_tab %>%
  transmute(
    Group        = group,
    N            = comma(n),
    `% White`    = sprintf("%.1f", 100 * pct_white),
    `% Black`    = sprintf("%.1f", 100 * pct_black),
    `% Hispanic` = sprintf("%.1f", 100 * pct_hispanic),
    `% Other`    = sprintf("%.1f", 100 * pct_other),
    `Mean age`   = sprintf("%.1f", mean_age),
    `% Female`   = sprintf("%.1f", 100 * pct_female),
    `% Dem`      = sprintf("%.1f", 100 * pct_dem)
  ) %>%
  pivot_longer(-Group, names_to = "Variable", values_to = "value") %>%
  mutate(Variable = factor(Variable, levels = var_levels)) %>%
  arrange(Variable) %>%
  pivot_wider(names_from = Group, values_from = value)

demo_cap <- paste0("Comparing Demographics: Unique Name vs Duplicated-Name Matches")

# escape = TRUE (default): % in the row labels is auto-escaped to \% for LaTeX
demo_disp %>%
  kbl(format = "latex", booktabs = TRUE, align = "lrr",
      col.names = c("", lab_matched, lab_dup),
      caption = demo_cap, label = "tab:name_uniqueness_demographics") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable(file = file.path(out_dir, "demographic_comparison.tex"))

message("saved -> ", out_dir, "/demographic_comparison.{csv,tex}")


# ==========================================================================
# TABLE 2 -- offline isolation percentiles ----------------------------------
# ==========================================================================
# matched isolation from the merged voter file; dup isolation from the Track C
# file. Both use unweighted_offline_isolation + imputed party (partisanship).
iso_matched <- matched %>%
  transmute(partisanship, offline_isolation = unweighted_offline_isolation,
            group = lab_matched)
iso_dup <- dup_iso %>%
  transmute(partisanship, offline_isolation = unweighted_offline_isolation,
            group = lab_dup)

group_levels <- c(lab_matched, lab_dup)
split_levels <- c("All voters", "Democrats", "Republicans")

# three party splits within each group
make_splits <- function(df){
  base <- df %>% filter(partisanship %in% c("Democrat", "Republican"))
  bind_rows(
    base %>% mutate(split = "All voters"),
    base %>% filter(partisanship == "Democrat")   %>% mutate(split = "Democrats"),
    base %>% filter(partisanship == "Republican") %>% mutate(split = "Republicans")
  )
}
allm <- bind_rows(make_splits(iso_matched), make_splits(iso_dup))

# quantiles -- mirrors get_sumstats_group() in the descriptive scripts
get_sumstats_group <- function(data, outcome){
  data %>%
    group_by(group, split) %>%
    summarize(mean = mean({{outcome}}, na.rm = TRUE),
              count = n(),
              quant01 = quantile({{outcome}}, probs = .01, na.rm = TRUE),
              quant10 = quantile({{outcome}}, probs = .10, na.rm = TRUE),
              quant25 = quantile({{outcome}}, probs = .25, na.rm = TRUE),
              quant50 = quantile({{outcome}}, probs = .50, na.rm = TRUE),
              quant75 = quantile({{outcome}}, probs = .75, na.rm = TRUE),
              quant90 = quantile({{outcome}}, probs = .90, na.rm = TRUE),
              quant99 = quantile({{outcome}}, probs = .99, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_longer(cols = c(mean, count, starts_with("quant")), names_to = "statistics")
}

des <- get_sumstats_group(allm, offline_isolation)

label_levels <- c("1st", "10th", "25th", "Median", "75th", "90th", "99th")
quant <- des %>%
  filter(str_detect(statistics, "quant")) %>%
  mutate(label = case_when(statistics == "quant01" ~ "1st",
                           statistics == "quant10" ~ "10th",
                           statistics == "quant25" ~ "25th",
                           statistics == "quant50" ~ "Median",
                           statistics == "quant75" ~ "75th",
                           statistics == "quant90" ~ "90th",
                           statistics == "quant99" ~ "99th"),
         label = factor(label, levels = label_levels))

# per-row N (D/R count in each split) + per-group ego count
n_row <- des %>% filter(statistics == "count") %>% select(group, split, n = value)
n_group <- n_row %>% filter(split == "All voters") %>% select(group, n_egos = n)

wide <- quant %>%
  select(group, split, label, value) %>%
  pivot_wider(names_from = label, values_from = value) %>%
  left_join(n_row, by = c("group", "split")) %>%
  mutate(group = factor(group, levels = group_levels),
         split = factor(split, levels = split_levels)) %>%
  arrange(group, split)

write_csv(wide, file.path(out_dir, "offline_isolation_quantiles_matched_vs_dup.csv"))

iso_disp <- wide %>%
  transmute(group,
            Voters = as.character(split),
            N = comma(n),
            across(all_of(label_levels), ~ sprintf("%.3f", .x)))
iso_body <- iso_disp %>% arrange(group) %>% select(-group)

n_matched <- n_group %>% filter(group == lab_matched) %>% pull(n_egos)
n_dup     <- n_group %>% filter(group == lab_dup)     %>% pull(n_egos)

iso_head <- c("Voters", label_levels)
iso_cap <- paste0("Offline partisan isolation: unique name matched sample vs. duplicated-name ",
                  "voters on Twitter.")

iso_body %>%
  select(-N) %>%
  kbl(format = "latex", booktabs = TRUE, linesep = "", align = "lrrrrrrr",
      col.names = iso_head, caption = iso_cap, label = "tab:iso_matched_vs_dup") %>%
  add_header_above(c(" " = 1, "Percentile of offline isolation" = 7))  %>%
  pack_rows(sprintf("%s (N = %s egos)", lab_matched, comma(n_matched)), 1, 3) %>%
  pack_rows(sprintf("%s (N = %s egos)", lab_dup, comma(n_dup)), 4, 6) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  save_kable(file = file.path(out_dir, "offline_isolation_quantiles_matched_vs_dup.tex"))

print(iso_body, n = 6)
