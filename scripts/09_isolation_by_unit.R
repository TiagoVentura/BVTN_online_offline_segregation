##############################################################################
# File-Name: 09_isolation_by_unit.R
# author: Tiago Ventura
# Purpose: table of OFFLINE PARTISAN ISOLATION quantiles (the same
#          percentiles reported in the paper: 1st/10th/25th/Median/75th/90th/99th) under each
#          neighborhood definition: 1000 nearest neighbors (the paper's baseline), precinct,
#          census tract, school district, county, and congressional district. Split by party,
#          with a column for the range of neighborhood size N. kableExtra table out.
# Data in: data/offline_isolation_nn1000.json (1000-NN)
#          data/offline_isolation_by_unit.json (admin units)
#          data/matched_voters_segregation.json (for the n_friends_w_ideology>9 sample)
# Data out: Table 6 (output/tables/offline_isolation_quantiles_by_unit.{tex,csv})
##############################################################################

# basics: path, packages and utils ------------------------------------------------------------

# path for the data
data_path <- "data"
out_dir   <- "output/tables"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# packages
pacman::p_load(tidyverse, here, janitor, ggtext, rjson, jsonlite, kableExtra, scales)

# source graph + shared utils
source("scripts/utils.R")

# JSONL reader
read_jsonl <- function(f) as_tibble(stream_in(file(f), verbose = FALSE))

# the paper's online-quality sample filter: keep egos with >9 friends with ideology.
# set to 0 to report on all matched D/R egos instead.
FRIENDS_MIN <- 9


# Sample: the paper's >9-friends egos --------------------------------------
# same merged file + same filter as 01_main_results.R, read only to define which
# egos enter the table (n_friends_w_ideology is imputation-independent).
temp_path <- file.path(data_path, "matched_voters_segregation.json")
stopifnot(file.exists(temp_path))
keep_ids <- as_tibble(stream_in(file(temp_path), verbose = FALSE)) %>%
  filter(n_friends_w_ideology > FRIENDS_MIN) %>%
  pull(LALVOTERID)


# Load measures (all 50 states) --------------------------------------------
# 1000-NN baseline -- the paper reports the UNWEIGHTED isolation, and the admin units are
# inherently unweighted (no distance), so we use unweighted_offline_isolation for an apples-
# to-apples comparison. Same roster-wide imputation underlies every measure.
nn <- read_jsonl(file.path(data_path, "offline_isolation_nn1000.json")) %>%
  transmute(LALVOTERID, partisanship,
            offline_isolation = unweighted_offline_isolation,
            unit = "nn1000", unit_n = 1000L)

# admin-unit measures (one row per ego x unit)
units <- read_jsonl(file.path(data_path, "offline_isolation_by_unit.json")) %>%
  select(LALVOTERID, partisanship, offline_isolation, unit, unit_n)

# stack baseline + units; restrict to the paper's sample and to D/R egos
allm <- bind_rows(nn, units) %>%
  filter(LALVOTERID %in% keep_ids, partisanship %in% c("Democrat", "Republican"))


# Quantiles ----------------------------------------------------------------
# mirrors get_sumstats_group() in the descriptive scripts, with the unit dimension added.
get_sumstats_group <- function(data, outcome){
  data %>%
    group_by(partisanship, unit) %>%
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

des <- get_sumstats_group(allm, offline_isolation) %>%
  filter(!is.na(partisanship))

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


# Assemble the table -------------------------------------------------------
# neighborhood-size range (number of D/R members) per unit
nrange <- allm %>%
  group_by(unit) %>%
  summarise(n_min = min(unit_n, na.rm = TRUE), n_max = max(unit_n, na.rm = TRUE),
            .groups = "drop")

# ego count per party (from the nn1000 row -- the full sample for that party)
n_ego <- des %>%
  filter(statistics == "count", unit == "nn1000") %>%
  select(partisanship, n_egos = value)

unit_levels <- c("nn1000", "precinct", "tract", "school", "county", "cd")
unit_labs   <- c(nn1000 = "Nearest neighbors (1000)", precinct = "Precinct",
                 tract = "Census tract", school = "School district",
                 county = "County", cd = "Congressional district")

wide <- quant %>%
  select(partisanship, unit, label, value) %>%
  pivot_wider(names_from = label, values_from = value) %>%
  left_join(nrange, by = "unit") %>%
  mutate(unit = factor(unit, levels = unit_levels)) %>%
  arrange(partisanship, unit)

# raw numbers out (full precision)
write_csv(wide, file.path(out_dir, "offline_isolation_quantiles_by_unit.csv"))

# formatted display rows
disp <- wide %>%
  transmute(partisanship,
            Measure = recode(as.character(unit), !!!unit_labs),
            `N range` = if_else(unit == "nn1000", "1,000",
                                paste0(comma(n_min), "–", comma(n_max))),
            across(all_of(label_levels), ~ sprintf("%.3f", .x)))

dem  <- disp %>% filter(partisanship == "Democrat")   %>% select(-partisanship)
rep  <- disp %>% filter(partisanship == "Republican") %>% select(-partisanship)
body <- bind_rows(dem, rep)

n_dem <- n_ego %>% filter(partisanship == "Democrat")   %>% pull(n_egos)
n_rep <- n_ego %>% filter(partisanship == "Republican") %>% pull(n_egos)

col_head <- c("Measure", "N range", label_levels)
cap <- paste0("Offline partisan isolation by neighborhood definition. Cells are ",
              "percentiles of ego-level offline isolation; N range is the span of neighborhood ",
              "sizes (number of D/R voters). Sample: matched voters with >",
              FRIENDS_MIN, " friends with ideology.")


# kableExtra: LaTeX --------------------------------------------------------
kbl_tex <- body %>%
  kbl(format = "latex", booktabs = TRUE, linesep = "", align = "lrrrrrrrr",
      col.names = col_head, caption = cap, label = "tab:iso_by_unit") %>%
  add_header_above(c(" " = 2, "Percentile of offline isolation" = 7)) %>%
  pack_rows(sprintf("Democrats (N = %s egos)", comma(n_dem)), 1, 6) %>%
  pack_rows(sprintf("Republicans (N = %s egos)", comma(n_rep)), 7, 12) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))
save_kable(kbl_tex, file = file.path(out_dir, "offline_isolation_quantiles_by_unit.tex"))

print(body, n = 12)
