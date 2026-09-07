# Replication Materials: Offline partisan segregation exceeds online echo chambers in the United States: Evidence from 1 million American voters

Replication materials for *Offline partisan segregation exceeds online echo chambers in the United States: Evidence from 1 million American voters*, authored by Megan A. Brown, Tiago Ventura, Joshua A. Tucker and Jonathan Nagler.

**Abstract:**

Social media is often blamed for the creation of echo chambers. However, these claims fail to consider the prevalence of *offline* echo chambers resulting from high levels of geographic partisan segregation in the United States. Our article empirically assesses these online versus offline dynamics by linking a novel dataset of voters' offline geographic partisan segregation extracted from publicly available voter files for 180 million US voters with their online network segregation on Twitter. We investigate offline and online partisan segregation using measures of geographical and online isolation of every matched voter-twitter user to their co-partisans offline and online. Our results show that while social media users do tend to form politically homogeneous online networks, these levels of partisan sorting are significantly lower than those found in their geographical offline settings. Thus social media is providing users with networks that are more heterogeneous than their offline environments. We also find that Democrats are more ideologically isolated than Republicans in both settings.

## Tutorial

This README file provides an overview of the replication materials for the article.

- The R code used in the article and supplementary information can be found in the folder **scripts**.
- The datasets used by the scripts live in the folder **data**.
- All figures and tables produced by the scripts are written to the folder **output** (`output/figs` for figures, `output/tables` for tables).

Every script is standalone and is run from the repository root, for example:

```bash
Rscript scripts/01_main_results.R
```

The scripts share three conventions. Packages are loaded through `pacman::p_load()`, which installs anything missing on first run. Each script sources `scripts/utils.R` where it needs the shared ggplot theme and helpers. Figures are saved by their number in the paper: `fig_1a`, `fig_1b`, `fig_2a`, `fig_2b` and `fig_3` for the main article, `sm_fig_4a` through `sm_fig_14` for the supplementary information, each as both .pdf and .png.

Two notes before running:

- `05_census_metro.R` pulls census tract populations and geometries from the Census API at runtime (tidycensus and tigris). It needs internet access, and a Census API key is recommended (`tidycensus::census_api_key("YOUR_KEY", install = TRUE)`; keyless requests work but are rate-limited). Its tract-to-metro crosswalk (`data/tracts_merge.csv`) is already included, so `08_online_degree_cutoffs.R` runs without it.
- The main voter-level files are large. Loading `matched_voters_segregation.json` takes several minutes and peak memory well above the file size; plan for a machine with at least 32 GB of RAM for scripts 01, 03, 04 and 09.

## Code

- `utils.R`: user-defined functions shared across scripts: the ggplot theme and fonts, party colors, `save_function()` (writes every figure to `output/figs`), state-name and state-FIPS helpers.
- `01_main_results.R`: descriptive analysis of online and offline isolation on the main specification (probabilistic partisanship assignment, 1000 nearest neighbors). It produces Figures 1 and 2 in the main article and Figures 5, 6 and 7 in the supplementary information, as well as the top panels of SI Figures 8 and 9.
- `02_subgroup_differences.R`: median online-offline isolation differences across socio-demographic subgroups, with bootstrapped 99% confidence intervals. It produces Figure 3 in the main article.
- `03_descriptive_table.R`: demographics of the Twitter-L2 panel against the ANES 2020 and Pew NPORS 2021 benchmarks. It produces Table 1 in the main article.
- `04_name_uniqueness.R`: compares the matched (unique-name) sample with duplicated-name voters found on Twitter. It produces Tables 2 and 3 in the main article.
- `05_census_metro.R`: online and offline isolation across metropolitan areas and census tract population density. It produces SI Figure 4 and writes the tract-to-metro crosswalk `data/tracts_merge.csv`.
- `06_robustness_500nn.R`: offline isolation with 500 nearest neighbors. It produces the bottom panel of SI Figure 8.
- `07_robustness_discrete.R`: offline isolation with discrete (rather than probabilistic) partisanship assignment. It produces the bottom panel of SI Figure 9.
- `08_online_degree_cutoffs.R`: online degree distribution, mean isolation by degree, online isolation under different minimum-friends cutoffs, and online degree across subgroups. It produces SI Figures 10, 11 and 12 and SI Table 5.
- `09_isolation_by_unit.R`: offline isolation percentiles under six neighborhood definitions (1000 nearest neighbors, precinct, census tract, school district, county, congressional district). It produces SI Table 6.
- `10_yougov_validation.R`: validates the ideology cutoffs against self-reported party identification in the YouGov donated-Twitter panel. It produces SI Table 7.
- `11_l2_validation.R`: validates the ideology cutoffs against voter-file party registration for the matched voters. It produces SI Table 8.
- `12_engagement_networks.R`: online isolation measured on the follow network against five engagement networks (all engagement, retweets, quote tweets, replies, mentions). It produces SI Figure 14.

Two SI items are not produced by this package: SI Figures 1-3 are survey questionnaire screenshots, and SI Figure 13 (neighborhood vs. acquaintance/coworker isolation in the YouGov survey) was produced by a coauthor from the survey microdata; that code is available on request.

## Data

The de-identified versions of the voter-level datasets will be added to the **data** folder before final submission. The aggregate-level files are already included.

Voter-level datasets (to be added, de-identified):

- `matched_voters_segregation.json`: the core analysis file. One row per matched voter-Twitter user with demographics, imputed partisanship, online isolation/exposure from the follow network, and offline isolation/exposure from the 1000 nearest neighbors (probabilistic assignment, the paper's main specification).
- `matched_voters_segregation_500nn.json`: same structure, offline measures computed on the 500 nearest neighbors.
- `matched_voters_segregation_discrete.json`: same structure, offline measures computed under discrete partisanship assignment.
- `duplicated_voters_demographics.json`: L2 covariates for the duplicated-name voters found on Twitter (the comparison group in Table 2).
- `duplicated_voters_offline_isolation.json`: offline isolation for the duplicated-name voters (Table 3).
- `offline_isolation_nn1000.json`: 1000-nearest-neighbor offline isolation for matched voters under the roster-wide imputation (the baseline row of SI Table 6).
- `offline_isolation_by_unit.json`: offline isolation for matched voters at each administrative unit, one row per voter and unit (SI Table 6).
- `matched_voters_ideology_registration.csv`: matched voters carrying a Twitter-based ideology score, with their voter-file party registration (SI Table 8).
- `yougov_validation_panel.csv`: the YouGov donated-Twitter panel with the paper's ideology score and party identification across eleven survey waves (SI Table 7).

Aggregate datasets (included):

- `blue_red_purple_states.csv`: 2020 two-party vote share by state and the resulting blue/red/purple classification.
- `anes_data_summary.csv`: demographic means and standard errors from the ANES 2020 (Table 1 benchmark).
- `weighted_pew_data_summary.csv`: weighted demographic means and standard errors from the Pew NPORS 2021 (Table 1 benchmark).
- `tracts_merge.csv`: census tract to metropolitan area crosswalk (2010 CBSAs), written by `05_census_metro.R` and read by `08_online_degree_cutoffs.R`.
- `engagement_network_percentiles.csv`: percentiles of online isolation on the follow network and on the five engagement networks, one row per percentile (SI Figure 14).

## Computational Infrastructure

The scripts were last run on a Linux HPC cluster. Scripts 01, 03, 04 and 09 load voter-level files with millions of rows; 32 GB of RAM or more is recommended for those. Here is the output of `sessionInfo()` at the time this project was last run:

```
R version 4.6.1 (2026-06-24)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 26.04 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.32.so;  LAPACK version 3.12.0

locale:
[1] C

time zone: America/New_York
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] sf_1.1-1          tigris_2.2.1      tidycensus_1.8.1  knitr_1.51
 [5] boot_1.3-32       scales_1.4.0      kableExtra_1.4.1  jsonlite_2.0.0
 [9] rjson_0.2.23      ggtext_0.1.2      janitor_2.2.1     wesanderson_0.3.7
[13] ggridges_0.5.7    here_1.0.2        lubridate_1.9.5   forcats_1.0.1
[17] stringr_1.6.0     dplyr_1.2.1       purrr_1.2.2       readr_2.2.0
[21] tidyr_1.3.2       tibble_3.3.1      ggplot2_4.0.3     tidyverse_2.0.0
[25] pacman_0.5.1
```
