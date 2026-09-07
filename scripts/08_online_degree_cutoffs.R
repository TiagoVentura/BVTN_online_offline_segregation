##############################################################################
# File-Name: 08_online_degree_cutoffs.R
# author: Tiago Ventura
# Purpose: online degree distribution, mean isolation by degree, online
#          isolation under different minimum-friends cutoffs, and online
#          degree across subgroups
# Data in: data/matched_voters_segregation.json,
#          data/blue_red_purple_states.csv, data/tracts_merge.csv
# Data out: SI Figures 10-12; Table 5
#           (output/tables/degree_heterogeneity_subgroups.tex)
##############################################################################

# basics: path, packages and utils ------------------------------------------------------------

# path for the data
data_path <- "data"

# packages
pacman::p_load(tidyverse, here, ggridges, wesanderson, janitor, ggtext, rjson, jsonlite)

# source graph
source("scripts/utils.R")

# Combined online + offline data ------------------------------------------
temp_path <- file.path(data_path, "matched_voters_segregation.json")
actual_matches_iso_expo_off <- as_tibble(stream_in(file(temp_path)))

# All the online analyses keep only egos with n_friends_w_ideology > 9.
FRIENDS_MIN <- 9

median =
  actual_matches_iso_expo_off %>%
  # removing people with less than 10 friends with ideology
  filter(n_friends_w_ideology>1) %>%
  group_by(partisanship) %>%
  summarise(m=median(n_friends_w_ideology,na.rm=TRUE))

# Log-log degree distribution (R1.3a) -------------------------------------
# Degree = n_friends_w_ideology (# followed accounts with an
# ideology score). Count voters at each degree value, take log10 of the count,
# and plot it as a continuous histogram with degree on a log x-axis.
# Dashed lines mark the party medians.
deg_counts <- actual_matches_iso_expo_off %>%
  filter(n_friends_w_ideology > 1,
         !is.na(partisanship), !is.na(n_friends_w_ideology)) %>%
  count(partisanship, n_friends_w_ideology, name = "count") %>%
  mutate(log_count = log10(count))



deg_counts %>%
  ggplot(aes(x = n_friends_w_ideology, y = log_count,
             fill = partisanship, color = partisanship)) +
  geom_point(position = "identity", alpha = .3) +
  geom_vline(data = median,
             aes(xintercept = m, color = partisanship),
             linetype = "dashed", size = 1, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_continuous(labels = function(x) format(round(10^x), big.mark = ",",
                                                 scientific = FALSE, trim = TRUE)) +
  annotation_logticks(sides = "b") +
  labs(y = "Counts of Matched Voters (log scale)",
       x = "Friends with Ideology Scores (log scale)",
       caption = "
       Dashed lines mark the partisan medians (Democrats 69, Republicans 49).") +
  ggtitle("") +
  scale_fill_manual(values = c(dem, rep), name = "") +
  scale_color_manual(values = c(dem, rep), name = "") +
  theme(legend.position = "bottom",
        panel.grid = element_blank()) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

save_function("sm_fig_10.png")
save_function("sm_fig_10.pdf")

# histograms with mean
hist <- actual_matches_iso_expo_off %>%
  # removing people with less than 10 friends with ideology
  filter(n_friends_w_ideology>1) %>%
  select(user_id, partisanship, online_isolation, n_friends_w_ideology) %>%
  pivot_longer(cols = -c(user_id, partisanship, online_isolation),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  mutate_if(is.character, ~str_replace(.x, "ideology", "Friends with Ideology Scores")) %>%
  group_by(values, partisanship) %>%
  summarise(
    n             = n(),
    mean_iso      = mean(online_isolation, na.rm=TRUE),
    sd_iso        = sd(online_isolation, na.rm=TRUE),
    se_iso        = sd_iso / sqrt(n),
    ci_lower      = mean_iso - qt(0.975, df = n - 1) * se_iso,
    ci_upper      = mean_iso + qt(0.975, df = n - 1) * se_iso) %>%
  ungroup()


# graph with logs
ggplot(hist,
       aes(y=mean_iso,
           x=log(values),
           ymin=ci_lower,
           ymax=ci_upper, fill=partisanship, color=partisanship)) +
  geom_point(alpha = 1, position="identity") +
  geom_errorbar(alpha=.1) +
  labs(y="Online Isolation", x="Log of the Number of Friends with Ideology Scores ",
       caption="Mean values presented together with 95% confidence intervals") +
  ggtitle("") +
  scale_fill_manual(values=c(dem, rep), name="") +
  scale_color_manual(values=c(dem, rep), name="")  +
  theme(legend.position = "bottom",
        panel.grid =  element_blank()) +
  guides(alpha="none",  fill = guide_legend(override.aes = list(alpha = 1))) +
  ylim(0,1)


save_function("sm_fig_11.png")
save_function("sm_fig_11.pdf")


# Online isolation under different cutoffs ---------------------------------

actual_matches_iso_expo_off_1 <-  actual_matches_iso_expo_off %>%
  select(everything(),
         -offline_exposure,
         -offline_isolation,
         # use the unweighted
         offline_exposure=unweighted_offline_exposure,
         offline_isolation=unweighted_offline_isolation
  )  %>%
  # removing people with less than 10 friends with ideology
  filter(n_friends_w_ideology>1)


# removing 5
actual_matches_iso_expo_off_5 <-  actual_matches_iso_expo_off %>%
  select(everything(),
         -offline_exposure,
         -offline_isolation,
         # use the unweighted
         offline_exposure=unweighted_offline_exposure,
         offline_isolation=unweighted_offline_isolation
  )  %>%
  # removing people with less than 10 friends with ideology
  filter(n_friends_w_ideology>5)


# removing 9
actual_matches_iso_expo_off_9 <-  actual_matches_iso_expo_off %>%
  select(everything(),
         -offline_exposure,
         -offline_isolation,
         # use the unweighted
         offline_exposure=unweighted_offline_exposure,
         offline_isolation=unweighted_offline_isolation
  )  %>%
  # removing people with less than 10 friends with ideology
  filter(n_friends_w_ideology>9)



# filter to republicans and democrats
get_sumstats_group <- function(data, group, outcome){
  data %>%
    group_by({{group}}) %>%
    summarize(mean=mean({{outcome}}, na.rm=TRUE),
              count=n(),
              quant01=quantile({{outcome}}, probs=.01, na.rm=TRUE),
              quant10=quantile({{outcome}}, probs=.10, na.rm=TRUE),
              quant25=quantile({{outcome}}, probs=.25, na.rm=TRUE),
              quant50=quantile({{outcome}}, probs=.50, na.rm=TRUE),
              quant75=quantile({{outcome}}, probs=.75, na.rm=TRUE),
              quant90=quantile({{outcome}}, probs=.90, na.rm=TRUE),
              quant99=quantile({{outcome}}, probs=.99, na.rm=TRUE)) %>%
    pivot_longer(cols=-{{group}},
                 names_to="statistics")
}

# online_exposure
dens = map2(list(actual_matches_iso_expo_off_1, actual_matches_iso_expo_off_5, actual_matches_iso_expo_off_9),
             list("1", "5", "9"),
          ~ .x%>%
            get_sumstats_group(partisanship, online_isolation) %>%
            mutate(outcome=.y)) %>%
  bind_rows() %>%
  filter(!is.na(partisanship)) %>%
  select(outcome, everything())

# let me compare wit Brown and Enos. Will generate exactly the same graph as fig 3
quant = dens %>%
  filter(str_detect(statistics, "quant")) %>%
  mutate(label=case_when(statistics=="quant01" ~ "1st",
                         statistics=="quant10" ~ "10th",
                         statistics=="quant25" ~ "25th",
                         statistics=="quant50" ~ "Median",
                         statistics=="quant75" ~ "75th",
                         statistics=="quant90" ~ "90th",
                         statistics=="quant99" ~ "99th"),
         label=fct_inorder(label),
         outcome=str_to_title(str_replace(outcome, "_", " ")),
         facet_y=str_c("Cutoff >", outcome, " Friends with Ideology Score"),
         facet_x = "Online Isolation")

# figure
ggplot(quant,
       aes(y=partisanship,
           x=label,
           color=partisanship,
           label=as.character(round(value, 2)))) +
  geom_point(shape=22,
             alpha=1,
             size=40,
             fill="gray94",
             stroke=1.5) +
  scale_color_manual(values=c(dem, rep)) +
  scale_size_continuous(range = c(60, 30)) +
  geom_text(size = 6, alpha =1, color = 'black') +
  coord_equal() +
  geom_vline(xintercept = 1:8 - 0.5, colour = "white", size = 1.5) +
  geom_hline(yintercept = 1:3 - 0.5, colour = "white", size = 1.5) +
  labs(y="", x=" \n Isolation by Percentile over Matched Voters") +
  facet_wrap(.~ facet_y, nrow=3) +
  guides(color="none", alpha="none", size="none") +
  theme(axis.title.x = element_text(hjust = .5),
        panel.grid.major  =  element_blank(),
        axis.ticks = element_blank())


# Taller canvas than the 12x8in default. The squares are geom_point(size = 40),
# an absolute 40mm, so each data row needs at least that much height. The
# two-facet quantile figures get ~42mm per row at 8in and render fine; this one
# stacks THREE facets (6 data rows + 3 strips), so 8in leaves only ~28mm per row,
# the squares overlap and the white separators cut through them. 12in restores
# the same per-row height the two-facet figures have.
save_function("sm_fig_12.png", height = 12)
save_function("sm_fig_12.pdf", height = 12)

# =========================================================================
# Heterogeneity in online degree across subgroups (R1.3b) -----------------
# =========================================================================

pacman::p_load(kableExtra, scales)

out_dir <- "output/tables"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

rbp_states <- read_csv(file.path(data_path, "blue_red_purple_states.csv"),
                       show_col_types = FALSE) %>%
  mutate(state_pol = paste0(state_pol, "s")) %>%
  select(state_po, state_pol)

# sample before the ten-friend cut.
het_base <- actual_matches_iso_expo_off %>%
  filter(!is.na(n_friends_w_ideology)) %>%
  mutate(state = str_sub(LALVOTERID, 4, 5)) %>%
  left_join(rbp_states, by = c("state" = "state_po"))

# Metro area.
metro_path <- file.path(data_path, "tracts_merge.csv")
if (file.exists(metro_path)) {
  metro_xwalk <- read_csv(metro_path, show_col_types = FALSE) %>%
    # labels are stored with a literal newline for plot wrapping; flatten it so
    # the table rows read as one line
    mutate(metro_area = str_squish(str_replace_all(metro_area, "\n", " "))) %>%
    select(census_tract, metro_area) %>%
    distinct(census_tract, .keep_all = TRUE)

  het_base <- het_base %>%
    add_state_code(Residence_Addresses_State) %>%
    mutate(census_tract = paste0(state_fips, Voters_FIPS,
                                 Residence_Addresses_CensusTract),
           census_tract = ifelse(str_detect(census_tract, "nan"), NA,
                                 census_tract)) %>%
    left_join(metro_xwalk, by = "census_tract")
} else {
  message("no metro crosswalk at ", metro_path,
          " -- the metro panel will be empty. Write tracts_merge out of ",
          "05_census_metro.R to add it.")
  het_base <- het_base %>% mutate(metro_area = NA_character_)
}

het <- het_base %>%
  transmute(
    deg    = n_friends_w_ideology,
    Party  = partisanship,
    Gender = case_when(Voters_Gender == "M" ~ "Male",
                       Voters_Gender == "F" ~ "Female"),
    Race   = case_when(
      EthnicGroups_EthnicGroup1Desc == "European"                           ~ "White",
      EthnicGroups_EthnicGroup1Desc == "Likely African-American"            ~ "Black",
      EthnicGroups_EthnicGroup1Desc == "Hispanic and Portuguese"            ~ "Hispanic",
      EthnicGroups_EthnicGroup1Desc %in% c("East and South Asian", "Other") ~ "Other"),
    Age    = case_when(suppressWarnings(as.numeric(Voters_Age)) < 35 ~ "Under 35",
                       suppressWarnings(as.numeric(Voters_Age)) < 61 ~ "35 to 60",
                       suppressWarnings(as.numeric(Voters_Age)) > 60 ~ "Over 60"),
    `State Politics`      = state_pol,
    `Metro Area`          = metro_area)

# long format: one row per voter x dimension
het_long <- het %>%
  pivot_longer(-deg, names_to = "Dimension", values_to = "Level") %>%
  filter(!is.na(Level)) %>%
  mutate(Dimension=str_replace(Dimension, " ", "\n"))

# n_kept is the denominator for the analytic-sample share, so it has to be the
# same total the paper analyses: every voter surviving the >9 cut.
n_full <- nrow(het)
n_kept <- sum(het$deg > FRIENDS_MIN)

# No bootstrap CI on these medians. 
het_tab <- het_long %>%
  group_by(Dimension, Level) %>%
  summarise(n             = n(),
            median_degree = stats::median(deg),
            p25           = stats::quantile(deg, .25),
            p75           = stats::quantile(deg, .75),
            pct_dropped   = 100 * mean(deg <= FRIENDS_MIN),
            share_full    = 100 * n() / n_full,
            share_kept    = 100 * sum(deg > FRIENDS_MIN) / n_kept,
            .groups = "drop") %>%
  mutate(delta_pp = share_kept - share_full) %>%
  arrange(Dimension, desc(n))

write_csv(het_tab, file.path(out_dir, "degree_heterogeneity_subgroups.csv"))

# table: rows grouped by dimension via pack_rows. het_tab keeps the filter-cost
# columns for the CSV; the printed table reports N, the median, the IQR and the
# subgroup's share of the panel. No CI column -- see the note above.
het_disp <- het_tab %>%
  transmute(Dimension,
            Level,
            N            = comma(n),
            Median       = sprintf("%.0f", median_degree),
            IQR          = sprintf("%.0f--%.0f", p25, p75),
            `% of Panel` = sprintf("%.1f", share_full))

pack_idx <- het_disp %>% count(Dimension) %>% deframe()

het_kbl <- het_disp %>%
  select(-Dimension) %>%
  kbl(format = "latex", booktabs = TRUE, align = "lrrrr",
      col.names = c("", "N", "Median", "IQR", "\\% of Panel"),
      escape = FALSE,
      caption = "Online Degree Across Subgroups",
      label = "degree_heterogeneity") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  pack_rows(index = pack_idx) %>%
  footnote(general = paste0(
    "Matched voters with a measurable online degree (N = ", comma(n_full),
    "). Median is of the number of friends with ideology scores; the IQR is ",
    "the spread of the underlying distribution. '\\% of Panel' is the ",
    "subgroup's share of that sample."),
    threeparttable = TRUE, escape = FALSE)

save_kable(het_kbl, file = file.path(out_dir, "degree_heterogeneity_subgroups.tex"))
