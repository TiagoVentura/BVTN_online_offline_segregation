##############################################################################
# File-Name: 12_engagement_networks.R
# author: Tiago Ventura
# Purpose: interaction/engagement network robustness.
#          Plots the percentile curve of online isolation measured on the FOLLOW
#          network (the paper's measure) against the same quantity measured on
#          five ENGAGEMENT networks (all engagement, retweets, quote tweets,
#          replies, mentions). Overlapping curves = the follow network is not
#          driving the level of measured online segregation.
# Data in:  data/engagement_network_percentiles.csv
#           (wide, 101 rows = percentiles 0.00-1.00 in the `index` col; one
#            column per network: online_isolation [= follow network],
#            engagement_isolation, retweet_isolation, quotetweet_isolation,
#            mentions_isolation, replies_isolation)
# Data out: SI Figure 14 (output/figs/probs/engagement_network_isolation_percentiles.{png,pdf})
#           printed percentile + gap tables for the SI prose
##############################################################################

# basics: path, packages and utils -------------------------------------------

# ggtext is required BEFORE sourcing utils.R: my_theme calls element_markdown()
pacman::p_load(tidyverse, here, ggtext)

# shared ggplot theme / helpers (my_theme, dem/rep colors, save_function -> figs/)
source("scripts/utils.R")

csv_path <- file.path("data",
                     "engagement_network_percentiles.csv")

# ---------------------------------------------------------------------------
# 0. Load and reshape.
#    The file is one row per percentile and one column per network, i.e. it is
#    ALREADY the plotted object -- no quantiles are computed here. Long form is
#    just what ggplot wants.
# ---------------------------------------------------------------------------
raw <- read_csv(csv_path, show_col_types = FALSE)

stopifnot(nrow(raw) == 101,
          all(c("index", "online_isolation", "engagement_isolation",
                "retweet_isolation", "quotetweet_isolation",
                "mentions_isolation", "replies_isolation") %in% names(raw)))

# legend order = the order the curves are introduced in the SI text: the
# paper's own measure first, then the pooled engagement network, then the
# four interaction types that make it up.
net_levels <- c("Follow Network Isolation", "All Engagement", "Retweets",
                "Quote Tweets", "Replies", "Mentions")

net_labels <- c(online_isolation     = "Follow Network Isolation",
                engagement_isolation = "All Engagement",
                retweet_isolation    = "Retweets",
                quotetweet_isolation = "Quote Tweets",
                replies_isolation    = "Replies",
                mentions_isolation   = "Mentions")

df <- raw %>%
  rename(percentile = index) %>%
  pivot_longer(cols = -percentile,
               names_to = "network", values_to = "online_isolation") %>%
  mutate(network = factor(recode(network, !!!as.list(net_labels)),
                          levels = net_levels))

# the follow network is the reference: dark and heavy, engagement networks are
# the comparison set and get the colored palette (Okabe-Ito, colorblind-safe).
net_colors <- c("Follow Network Isolation" = "#22211d",
                "All Engagement"           = "#E69F00",
                "Retweets"                 = "#009E73",
                "Quote Tweets"             = "#D55E00",
                "Replies"                  = "#CC79A7",
                "Mentions"                 = "#0072B2")

net_widths <- c("Follow Network Isolation" = 1.6,
                "All Engagement"           = 1.0,
                "Retweets"                 = 1.0,
                "Quote Tweets"             = 1.0,
                "Replies"                  = 1.0,
                "Mentions"                 = 1.0)

# ---------------------------------------------------------------------------
# 1. Percentile table for the SI prose. Reported at the same cut points the
#    paper uses elsewhere for isolation quantiles.
# ---------------------------------------------------------------------------
key_p <- c(0.10, 0.25, 0.50, 0.75, 0.90)

pct_tab <- df %>%
  filter(round(percentile, 2) %in% key_p) %>%
  mutate(percentile = round(percentile, 2)) %>%
  pivot_wider(names_from = percentile, values_from = online_isolation,
              names_prefix = "p") %>%
  arrange(network)

print(pct_tab, n = Inf)

# how far each engagement network sits from the follow network, on average and
# at its worst. Quote tweets are the outlier -- say so rather than smooth it.
gap_tab <- df %>%
  pivot_wider(names_from = network, values_from = online_isolation) %>%
  pivot_longer(cols = all_of(net_levels[-1]),
               names_to = "network", values_to = "value") %>%
  mutate(gap = value - `Follow Network Isolation`) %>%
  group_by(network) %>%
  summarise(mean_gap = mean(gap),
            med_gap  = median(gap),
            max_abs  = max(abs(gap)), .groups = "drop") %>%
  arrange(desc(max_abs))

print(gap_tab)

# ---------------------------------------------------------------------------
# 2. The figure.
# ---------------------------------------------------------------------------
p_main <- ggplot(df, aes(x = percentile, y = online_isolation,
                         color = network, linewidth = network)) +
  geom_line() +
  scale_color_manual(values = net_colors, name = "") +
  scale_linewidth_manual(values = net_widths, guide = "none") +
  # 2% expansion: the curves run the full 0-1 range on both axes, so with the
  # default of no expansion they sit flush against the panel border.
  scale_x_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1),
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1),
                     expand = expansion(mult = 0.02)) +
  labs(x = "User Distribution Percentile", y = "Online Isolation") +
  # six labels don't fit on one row at 11in wide -- 2 x 3 is what renders.
  guides(color = guide_legend(nrow = 2,
                              override.aes = list(linewidth = 1.4))) +
  # theme_bw() overrides the theme_light() + my_theme base set in utils.R, so
  # the font and axis sizes are respecified below -- without the text element
  # the figure renders in the default sans, not the paper's Palatino.
  # plot.margin is explicit because the 5.5pt default clips the y tick labels.
  theme_bw() +
  theme(text               = element_text(family = my_font),  # Palatino, from utils.R
        plot.margin        = margin(t = 12, r = 20, b = 8, l = 12, unit = "pt"),
        #axis.title.x       = element_text(hjust = 1, margin = margin(t = 10)),
        #axis.title.y       = element_text(margin = margin(r = 10)),
        legend.position    = "bottom",
        legend.text        = element_text(size = 13),
        legend.key.width   = unit(0.8, "cm"),
        legend.margin      = margin(0, 0, 0, 0),
        legend.box.spacing = unit(0.5, "cm"), 
        axis.title= element_text(size=16),
        axis.text = element_text(size=14),
        axis.title.x = element_text(hjust=1))

p_main

save_function("sm_fig_14.png",
              width = 11, height = 7.5)
save_function("sm_fig_14.pdf",
              width = 11, height = 7.5)

## end of script
