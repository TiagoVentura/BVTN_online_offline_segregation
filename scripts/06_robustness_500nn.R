##############################################################################
# File-Name: 06_robustness_500nn.R
# author: Tiago Ventura
# Purpose: offline isolation robustness with 500 nearest neighbors
# Data in: data/matched_voters_segregation_500nn.json
# Data out: SI Figure 8 (bottom panel)
##############################################################################

# basics: path, packages and utils ------------------------------------------------------------

# path for the data
data_path <- "data"

# packages
pacman::p_load(tidyverse, here, ggridges, wesanderson, janitor, ggtext, rjson, jsonlite)

# source graph
source("scripts/utils.R")

# Combined online + offline data ------------------------------------------
temp_path <- file.path(data_path, "matched_voters_segregation_500nn.json")
stopifnot(file.exists(temp_path))
actual_matches_iso_expo_off <- as_tibble(stream_in(file(temp_path)))

## all this code will use the unweighted measure. I will rename just to make my life easier
actual_matches_iso_expo_off <-  actual_matches_iso_expo_off %>%
  select(everything(), 
         -offline_exposure, 
         -offline_isolation,
         # use the unweighted
         offline_exposure=unweighted_offline_exposure, 
         offline_isolation=unweighted_offline_isolation
  )  %>%
  # removing people with less than 10 friends with ideolog
  filter(n_friends_w_ideology>9)

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
des = map(c("online_exposure", "online_isolation", 
            "offline_exposure","offline_isolation"), 
          ~ actual_matches_iso_expo_off %>%
            get_sumstats_group(partisanship, get(.x)) %>%
            mutate(outcome=.x)) %>% 
  bind_rows() %>%
  filter(!is.na(partisanship)) %>%
  select(outcome, everything()) 


#actual_matches_iso_expo_off %>% select(contains("n_"), online_isolation, offline_isolation) %>% View()

# let me compare wit Brown and Enos. Will generate exactly the same graph as fig 3
quant = des %>% 
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
         facet_y=str_remove_all(outcome, "Exposure|Isolation"), 
         facet_x=str_remove_all(outcome, "Offline|Online"),
         alpha=case_when(str_detect(outcome, "Exposure") ~ 1-value, 
                         str_detect(outcome, "Isolation") ~ value), 
         stroke=abs(alpha-2))  %>%
  mutate_if(is.character, ~str_replace(.x, "Exposure", "Outgroup \n Exposure")) %>%
  mutate_if(is.character, ~str_replace(.x, "Isolation", "Ingroup \n Isolation"))

# circle to differentiate from enos == new

ggplot(quant %>% filter(facet_x==" Ingroup \n Isolation") %>% 
         mutate(outcome=str_replace(outcome, " ", "\n"), 
                facet_y=str_c(facet_y, " Isolation")), 
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
  labs(y="", x=" \n Isolation by percentile over Twitter-L2 matched voters") +
  facet_grid(facet_y~.) +
  guides(color="none", alpha="none", size="none") +
  theme(axis.title.x = element_text(hjust = .5), 
        panel.grid.major  =  element_blank(), 
        axis.ticks = element_blank()) 

save_function("sm_fig_8b.png")
save_function("sm_fig_8b.pdf")
