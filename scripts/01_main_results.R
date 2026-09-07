##############################################################################
# File-Name: 01_main_results.R
# author: Tiago Ventura
# Purpose: descriptive analysis of online and offline partisan segregation
#          (probabilistic imputation, 1000 nearest neighbors)
# Data in: data/matched_voters_segregation.json
# Data out: Figure 1 (A, B), Figure 2, SI Figures 5-7,
#           SI Figure 8 (top), SI Figure 9 (top)
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
des = map(c("online_exposure", "online_isolation",
            "offline_exposure","offline_isolation"),
          ~ actual_matches_iso_expo_off %>%
            get_sumstats_group(partisanship, get(.x)) %>%
            mutate(outcome=.x)) %>%
  bind_rows() %>%
  filter(!is.na(partisanship)) %>%
  select(outcome, everything())

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
  labs(y="", x=" \n Isolation by Percentile over Matched Voters") +
  facet_grid(facet_y~.) +
  guides(color="none", alpha="none", size="none") +
  theme(axis.title.x = element_text(hjust = .5),
        panel.grid.major  =  element_blank(),
        axis.ticks = element_blank())

save_function("fig_1b.png")
save_function("fig_1b.pdf")
# same figure serves as the top panel of SI Figures 8 and 9
save_function("sm_fig_8a.png")
save_function("sm_fig_8a.pdf")
save_function("sm_fig_9a.png")
save_function("sm_fig_9a.pdf")


# histograms
hist <- actual_matches_iso_expo_off %>%
  select(user_id, partisanship, offline_exposure, online_exposure, offline_isolation, online_isolation) %>%
  pivot_longer(cols = -c(user_id, partisanship),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  mutate_if(is.character, ~str_replace(.x, "Exposure", "Outgroup \n Exposure")) %>%
  mutate_if(is.character, ~str_replace(.x, "Isolation", "\n Isolation"))

# median
median <- quant %>%
  filter(label=="Median")

# isolation
# graph
ggplot(hist %>%
         filter(str_detect(outcome, "Isolation")) %>%
         mutate(outcome=str_remove(outcome, "\n Isolation")),
       aes(x=values,
           fill=partisanship, color=partisanship)) +
  geom_histogram(binwidth=.01,alpha = .4,position="identity") +
  geom_segment(data = median %>% filter(str_detect(outcome, "Isolation")) %>%
                 mutate(outcome=str_remove(outcome, "Ingroup \n Isolation")),
               aes(xend=value,
                   x=value,
                   y=0,
                   yend=20000,
                   color = partisanship), size=1,  linetype="dashed") +
  labs(y="Counts", x="Partisan Isolation",
       caption="Dashed Lines Represents the Median of the Distributions") +
  ggtitle("") +
  facet_grid(outcome~.) +
  scale_fill_manual(values=c(dem, rep), name="") +
  scale_color_manual(values=c(dem, rep), name="")  +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1)) +
  scale_y_continuous(labels=)+
  theme(legend.position = "bottom",
        panel.grid =  element_blank()) +
  guides(alpha="none", fill = guide_legend(override.aes = list(alpha = 1))) +
  ylim(0,20000)

save_function("fig_1a.png")
save_function("fig_1a.pdf")


# Exposure race brackets -----------------------------------
# create age
actual_matches_iso_expo_off <-  actual_matches_iso_expo_off %>%
  mutate(race=case_when(EthnicGroups_EthnicGroup1Desc=="European"~"White",
                        TRUE~"Non-White"))


# merge
race_l2 <- actual_matches_iso_expo_off %>%
  select(race, user_id, partisanship, offline_exposure, online_exposure, offline_isolation, online_isolation) %>%
  pivot_longer(cols = -c(user_id, partisanship, race),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  filter(!is.na(partisanship), !is.na(race))   %>%
  mutate(outcome=str_replace(outcome, "Exposure", "Outgroup \n  Exposure"))


# median
median <- race_l2 %>%
  group_by(race, partisanship, outcome) %>%
  summarise(value=median(values, na.rm=TRUE)) %>%
  na.exclude()

# graph density
ggplot(race_l2 %>%
         filter(str_detect(outcome, "Isolation"),
                !is.na(partisanship)) %>%
         mutate(outcome=str_remove_all(outcome, "Ingroup")),
       aes(x=values,
           fill=partisanship,
           color=partisanship)) +
  geom_density(binwidth=.01,alpha = .5,position="identity") +
  geom_segment(data = median %>% filter(str_detect(outcome, "Isolation")) %>%
               mutate(outcome=str_remove_all(outcome, "Ingroup")),
               aes(xend=value,
                   x=value,
                   y=0,
                   yend=3,
                   color = partisanship), size=1,  linetype="solid") +
  labs(y="Densities", x="Partisan Isolation",
       caption="Straight Lines Represents the Median of the Distributions") +
  ggtitle("") +
  facet_grid(outcome~race) +
  scale_fill_manual(values=c(dem, rep), name="") +
  scale_color_manual(values=c(dem, rep), name="")  +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .25)) +
  scale_y_continuous(labels=)+
  theme(legend.position = "bottom",
        panel.grid =  element_blank(),
        strip.background = element_rect(fill="gray92", color="gray92"),
        strip.text = element_text(size=12)) +
  guides(alpha="none",
         fill = guide_legend(override.aes = list(alpha = 1)))

save_function("sm_fig_7.png")
save_function("sm_fig_7.pdf")


# Exposure age brackets ---------------------------------

# create age
actual_matches_iso_expo_off <-  actual_matches_iso_expo_off %>%
  mutate(age_num=as.numeric(Voters_Age),
         age_brackets=case_when(age_num<35 ~ "Age: <35",
                                age_num>34 & age_num <61 ~ " Age: 35 - 60",
                                age_num>60 ~ "Age: +60"))


# merge
age_l2 <- actual_matches_iso_expo_off %>%
  select(age_brackets, user_id, partisanship, offline_exposure, online_exposure, offline_isolation, online_isolation) %>%
  pivot_longer(cols = -c(user_id, partisanship, age_brackets),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  filter(!is.na(partisanship), !is.na(age_brackets)) %>%
  mutate(age_brackets=fct_relevel(age_brackets, c("Age: <35",  " Age: 35 - 60", "Age: +60")))  %>%
  mutate(outcome=str_replace(outcome, "Exposure", "Outgroup \n  Exposure"))


# median
median <- age_l2 %>%
  group_by(age_brackets, partisanship, outcome) %>%
  summarise(value=median(values, na.rm=TRUE)) %>%
  na.exclude()

# graph density
ggplot(age_l2 %>%
         filter(str_detect(outcome, "Isolation"),
                !is.na(partisanship)) %>%
         mutate(outcome=str_remove_all(outcome, "Ingroup")),
       aes(x=values,
           fill=partisanship,
           color=partisanship)) +
  geom_density(binwidth=.01,alpha = .5,position="identity") +
  geom_segment(data = median %>% filter(str_detect(outcome, "Isolation")) %>%
                 mutate(outcome=str_remove_all(outcome, "Ingroup")),
               aes(xend=value,
                   x=value,
                   y=0,
                   yend=3,
                   color = partisanship), size=1,  linetype="dashed") +
  labs(y="Densities", x="Partisan Isolation",
       caption="Straight Lines Represents the Median and Dashed Lines the Mean of the Distributions") +
  ggtitle("") +
  facet_grid(outcome~age_brackets) +
  scale_fill_manual(values=c(dem, rep), name="") +
  scale_color_manual(values=c(dem, rep), name="")  +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .25)) +
  scale_y_continuous(labels=)+
  theme(legend.position = "bottom",
        panel.grid =  element_blank(),
        strip.background = element_rect(fill="gray92", color="gray92"),
        strip.text = element_text(size=12)) +
  guides(alpha="none",
         fill = guide_legend(override.aes = list(alpha = 1)))

save_function("sm_fig_6.png")
save_function("sm_fig_6.pdf")


# Exposure gender ---------------------------------

# create gender
actual_matches_iso_expo_off <-  actual_matches_iso_expo_off %>%
  mutate(gender=ifelse(Voters_Gender=="M", "Male", "Female"))


# merge
gender_l2 <- actual_matches_iso_expo_off %>%
  select(gender, user_id, partisanship, offline_exposure, online_exposure, offline_isolation, online_isolation) %>%
  pivot_longer(cols = -c(user_id, partisanship, gender),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  filter(!is.na(partisanship), !is.na(gender))

# median
median <- gender_l2 %>%
  group_by(gender, partisanship, outcome) %>%
  summarise(value=median(values, na.rm=TRUE)) %>%
  na.exclude()

# graph density
ggplot(gender_l2 %>%
         filter(str_detect(outcome, "Isolation"),
                !is.na(partisanship)) %>%
         mutate(outcome=str_remove_all(outcome, "Ingroup")),
       aes(x=values,
           fill=partisanship,
           color=partisanship)) +
  geom_density(binwidth=.01,alpha = .5,position="identity") +
  geom_segment(data = median %>% filter(str_detect(outcome, "Isolation")) %>%
                 mutate(outcome=str_remove_all(outcome, "Ingroup")),
               aes(xend=value,
                   x=value,
                   y=0,
                   yend=3,
                   color = partisanship), size=1,  linetype="dashed") +
  labs(y="Densities", x="Partisan Isolation",
       caption="Straight Lines Represents the Median and Dashed Lines the Mean of the Distributions") +
  ggtitle("") +
  facet_grid(outcome~gender) +
  scale_fill_manual(values=c(dem, rep), name="") +
  scale_color_manual(values=c(dem, rep), name="")  +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .25)) +
  scale_y_continuous(labels=)+
  theme(legend.position = "bottom",
        panel.grid =  element_blank(),
        strip.background = element_rect(fill="gray92", color="gray92"),
        strip.text = element_text(size=12)) +
  guides(alpha="none",
         fill = guide_legend(override.aes = list(alpha = 1)))

save_function("sm_fig_5.png")
save_function("sm_fig_5.pdf")


# Smoother Correlation ----------------------------------------------------

## graph isolation
# binning
d <- actual_matches_iso_expo_off
cut <- cut(d$offline_isolation,500, include.lowest = TRUE)
tmp <- aggregate(d$online_isolation, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate(d$offline_isolation, by=list(cut = cut), FUN=mean, na.rm=T)
data <- data.frame(offline_isolation = tmp1$x, online_isolation = tmp$x) %>% mutate(partisanship="All")


# dem
d <- actual_matches_iso_expo_off %>% filter(partisanship=="Democrat")
cut <- cut(d$offline_isolation,500, include.lowest = TRUE)
tmp <- aggregate(d$online_isolation, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate(d$offline_isolation, by=list(cut = cut), FUN=mean, na.rm=T)
data_d <- data.frame(offline_isolation = tmp1$x, online_isolation = tmp$x) %>% mutate(partisanship="Democrat")


# dem
d <- actual_matches_iso_expo_off %>% filter(partisanship=="Republican")
cut <- cut(d$offline_isolation,500, include.lowest = TRUE)
tmp <- aggregate(d$online_isolation, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate(d$offline_isolation, by=list(cut = cut), FUN=mean, na.rm=T)
data_r <- data.frame(offline_isolation = tmp1$x, online_isolation = tmp$x) %>% mutate(partisanship="Republican")

# bind
data = bind_rows(data, data_d, data_r)

# Global
ggplot(data %>% filter(partisanship=="All"),
       aes(x=offline_isolation, y=online_isolation)) +
  geom_smooth(alpha=.2, color="black", fill="gray") +
  geom_point(shape=21, size=3, alpha=.3,
             color="black", fill="gray") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom") +
  labs(x="Offline Outgroup Isolation", y="Online Outgroup Isolation") +
  ylim(0.3, 0.75)

save_function("fig_2a.png")
save_function("fig_2a.pdf")

# only parties
ggplot(data %>% filter(partisanship!="All"),
       aes(x=offline_isolation, y=online_isolation, fill=partisanship, color=partisanship)) +
  geom_smooth(alpha=.2) +
  geom_point(shape=21,
             size=3,
             alpha=.3) +
  scale_fill_manual(values=c(dem, rep), name="")  +
  scale_color_manual(values=c(dem, rep), name="")  +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom") +
  labs(x="Offline Outgroup Isolation", y="Online Outgroup Isolation") +
  ylim(0.3, 0.75)

save_function("fig_2b.png")
save_function("fig_2b.pdf")
