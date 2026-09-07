##############################################################################
# File-Name: 05_census_metro.R
# author: Tiago Ventura
# Purpose: online and offline isolation across metropolitan areas and census
#          tract population density. Pulls tract population and geometry from
#          the Census API (tidycensus/tigris; requires a Census API key).
# Data in: data/matched_voters_segregation.json + Census API
# Data out: SI Figure 4; data/tracts_merge.csv (tract-to-metro crosswalk,
#           also used by 08_online_degree_cutoffs.R)
##############################################################################

# basics: path, packages and utils ------------------------------------------------------------

# path for the data
data_path <- "data"

# packages
pacman::p_load(tidyverse, here, ggridges, wesanderson, janitor, ggtext, rjson, jsonlite)

# source graph
source("scripts/utils.R")

# Open datasets -----------------------------------------------------------
actual_matches_iso_expo_off = stream_in(file(file.path(data_path, "matched_voters_segregation.json")))
actual_matches_iso_expo_off = as_tibble(actual_matches_iso_expo_off)


# Merge with census data --------------------------------------------------

# to merge with census data, I want merge voter at the block level.
## this is what the block looks like in the census
#Block* = STATE+COUNTY+TRACT+BLOCK
#           2+3+6+4=15
#Block 1050 in Census Tract 2231 in Harris County, TX
# 482012231001050


#state 2
actual_matches_iso_expo_off <- actual_matches_iso_expo_off %>%
  add_state_code(Residence_Addresses_State)

# build 15 digit id
actual_matches_iso_expo_off <- actual_matches_iso_expo_off %>%
  mutate(census_id = paste0(state_fips,Voters_FIPS, Residence_Addresses_CensusTract,Residence_Addresses_CensusBlock),
         census_tract = paste0(state_fips,Voters_FIPS, Residence_Addresses_CensusTract),
         census_id_county = paste0(state_fips,Voters_FIPS)) %>%
  mutate(census_id=ifelse(str_detect(census_id, "nan"),NA, census_id))

## select the variables
actual_matches_iso_expo_off = actual_matches_iso_expo_off %>%
  select(everything(),
         -offline_exposure,
         -offline_isolation,
         # use the unweighted
         offline_exposure=unweighted_offline_exposure,
         offline_isolation=unweighted_offline_isolation
  )   %>%
  filter(n_friends_w_ideology>9)


# Census data -------------------------------------------------------------

# Pull census tract data for population and geometry
list_states <- unique(actual_matches_iso_expo_off$state_fips)

library(tidycensus)
library(sf)

# pull census tract population data
pop_states <- map(list_states, ~ get_acs(
  geography = "tract",
  variables = c(population = "B01003_001"),
  state = .x,
  year = 2010,
  geometry = TRUE) %>%
  mutate(
    land_area_km2 = as.numeric(st_area(geometry)) / 1e6,
    land_area_ml =  as.numeric(st_area(geometry)) / (1e6 / 2.58999),
    population_density = estimate / land_area_km2  ,
    population_density_ml = estimate/land_area_ml
  )
)


# combine all
pop_all <- bind_rows(pop_states) %>% st_drop_geometry()


# have 23 missings here. These are mostly voters I do not have their census tract.
# I investigated if the issue here was the change in census tract code between 2010 and 2020, but it is not
actual_matches_iso_expo_off = left_join(actual_matches_iso_expo_off, pop_all, by=c("census_tract"="GEOID"))


# get major and minor urban areas -------------------------------------------------------------------------
library(tigris)

# metro areas
all_metro <- core_based_statistical_areas(cb = TRUE, year = 2010) %>% st_transform(8528)

# get tract
all_tract <- map_dfr(list_states, ~{
  tracts(.x, cb = TRUE, year = 2010)
}) %>%
  st_transform(8528)


# merge
tracts_instersect <- st_join(all_tract, all_metro, join = st_within)

tracts_merge <- tracts_instersect %>%
                  select(geoid_census=GEO_ID.x, STATE, COUNTY, TRACT, metro_area=LSAD.y) %>%
                  st_drop_geometry() %>%
                  clean_names()

# merge back with actuall matches

tracts_merge <- tracts_merge %>%
  as_tibble() %>%
  mutate(metro_area=case_when(metro_area=="Metro" ~ "Major \n Metropolitan Areas",
                              metro_area=="Micro" ~ "Minor \n Metropolitan Areas",
                              TRUE ~ "Outside of \n Metropolitan Areas"),
         census_tract=paste0(state, county, tract)
         )

write_csv(tracts_merge, file.path(data_path, "tracts_merge.csv"))

actual_matches_iso_expo_off <- left_join(actual_matches_iso_expo_off, tracts_merge, by="census_tract")

# define population density
actual_matches_iso_expo_off <-actual_matches_iso_expo_off %>%
  mutate(pop_density_category=case_when(population_density_ml<102 ~ "Very Low Density",
                                                       population_density_ml<800 &population_density_ml>101 ~ "Low Density",
                                                       population_density_ml>800 & population_density_ml<2213  ~ "Medium Density",
                                                       population_density_ml>2213 ~ "High Density"))


# Separate by census information ------------------------------------------------

# histograms
hist <- actual_matches_iso_expo_off %>%
  select(user_id, partisanship, offline_exposure, online_exposure, offline_isolation, online_isolation,
  metro_area, pop_density_category) %>%
  pivot_longer(cols = -c(user_id, partisanship, metro_area, pop_density_category),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  mutate_if(is.character, ~str_replace(.x, "Exposure", "Outgroup \n Exposure")) %>%
  mutate_if(is.character, ~str_replace(.x, "Isolation", "Ingroup \n Isolation"))



# median
median <- hist %>%
  group_by(partisanship, metro_area, outcome) %>%
  summarise(value=median(values, na.rm=TRUE)) %>%
  na.exclude() %>%
  bind_cols(max=c(rep(12000, 4), rep(1100, 4),
                  rep(800, 4),rep(12000, 4), rep(1100, 4),
                  rep(800, 4)))

# density

# graph
ggplot(hist %>%
         filter(str_detect(outcome, "Isolation")) %>%
         mutate(outcome=str_remove(outcome, "Ingroup \n Isolation")) %>% drop_na(),
       aes(x=values,
           fill=partisanship, color=partisanship)) +
  geom_density(aes(y=..density..), binwidth=.01,alpha = .4) +
  geom_segment(data = median %>% filter(str_detect(outcome, "Isolation")) %>%
                 mutate(outcome=str_remove(outcome, "Ingroup \n Isolation")),
               aes(xend=value,
                   x=value,
                   y=0,
                   yend=4,
                   color = partisanship), size=1,  linetype="dashed") +
  labs(y="Density", x="") +
  ggtitle("") +
  facet_grid(outcome~metro_area, scales = "free_y") +
  scale_fill_manual(values=c(dem, rep), name="") +
  scale_color_manual(values=c(dem, rep), name="")  +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .25)) +
  scale_y_continuous(labels=)+
  theme(legend.position = "bottom",
        panel.grid =  element_blank(),
        strip.background = element_blank()) +
  guides(alpha="none", fill = "none", color="none")

save_function("sm_fig_4a.png")
save_function("sm_fig_4a.pdf")


# Separate by census information ------------------------------------------------

# histograms
hist <- actual_matches_iso_expo_off %>%
  select(user_id, partisanship, offline_exposure, online_exposure, offline_isolation, online_isolation,
         metro_area, pop_density_category) %>%
  pivot_longer(cols = -c(user_id, partisanship, metro_area, pop_density_category),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  mutate_if(is.character, ~str_replace(.x, "Exposure", "Outgroup \n Exposure")) %>%
  mutate_if(is.character, ~str_replace(.x, "Isolation", "Ingroup \n Isolation")) %>%
  mutate(pop_density_category=fct_relevel(pop_density_category,
                                          c("High Density",
                                            "Medium Density",
                                            "Low Density",
                                            "Very Low Density")))



# median
median <- hist %>%
  group_by(partisanship, pop_density_category, outcome) %>%
  summarise(value=median(values, na.rm=TRUE)) %>%
  na.exclude() %>%
  bind_cols(max=c(rep(7500, 4), rep(7500, 4),
                  rep(7500, 4),rep(7500, 4), rep(7500, 4),
                  rep(7500, 4),
                  rep(7500, 4),
                  rep(7500, 4)))

# density

# graph
ggplot(hist %>%
         filter(str_detect(outcome, "Isolation")) %>%
         mutate(outcome=str_remove(outcome, "Ingroup \n Isolation")) %>% drop_na(),
       aes(x=values,
           fill=partisanship, color=partisanship)) +
  geom_density(aes(y=..density..), binwidth=.01,alpha = .4) +
  geom_segment(data = median %>% filter(str_detect(outcome, "Isolation")) %>%
                 mutate(outcome=str_remove(outcome, "Ingroup \n Isolation")),
               aes(xend=value,
                   x=value,
                   y=0,
                   yend=4,
                   color = partisanship), size=1,  linetype="dashed") +
  labs(y="Density", x="Ingroup Isolation",
       caption="Straight Lines Represents the Median of the Distributions") +
  ggtitle("") +
  facet_grid(outcome~pop_density_category, scales = "free_y") +
  scale_fill_manual(values=c(dem, rep), name="") +
  scale_color_manual(values=c(dem, rep), name="")  +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .25)) +
  scale_y_continuous(labels=)+
  theme(legend.position = "bottom",
        panel.grid =  element_blank(),
        strip.background = element_blank()) +
  guides(alpha="none", fill = guide_legend(override.aes = list(alpha = 1)))

save_function("sm_fig_4b.png")
save_function("sm_fig_4b.pdf")
