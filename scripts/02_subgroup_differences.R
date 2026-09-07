##############################################################################
# File-Name: 02_subgroup_differences.R
# author: Tiago Ventura
# Purpose: online - offline isolation differences across socio-demographic
#          subgroups, with bootstrapped confidence intervals
# Data in: data/matched_voters_segregation.json,
#          data/blue_red_purple_states.csv
# Data out: Figure 3
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

# REALLY IMPORTANT:filter the data -------------------
actual_matches_iso_expo_off <-actual_matches_iso_expo_off %>%
  select(everything(),
         -offline_exposure,
         -offline_isolation,
         # use the unweighted
         offline_exposure=unweighted_offline_exposure,
         offline_isolation=unweighted_offline_isolation
  )  %>%
  # removing people with less than 10 friends with ideolog
  filter(n_friends_w_ideology>9)


# Exposure race brackets -----------------------------------

# create age
actual_matches_iso_expo_off <-  actual_matches_iso_expo_off %>%
  mutate(race=case_when(EthnicGroups_EthnicGroup1Desc=="European"~"White",
                        TRUE~"Non-White"))


# merge
race_l2 <- actual_matches_iso_expo_off %>%
  mutate(delta_isolation=online_isolation - offline_isolation,
         delta_exposure=online_exposure - offline_exposure) %>%
  select(race, user_id, partisanship, delta_exposure, delta_isolation) %>%
  pivot_longer(cols = -c(user_id, partisanship, race),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  filter(!is.na(partisanship), !is.na(race))   %>%
  mutate(outcome=str_replace(outcome, "Delta Exposure", "Online Exposure Index"),
         outcome=str_replace(outcome, "Delta Isolation", "Online Isolation Index"))


# Load required library
library(boot)

# Define a function to calculate the median using bootstrap indices
median_func <- function(data, indices) {
  resampled_data <- data[indices]
  return(median(resampled_data))
}



# Perform the bootstrap
# Function to apply bootstrapping within each group
bootstrap_median_ci <- function(group_data) {

  # Apply bootstrapping with 1000 resamples
  boot_result <- boot(group_data$values, statistic = median_func, R = 200)

  # Calculate the 95% confidence interval
  ci <- boot.ci(boot_result,conf = 0.99, type = "perc")$percent[4:5]  # Extract the lower and upper CI
  return(data.frame(
    median = median(group_data$values),
    ci_lower = ci[1],
    ci_upper = ci[2]
  ))
}

# Apply the bootstrapping CI calculation within each group
results_race <- race_l2  %>%
  drop_na() %>%
  group_by(race, partisanship, outcome)  %>%
  group_modify(~ bootstrap_median_ci(.x))



# Exposure age brackets ---------------------------------

# create age
actual_matches_iso_expo_off <-  actual_matches_iso_expo_off %>%
  mutate(age_num=as.numeric(Voters_Age),
         age_brackets=case_when(age_num<35 ~ "Age: <35",
                                age_num>34 & age_num <61 ~ " Age: 35 - 60",
                                age_num>60 ~ "Age: +60"))


# merge
age_l2 <- actual_matches_iso_expo_off %>%
  mutate(delta_isolation=online_isolation - offline_isolation,
         delta_exposure=online_exposure - offline_exposure) %>%
  select(age_brackets, user_id, partisanship, delta_exposure, delta_isolation) %>%
  pivot_longer(cols = -c(user_id, partisanship, age_brackets),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  filter(!is.na(partisanship), !is.na(age_brackets))   %>%
  mutate(outcome=str_replace(outcome, "Delta Exposure", "Online Exposure Index"),
         outcome=str_replace(outcome, "Delta Isolation", "Online Isolation Index")) %>%
  mutate(age_brackets=fct_relevel(age_brackets, c("Age: <35",  " Age: 35 - 60", "Age: +60")))


# median
results_age <- age_l2 %>%
  drop_na() %>%
  group_by(age_brackets, partisanship, outcome)  %>%
  group_modify(~ bootstrap_median_ci(.x))


# Exposure gender ---------------------------------

# create gender
actual_matches_iso_expo_off <-  actual_matches_iso_expo_off %>%
  mutate(gender=ifelse(Voters_Gender=="M", "Male", "Female"))


# merge
gender_l2 <-  actual_matches_iso_expo_off %>%
  mutate(delta_isolation=online_isolation - offline_isolation,
         delta_exposure=online_exposure - offline_exposure) %>%
  select(gender, user_id, partisanship, delta_exposure, delta_isolation) %>%
  pivot_longer(cols = -c(user_id, partisanship, gender),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  filter(!is.na(partisanship))   %>%
  mutate(outcome=str_replace(outcome, "Delta Exposure", "Online Exposure Index"),
         outcome=str_replace(outcome, "Delta Isolation", "Online Isolation Index"))


# median
results_gender <- gender_l2 %>%
  drop_na() %>%
  group_by(gender, partisanship, outcome)  %>%
  group_modify(~ bootstrap_median_ci(.x))


# Exposure partisanship ---------------------------------

# merge
p_l2 <- actual_matches_iso_expo_off %>%
  mutate(delta_isolation=online_isolation - offline_isolation,
         delta_exposure=online_exposure - offline_exposure) %>%
  select(partisanship, user_id, delta_exposure, delta_isolation) %>%
  pivot_longer(cols = -c(user_id, partisanship),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  filter(!is.na(partisanship))   %>%
  mutate(outcome=str_replace(outcome, "Delta Exposure", "Online Exposure Index"),
         outcome=str_replace(outcome, "Delta Isolation", "Online Isolation Index"))

# median
partisanship_results <- p_l2 %>%
  drop_na() %>%
  group_by(partisanship, outcome)  %>%
  group_modify(~ bootstrap_median_ci(.x))


# Blue and Red States -----------------------------------------------------
rbp_states <- read_csv(file.path(data_path, "blue_red_purple_states.csv")) %>%
  mutate(state_pol=paste0(state_pol, "s"))

#create state
actual_matches_iso_expo_off <- actual_matches_iso_expo_off %>%
  mutate(state=str_sub(LALVOTERID, 4, 5))

# merge
bind_states_l2 <- actual_matches_iso_expo_off %>%
  as_tibble() %>%
  left_join(rbp_states, by=c("state"="state_po"))

bs_l2 <- bind_states_l2 %>%
  mutate(delta_isolation=online_isolation - offline_isolation,
         delta_exposure=online_exposure - offline_exposure) %>%
  select(state_pol, partisanship, user_id, delta_exposure, delta_isolation) %>%
  pivot_longer(cols = -c(user_id, partisanship, state_pol),
               names_to="outcome",
               values_to="values") %>%
  mutate(outcome=str_to_title(str_replace(outcome, "_", " "))) %>%
  filter(!is.na(partisanship))   %>%
  mutate(outcome=str_replace(outcome, "Delta Exposure", "Online Exposure Index"),
         outcome=str_replace(outcome, "Delta Isolation", "Online Isolation Index"))

# median
bs_results <- bs_l2%>%
  drop_na() %>%
  group_by(partisanship,state_pol, outcome)  %>%
  group_modify(~ bootstrap_median_ci(.x))


# Combine Results ---------------------------------------------------------

# fix partisanship
partisanship_results <- partisanship_results %>%
  mutate(subgroup="Pooled Partisanship") %>%
  select(subgroup, everything())

# fix and bind all
res <-  bs_results %>% rename("subgroup"="state_pol")%>% mutate(col="States") %>%
bind_rows(results_age %>%
  rename("subgroup"="age_brackets") %>% mutate(col="Age")) %>%
  bind_rows(results_gender %>% rename("subgroup"="gender") %>% mutate(col="Gender")) %>%
  bind_rows(results_race %>% rename("subgroup"="race")%>% mutate(col="Race")) %>%
  bind_rows(partisanship_results %>% mutate(col="Partisanship")) %>%
  ungroup()

# fix levels
res <- res %>% mutate(subgroup=fct_rev(fct_relevel(subgroup,
                                           c("Blue States", "Purple States", "Red States", "Age: <35",
                                             " Age: 35 - 60", "Age: +60",
                                             "Female"   ,     "Male"    ,      "Non-White" ,    "White",
                                           "Democrat"    ,  "Republican"))),
                      col=fct_rev(fct_inorder(col))) %>%
  mutate_if(is.numeric, ~-1*.x)


# centering around the global mean
# isolation
ggplot(res %>% filter(outcome=="Online Isolation Index"), aes(x = median,
             y = subgroup,
             color = partisanship,
             fill=partisanship)) +
  geom_errorbar(aes(xmin=ci_lower,
                    xmax=ci_upper),
                size=1, width=.2, alpha=.8,
                position=position_dodge(width = .3))  +
  geom_bar(stat="identity",
           alpha=.2,
           width=.4,
           color="transparent",
           fill="gray20",
           position=position_dodge(width = 0.3)) +
  scale_color_manual(values=c("blue", "red"), name="Partisanship") +
  scale_fill_manual(values=c("blue", "red"), name="Partisanship") +
  geom_vline(aes(xintercept=0), linetype="dashed", color="gray") +
  facet_grid(col~partisanship,scales = 'free_y') +
  theme(legend.position = "bottom") +
  xlab(" Online Isolation - Offline Isolation") +
  ylab("") +
  labs(title="",
       subtitle="",
       caption="Positive values indicate users have a higher ingroup isolation online than offline. \n Points represent the median for Online Exposure Index, with 99% confidence intervals calculate via bootstrapping.")


save_function("fig_3.png")
save_function("fig_3.pdf")
