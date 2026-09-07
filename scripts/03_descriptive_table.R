##############################################################################
# File-Name: 03_descriptive_table.R
# author: Tiago Ventura
# Purpose: demographics of the Twitter-L2 panel vs. ANES 2020 and
#          Pew Research NPORS 2021
# Data in: data/matched_voters_segregation.json,
#          data/anes_data_summary.csv, data/weighted_pew_data_summary.csv
# Data out: Table 1 (output/tables/tab1.tex)
##############################################################################

# basics: path, packages and utils ------------------------------------------------------------

# path for the data
data_path <- "data"

# packages
pacman::p_load(tidyverse, here, ggridges, wesanderson, janitor, ggtext, rjson, jsonlite)

# source graph
source("scripts/utils.R")

# open anes ---------------------------------------------------------------
x <- read_csv(file.path(data_path, "anes_data_summary.csv"))
pew <- read_csv(file.path(data_path, "weighted_pew_data_summary.csv")) %>% select(variable, se=SE,everything()) %>% mutate(source="Pew")


# Comparison socio demographics data ---------------------------------------

# open twitter-l2 data
tl2 =  stream_in(file(file.path(data_path, "matched_voters_segregation.json")))

# get covariates
std_mean <- function(x) sd(x, na.rm=TRUE)/sqrt(length(x))

tl2 <- tl2 %>%
  mutate(white=case_when(EthnicGroups_EthnicGroup1Desc=="European"~1,
                      EthnicGroups_EthnicGroup1Desc=="nan" | is.na(EthnicGroups_EthnicGroup1Desc)~NA_real_,
                      TRUE~0),
         age_num=as.numeric(Voters_Age),
         female=case_when(Voters_Gender=="F" ~ 1,
                       TRUE ~ 0),
         party=case_when(Parties_Description=="Democratic"~1,
                         TRUE~0))


# function to get the means and se

sum_stats <- function(data, var){

#remember this fucking thing!!!!!!!
variable <- deparse(substitute(var))

data %>%
    select({{var}}) %>%
    summarise(mean=mean({{var}}, na.rm=TRUE),
              SE=std_mean({{var}})) %>%
    mutate(variable=variable,
           source="tl2")
  }


# run all
tl2_sum <- bind_rows(sum_stats(tl2, white),
                          sum_stats(tl2, age_num),
                          sum_stats(tl2, female),
                          sum_stats(tl2, party)) %>%
            mutate(variable=case_when(str_detect(variable, "age")~"age",
                            TRUE ~ variable)) %>%
           select(variable, mean, se=SE, source) %>%
  mutate(variable=case_when(variable=="party"~"Partisanship (\\% Democrats)",
                            variable=="age" ~ "Age (Mean years)",
                            variable=="white" ~ "Ethnicity (\\% White)",
                            variable=="female"~ "Gender (\\% Female)"))


# bind
dwide = bind_rows(x, pew, tl2_sum)%>%
 mutate(source=str_to_lower(str_replace_all(source, " ", "_"))) %>%
  pivot_wider(id_cols = variable,
               names_from=source,
               values_from = c(mean, se))


# calculate p-value
dwide <- dwide %>%
          mutate(diff_anes_l2=mean_anes-mean_tl2,
                 zscore_anes_l2=(mean_anes-mean_tl2)/sqrt(se_anes^2+se_tl2^2),
                 diff_pew_l2=mean_pew-mean_tl2,
                 zscore_pew_l2=(mean_pew-mean_tl2)/sqrt(se_pew^2+se_tl2^2))



# make a nice table
tab = dwide %>%
  mutate_if(is.numeric, ~round(.x, 2)) %>%
  mutate(l2_values=paste0(mean_tl2, "(",se_tl2, ")"),
         anes_values=paste0(mean_anes, "(",se_anes, ")"),
         pew_values=paste0(mean_pew, "(",se_pew, ")")) %>%
  select(variable, l2_values, pew_values, anes_values,  diff_pew_l2,diff_anes_l2,  zscore_pew_l2, zscore_anes_l2)

library(kableExtra)
kable(
  tab,
  caption = "Comparing Demographics:Twitter-L2 Panel vs. ANES 2020 vs Pew Research NPORS 2021",
  format = "latex", booktabs = T, escape = F, linesep = "",
  row.names = F,
  label = "tab1",
  col.names = c("Variable", "Twitter Panel", "Pew", " ANES", "Pew", " ANES",
                "Pew", "ANES")) %>%
  add_header_above(c(" " = 1,"Sample Values" = 3, "$\\Delta$ Twitter Panel" = 2, "Z-Scores" = 2)) %>%
  kable_styling(full_width = F, latex_options = c("HOLD_position", "scale_down")) %>%
  column_spec(1, width = "7cm") %>%
  save_kable(., file = "output/tables/tab1.tex")
