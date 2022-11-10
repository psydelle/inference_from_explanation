## DOCUMENT DETAILS ----------------------------------------------------------

# Project: DR. NLP Replication Study
# Working Title: Replicating Kirfel et al. (2022)
# Authors: Sydelle de Souza & Ivan Vegner
# Created: 2022/10/29 # nolint
# R version: 4.1.3 (2022-03-10) -- "One Push-Up"

#-----------------------------------------------------------------------------#

## COMMENTS -------------------------------------------------------------------

# Kirfel, L., Icard, T., & Gerstenberg, T. (2022). Inference from explanation.
# Journal of Experimental Psychology: General, 151(7), 1481–1501.
# https://doi.org/10.1037/xge0001151

# this file contains data analysis code forked from the Kirfel et al.'s (2022)
# github repo (https://github.com/cicl-stanford/inference_from_explanation.git)
# which contains all the materials required to reproduce their analysis.
# the objective here is to reproduce their data wrangling process and obtain
# a clean dataset in long format that can then be used to reproduce their
# statistical analysis.

#-----------------------------------------------------------------------------#

## SET UP ---------------------------------------------------------------------

## load libraries
# we do no need all these libraries for the data wrangling, but loading them jic
# as their code is quite messy and has unconventional dependencies,
# will comment them out later
# added the 'here' package to help set up a project-oriented workflow
# added 'skimr' for overviews
#

library("knitr")       # for RMarkdown commands
library("kableExtra")  # for nice tables
library("janitor")     # for cleaning column names
library("brms")        # for Bayesian regression modeling
library("tidybayes")   # for Bayesian regression modeling
library("broom.mixed") # for tidy regression results
library("xtable")      # for latex tables
library("ggeffects")   # for marginal effects
library("car")         # for ANOVAs
library("lme4")        # for linear mixed effects models
library("effectsize")  # for calculating effect sizes
library("pwr")         # for power analysis
library("patchwork")   # for figure panels
library("tidyverse")   # for data wrangling, visualization, etc.
library("here")        # for setting up wd workflow
library("skimr")       # for quick descriptives

#-----------------------------------------------------------------------------#

## CONSTRUCTING THE DATASET ---------------------------------------------------

## here we make small changes to the original code to set up file paths
# and formatting for readability
#

## CONJUNCTIVE STRUCTURE: STATISTICAL NORMS
df.stat_con_data = read.csv(here("data/statistical_conjunctive.csv"),
                   stringsAsFactors = F) %>%
  filter(row_number() > 2) %>% # additional rows in qualtrics csv
  clean_names() %>%
  filter(!status == "Survey Preview", #exclude preview trials,
         !ethnicity == "") # exclude people who did not make it
                           #  until demographics,i.e. incomplete data

head(df.stat_con_data)

# Reference clip in final test clip: dark motion block is at the top
# in test clip (Recode condition "db")
# Reference slider: picture with dark block at the top is on the right
# of the slider (0-"Normal", 100 "Abnormal") (Recode condition "1 and "4"")

df.stat_con_judgments = df.stat_con_data %>%
  mutate(participant = 1:n()) %>%
       select(dt_ball_a_1:b_dark_top_right_1,
              contains("dark_thru"),
              condition,
              participant) %>%
       pivot_longer(cols = -c(participant, condition),
                     names_to = "index",
                     values_to = "value") %>%
       filter(value != "") %>%
       mutate(index = ifelse(str_detect(index, "conjunctive"),
                             "conjunctive",
                             index),
              position = str_extract_all(index, "db|dt"),
              index = ifelse(str_detect(index, "thru"),
                             "selection",
                             index),
              index = ifelse(str_detect(index, "top"),
                             "judgment",
                            index),
              index = str_remove(index, "db_"),
              index = str_remove(index, "dt_"),
              index = str_remove(index, "_1"),
              value = ifelse(value == "Ball E did go through the gate because 
                                       ball A did go through the motion block.",
                            "1",
                            ifelse(value == "Ball E did go through the gate 
                                             because ball B did go through 
                                             the motion block.",
                                   2,
                                   value))) %>%
       mutate(value = as.numeric(value),
              value = ifelse(test = (position == "db" &
                                     str_detect(index, "ball")),
                            yes = 100 - value,
                            no = value),
              value = ifelse((position == "db" &
                              str_detect(index, "selection")),
                            3 - value,
                            value)) %>%
       select(-position) %>%
       pivot_wider(names_from = index,
                     values_from = value) %>%
       mutate(selection = factor(selection,
                                   levels = 1:2,
                                   labels = c("abnormal", "normal"))) %>%
       mutate(judgment = ifelse(condition %in% c(1, 4),
                            100 - judgment,
                            judgment)) %>%
       rename(ball_abnormal = ball_a,
              ball_normal = ball_b) %>%
       arrange(participant) %>%
       # define exclusion criteria 
       mutate(exclude = ifelse((ball_abnormal < ball_normal) &
              (conjunctive < 50), 0, 1))

head(df.stat_con_judgments)
skim(df.stat_con_judgments)



# CONJUNCTIVE STRUCTURE: PRESCRIPTIVE NORMS
df.pres_con_data = read.csv(here("data/prescriptive_conjunctive.csv"),
                            stringsAsFactors = F) %>%
       filter(row_number() > 2) %>% # additional rows in qualtrics csv
       clean_names() %>%
       filter(!status == "Survey Preview", #exclude preview trials,
              !ethnicity == "") # exclude people who did not make
                           # until demographics, i.e. incomplete survey

# reference clip:
# Billy is the abnormal agent, Suzy the normal agent (Recode norm_condition "2")
# The selected agent as the abnormal agent is at the top of the
# judgment scale ( Recode learn_condition ="2")

df.pres_con_judgments = df.pres_con_data %>%
  mutate(participant = 1:n()) %>% 
  select(b_abnorm_billy_1:suzy_b_abnorm_right_1,
         learn_condition,
         condition,
         participant,
         -starts_with("q"),
         -starts_with("timing")) %>% 
  pivot_longer(cols = -c(participant, learn_condition, condition),
               names_to = "index",
               values_to = "value") %>%
  filter(value != "") %>%
  mutate(index = ifelse(str_detect(index, "left|right"),
                        "judgment",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "b_abnorm_billy_1|s_abnorm_billy_1"),
                        "agreement_Billy",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "b_abnorm_suzy_1|s_abnorm_suzy_1"),
                        "agreement_Suzy",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "conjunctive"),
                        "conjunctive",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "selection"),
                        "selection",
                        index))%>% 
  mutate(value = as.numeric(value),
         value = ifelse(test = (learn_condition == "2" & str_detect(index, "agreement")),
                        yes = 100 - value,
                        no = value),
         value = ifelse((learn_condition == "2" & str_detect(index, "selection")),
                        3 - value,
                        value))%>% 
  pivot_wider(names_from = index,
              values_from = value) %>% 
  mutate(selection = factor(selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal"))) %>%
  mutate(judgment = ifelse(condition %in% c(1, 4),
                           100 - judgment,
                           judgment)) %>% 
  rename(agreement_abnormal_agent = agreement_Billy,
         agreement_normal_agent = agreement_Suzy) %>% 
  arrange(participant) %>% 
  # define exclusion criteria 
  mutate(exclude = ifelse((agreement_abnormal_agent < agreement_normal_agent) &
         (conjunctive < 50), 0, 1))

head(df.pres_con_judgments)
skim(df.pres_con_judgments)


# DISJUNCTIVE STRUCTURE: STATISTICAL NORMS
df.stat_dis_data = read.csv(here("data/statistical_disjunctive.csv"),
                   stringsAsFactors = F) %>%
  filter(row_number() > 2) %>% # additional rows in qualtrics csv
  clean_names() %>% 
  filter(!status == "Survey Preview", # exclude preview trials,
         !ethnicity == "") # exclude people who did not make until it demographics

# Reference clip in final test clip: dark motion block is at the top in
# test clip (Recode condition "db")
# Reference slider: picture with dark block at the top is on the right of 
# the slider (0-"Normal", 100 "Abnormal") (Recode condition "1 and "4"")
df.stat_dis_judgments = df.stat_dis_data %>% 
  mutate(participant = 1:n()) %>% 
  select(dt_ball_a_1:b_dark_top_right_1,
         contains("dark_thru"),
         condition,
         participant) %>% 
  pivot_longer(cols = -c(participant, condition),
               names_to = "index",
               values_to = "value") %>%
  filter(value != "") %>% 
  mutate(index = ifelse(str_detect(index, "disjunctive"),
                        "disjunctive",
                        index),
         position = str_extract_all(index, "db|dt"),
         index = ifelse(str_detect(index, "thru"),
                        "selection",
                        index),
         index = ifelse(str_detect(index, "top"),
                        "judgment",
                        index),
         index = str_remove(index, "db_"),
         index = str_remove(index, "dt_"),
         index = str_remove(index, "_1"),
         value = ifelse(value == "Ball E did go through the gate because ball A did go through the motion block.",
                        "1",
                        ifelse(value == "Ball E did go through the gate because ball B did go through the motion block.",
                               2,
                               value))) %>% 
  mutate(value = as.numeric(value),
         value = ifelse(test = (position == "db" & str_detect(index, "ball")),
                        yes = 100 - value,
                        no = value),
         value = ifelse((position == "db" & str_detect(index, "selection")),
                        3 - value,
                        value)) %>% 
  select(-position) %>% 
  pivot_wider(names_from = index,
              values_from = value) %>% 
  mutate(selection = factor(selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal"))) %>%
  mutate(judgment = ifelse(condition %in% c(1, 4),
                           100 - judgment,
                           judgment)) %>% 
  rename(ball_abnormal = ball_a,
         ball_normal = ball_b) %>% 
  arrange(participant) %>% 
  # define exclusion criteria 
  mutate(exclude = ifelse((ball_abnormal < ball_normal) & 
         (disjunctive > 50), 0, 1))

# DISJUNCTIVE STRUCTURE: PRESCRIPTIVE NORMS 
df.pres_dis_data = read.csv(here("data/prescriptive_disjunctive.csv"),
                   stringsAsFactors = F) %>% 
  filter(row_number() > 2) %>% # additional rows in qualtrics csv
  clean_names() %>% 
  filter(!status == "Survey Preview", #exclude preview trials, 
         !age == "") #exclude people who did not make until demographics

# reference clip: 
# Billy is the abnormal agent, Suzy the normal agent (Recode norm_condition "2")
# The selected agent as the abnormal agent is at the top of the 
# judgment scale (100) ( Recode learn_condition ="2")

df.pres_dis_judgments = df.pres_dis_data %>% 
  mutate(participant = 1:n()) %>% 
  select(b_abnorm_billy_1:suzy_b_abnorm_right_1,
         learn_condition,
         condition,
         participant,
         -starts_with("q"),
         -starts_with("timing")) %>% 
  pivot_longer(cols = -c(participant, learn_condition, condition),
               names_to = "index",
               values_to = "value") %>%
  filter(value != "") %>%
  mutate(index = ifelse(str_detect(index, "left|right"),
                        "judgment",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "b_abnorm_billy_1|s_abnorm_billy_1"),
                        "agreement_Billy",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "b_abnorm_suzy_1|s_abnorm_suzy_1"),
                        "agreement_Suzy",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "disjunctive"),
                        "disjunctive",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "selection"),
                        "selection",
                        index))%>% 
  mutate(value = as.numeric(value),
         value = ifelse(test = (learn_condition == "2" & str_detect(index, "agreement")),
                        yes = 100 - value,
                        no = value),
         value = ifelse((learn_condition == "2" & str_detect(index, "selection")),
                        3 - value,
                        value))%>%
  pivot_wider(names_from = index,
              values_from = value) %>%
  mutate(selection = factor(selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal"))) %>%
  mutate(judgment = ifelse(condition %in% c(1, 4),
                           100 - judgment,
                           judgment)) %>%
  rename(agreement_abnormal_agent = agreement_Billy,
         agreement_normal_agent = agreement_Suzy) %>%
  arrange(participant) %>%
  # define exclusion criteria
  mutate(exclude = ifelse((agreement_abnormal_agent < agreement_normal_agent) &
         (disjunctive > 50), 0, 1))

# COMBINE THE DATA
df.normality = df.stat_con_judgments %>%
  mutate(structure = "conjunctive") %>%
  rename(structure_question = conjunctive) %>%
  bind_rows(df.stat_dis_judgments %>%
              mutate(structure = "disjunctive") %>% 
              rename(structure_question = disjunctive)) %>%
  mutate(norm = "statistical") %>%
  rename(abnormal_rating = ball_abnormal,
         normal_rating = ball_normal, 
         structure_rating = structure_question) %>%
  bind_rows(df.pres_con_judgments %>%
              mutate(structure = "conjunctive") %>%
              rename(structure_question = conjunctive) %>%
              bind_rows(df.pres_dis_judgments %>%
                          mutate(structure = "disjunctive") %>%
                          rename(structure_question = disjunctive)) %>%
              mutate(norm = "prescriptive") %>%
              rename(abnormal_rating = agreement_abnormal_agent,
                     normal_rating = agreement_normal_agent,
                     structure_rating = structure_question) %>%
              select(- learn_condition)) %>%
  filter(exclude == 0) %>% # filter based on exclusion criteria
  mutate(participant = 1:n()) %>% 
  mutate(structure = factor(structure, 
                            levels = c("conjunctive", "disjunctive")),
         norm = factor(norm, 
                       levels = c("statistical", "prescriptive"))) %>%
  select(-condition)

head(df.normality)
view(df.normality)
