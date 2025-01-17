
# Load packages 
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

library("here")
#here("GitHub/inference_from_explanation")

getwd()

#setwd("C:/Users/psyde/Documents/GitHub/inference_from_explanation")

# set ggplot theme 
theme_set(theme_classic())


# suppress summarise() grouping warning 
options(dplyr.summarise.inform = F)

# use effects contrast coding to interpret effects from categorical variables as main effects 
options(contrasts = c("contr.sum", "contr.poly"))


# Functions 


# function for printing out html or latex tables 
print_table = function(data, format = "html", digits = 2){
  if(format == "html"){
    data %>% 
      kable(digits = digits) %>% 
      kable_styling()
  }else if(format == "latex"){
    data %>% 
      xtable(digits = digits,
             caption = "Caption",
             label = "tab:table") %>%
      print(include.rownames = F,
            booktabs = T,
            sanitize.colnames.function = identity,
            caption.placement = "top")
  }
}


# Experiment 1: Normality inference

## Read in data 


# CONJUNCTIVE STRUCTURE: STATISTICAL NORMS 
df.stat_con_data = read.csv(here("../../data/statistical_conjunctive.csv"),
                   stringsAsFactors = F) %>% 
  filter(row_number() > 2) %>% # additional rows in qualtrics csv
  clean_names() %>% 
  filter(!status == "Survey Preview", #exclude preview trials, 
         !ethnicity == "") #exclude people who did not make it until demographics, i.e. incomplete data

# Reference clip in final test clip: dark motion block is at the top 
# in test clip (Recode condition "db")
# Reference slider: picture with dark block at the top is on the right 
# of the slider (0-"Normal", 100 "Abnormal") (Recode condition "1 and "4"")
df.stat_con_judgments = df.stat_con_data %>% 
  mutate(participant = 1:n()) %>% 
  select(dt_ball_a_1:b_dark_top_right_1, contains("dark_thru"), condition, participant) %>% 
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
         (conjunctive < 50), 0, 1))

# CONJUNCTIVE STRUCTURE: PRESCRIPTIVE NORMS 
df.pres_con_data = read.csv(here("../../data/prescriptive_conjunctive.csv"),
                   stringsAsFactors = F) %>% 
  filter(row_number() > 2) %>% # additional rows in qualtrics csv
  clean_names() %>% 
  filter(!status == "Survey Preview", #exclude preview trials, 
         !ethnicity == "") #exclude people who did not make until demographics, i.e. incomplete survey

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

# DISJUNCTIVE STRUCTURE: STATISTICAL NORMS 
df.stat_dis_data = read.csv(here("../../data/statistical_disjunctive.csv"),
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
df.pres_dis_data = read.csv(here("../../data/prescriptive_disjunctive.csv"),
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
  mutate(structure = factor(structure, levels = c("conjunctive", "disjunctive")),
         norm = factor(norm, levels = c("statistical", "prescriptive"))) %>% 
  select(-condition)
  

view(df.normality)
## Demographics 


df.normality.demo = df.stat_con_data %>% 
  select(feedback,
         age_1_text,
         gender,
         race_1_text,
         ethnicity) %>% 
  mutate(norm = "statistical",
         structure = "conjunctive",
         participant = 1:n()) %>% 
  left_join(df.stat_con_judgments %>% 
              select(participant, exclude),
            by = "participant") %>% 
  bind_rows(df.stat_dis_data %>% 
              select(feedback,
                     age_1_text,
                     gender,
                     race_1_text,
                     ethnicity) %>% 
              mutate(norm = "statistical",
                     structure = "disjunctive",
                     participant = 1:n()) %>% 
              left_join(df.stat_dis_judgments %>% 
                          select(participant, exclude),
                        by = "participant")) %>% 
  bind_rows(df.pres_con_data %>% 
              select(feedback,
                     age_1_text,
                     gender,
                     race_1_text,
                     ethnicity) %>% 
              mutate(norm = "prescriptive",
                     structure = "conjunctive",
                     participant = 1:n()) %>% 
              left_join(df.pres_con_judgments %>% 
                          select(participant, exclude),
                        by = "participant")) %>% 
  bind_rows(df.pres_dis_data %>% 
              select(feedback,
                     age_1_text,
                     gender,
                     race_1_text,
                     ethnicity) %>% 
              mutate(norm = "prescriptive",
                     structure = "disjunctive",
                     participant = 1:n()) %>% 
              left_join(df.pres_dis_judgments %>% 
                          select(participant, exclude),
                        by = "participant")) %>% 
  mutate(age = as.numeric(age_1_text),
         age = ifelse(age > 1000, 2019 - age, age),
         gender = ifelse(gender == 1, "Female", gender),
         gender = ifelse(gender == 2, "Male", gender),
         gender = ifelse(gender == 3, "Non-binary", gender),
         gender = ifelse(gender == 4, "Prefer not to say.", gender))

df.normality.demo %>% 
  summarize(n_excluded = sum(exclude == 1))

df.normality.demo %>% 
  filter(exclude == 0) %>%
  count(norm, structure) %>% 
  print_table()

df.normality.demo %>% 
  summarize(n_total = n(),
            n_female = sum(gender == "Female"),
            n_male = sum(gender == "Male"),
            n_not_say = sum(gender == "Prefer not to say."),
            n_non_binary = sum(gender == "Non-binary"),
            mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T)) %>% 
  mutate(across(contains("age"), round)) %>% 
  pivot_longer(cols = everything()) %>% 
  print_table()

# remove unnecessary variables
rm(list = ls()[!ls() %in% c("df.normality",
                            "df.normality.demo",
                            "print_table")])


## Plots 

### Causal selections 


set.seed(1)

df.plot = df.normality %>% 
  mutate(selection = ifelse(selection == "abnormal", 1, 0),
         norm = factor(norm,
                       levels = c("statistical", "prescriptive"),
                       labels = c("statistical\nnorm", "prescriptive\nnorm")))

df.text = df.plot %>% 
  count(structure, norm) %>% 
  mutate(label = str_c("n = ", n),
         selection = 1.05)

p.exp1.selections = ggplot(data = df.plot,
                           mapping = aes(x = norm, 
                                         y = selection,
                                         group = structure,
                                         fill = structure)) +
  geom_hline(yintercept = 0.5, 
             linetype = 2) + 
  stat_summary(fun.data = "mean_cl_boot",
               geom = "pointrange",
               position = position_dodge(width = 0.5),
               size = 1.5,
               shape = 21) + 
  geom_text(data = df.text,
            mapping = aes(label = label),
            position = position_dodge(width = 0.5),
            size = 6) +
  labs(x = "type of norm",
       y = "% abnormal cause\nselections",
       title = "Experiment 1") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     labels = str_c(seq(0, 100, 25), "%"),
                     expand = expansion(mult = c(0.05, 0))) +
  coord_cartesian(clip = "off",
                  ylim = c(0, 1)) + 
  theme(text = element_text(size = 24),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        plot.margin = margin(t = 1, l = 0.2, r = 0.2, unit = "cm"),
        plot.title = element_text(vjust = 4,
                                  color = "gray40",
                                  hjust = 0.5))

p.exp1.selections

ggsave(filename = "../../figures/plots/normality_selections.pdf",
       plot = p.exp1.selections,
       width = 8,
       height = 6)


### Main judgment data 


set.seed(1)

df.plot = df.normality %>% 
  mutate(participant = 1:n(),
         norm = factor(norm,
                       levels = c("statistical", "prescriptive"),
                       labels = c("statistical\nnorm", "prescriptive\nnorm")),
         judgment = judgment - 50)

df.text = df.plot %>% 
  count(structure, norm) %>% 
  mutate(label = str_c("n = ", n),
         judgment = 55)

p.exp1.judgments = ggplot(data = df.plot,
                          mapping = aes(x = norm, 
                                        y = judgment,
                                        group = structure,
                                        fill = structure)) +
  geom_hline(yintercept = 0, 
             linetype = 2) + 
  geom_point(mapping = aes(color = structure), 
             alpha = 0.2,
             size = 2,
             position = position_jitterdodge(jitter.width = 0.1,
                                             dodge.width = 0.5),
             show.legend = F) + 
  stat_summary(fun.data = "mean_cl_boot",
               geom = "pointrange",
               position = position_dodge(width = 0.5),
               size = 1.5,
               shape = 21) + 
  annotate(x = c(0.5, 0.5),
           y = c(-50, 50),
           geom = "text",
           label = c("preference for\nnormal event", "preference for\nabnormal event"),
           hjust = c(0, 1),
           angle = 90,
           size = 6) + 
  geom_text(data = df.text,
            mapping = aes(label = label),
            position = position_dodge(width = 0.5),
            size = 5) + 
  labs(x = "type of norm",
       y = "normality inference") + 
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(-50, 50, 25),
                     labels = seq(-50, 50, 25),
                     expand = expansion(mult = c(0.05, 0))) + 
  coord_cartesian(clip = "off",
                  ylim = c(-50, 50)) +
  theme(text = element_text(size = 24),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        plot.margin = margin(t = 1, l = 0.2, r = 0.2, unit = "cm"))

p.exp1.judgments

ggsave(filename = "../../figures/plots/normality_judgments.pdf",
       plot = p.exp1.judgments,
       width = 8,
       height = 6)


### Individual differences


set.seed(1)

df.plot = df.normality %>% 
  mutate(judgment = judgment - 50,
         selection = str_c(selection, "\nselection"))

df.text = df.plot %>% 
  count(selection, structure) %>% 
  mutate(judgment = 55,
         label = str_c("n = ", n))

p.exp1.individual = ggplot(data = df.plot,
                           mapping = aes(x = selection, 
                                         y = judgment,
                                         group = structure,
                                         fill = structure)) +
  geom_hline(yintercept = 0, 
             linetype = 2) + 
  geom_point(mapping = aes(color = structure),
             alpha = 0.2,
             size = 2,
             position = position_jitterdodge(jitter.width = 0.1,
                                             dodge.width = 0.5),
             show.legend = F) + 
  stat_summary(fun.data = "mean_cl_boot",
               geom = "pointrange",
               position = position_dodge(width = 0.5),
               size = 1.5,
               shape = 21) + 
  annotate(x = c(0.5, 0.5),
           y = c(-50, 50),
           geom = "text",
           label = c("preference for\nnormal event", "preference for\nabnormal event"),
           hjust = c(0, 1),
           angle = 90,
           size = 6) + 
  geom_text(data = df.text,
            mapping = aes(label = label),
            position = position_dodge(width = 0.5),
            size = 5) +
  labs(x = "type of norm",
       y = "normality inference") + 
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(-50, 50, 25),
                     labels = seq(-50, 50, 25),
                     expand = expansion(mult = c(0.05, 0))) + 
  coord_cartesian(clip = "off",
                  ylim = c(-50, 50)) +
  theme(text = element_text(size = 24),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        plot.margin = margin(t = 1, l = 0.2, r = 0.2, unit = "cm"))

p.exp1.individual  
  
ggsave(filename = "../../figures/plots/normality_judgments_based_on_selections.pdf",
       plot = p.exp1.individual,
       width = 8,
       height = 6)


## Stats 

### Means and standard deviations 


df.normality %>% 
  group_by(structure) %>% 
  summarize(mean = mean(judgment),
            sd = sd(judgment)) %>% 
  print_table()


### Bayesian regression on selection data



fit_normality_selection = brm(formula = selection ~ structure * norm,
                              family = "bernoulli",
                              data = df.normality %>%
                                mutate(selection = ifelse(selection == "normal", 0, 1)),
                              seed = 1,
                              cores = 2,
                              file = "cache/fit_normality_selection")


fit_normality_selection



fit_normality_selection = brm(formula = selection ~ structure * norm,
                              family = "bernoulli",
                              data = df.normality %>%
                                mutate(selection = ifelse(selection == "normal", 0, 1)),
                              seed = 1,
                              cores = 2,
                              file = "cache/fit_normality_selection")

pp_check(fit_normality_selection, ndraws=1000)
# Predictions on probability scale: 


fit_normality_selection %>% 
  ggpredict(terms = c("structure", "norm"))


### Bayesian regression on judgment data

densityPlot(df.normality$judgment)

fit_normality_judgment = brm(formula = judgment ~ structure * norm,
                             data = df.normality,
                             seed = 1,
                             cores = 2,
                             file = "cache/fit_normality_judgment")

prior_summary(fit_normality_judgment)
fit_normality_judgment
pp_check(fit_normality_judgment)


fit_normality_judgment_m2 = brm(formula = judgment ~ structure * norm,
                             data = df.normality,
                             family = "negbinomial",
                             seed = 1,
                             cores = 2,
                             file = "cache/fit_normality_judgment_m2")

fit_normality_judgment_m2
pp_check(fit_normality_judgment_m2)
performance::check_distribution(fit_normality_judgment)


fit_normality_judgment_m3 = brm(formula = judgment ~ structure * norm,
                                data = df.normality,
                                family = "zero_inflated_negbinomial",
                                seed = 1,
                                cores = 2,
                                file = "cache/fit_normality_judgment_m3")

fit_normality_judgment_m3

pp_check(fit_normality_judgment_m3)

df.normality$bjudgment <- df.normality$judgment/100
gghistogram(df.normality$bjudgment, bins = 100)

ggqqplot(df.normality$bjudgment)

(contrasts(df.normality$structure) <- contr.sum(2)/2)
( contrasts(df.normality$norm) <- contr.sum(2)/2)

fit_normality_judgment_m4 = brm(formula = bjudgment ~ structure * norm,
                                data = df.normality,
                                family = "zero_one_inflated_beta",
                                seed = 1,
                                cores = 2,
                                file = "cache/fit_normality_judgment_m4.1")

fit_normality_judgment_m4
pp_check(fit_normality_judgment_m4, ndraws = 100)

hist(df.normality$judgment)



## Tables

### Causal selections 


fit_normality_selection %>% 
  gather_draws(b_Intercept, 
               b_structure1,
               b_norm1,
               `b_structure1:norm1`) %>% 
  mean_hdi() %>% 
  rename(term = .variable,
         estimate = .value,
         `lower 95% CI` = .lower,
          `upper 95% CI` = .upper) %>% 
  select(-c(.width, .point, .interval)) %>% 
  mutate(term = tolower(term),
         term = str_remove_all(term, "b_"),
         term = str_remove_all(term, "1"),
         term = factor(term, levels = c("intercept", "structure", "norm", "structure:norm"))) %>% 
  arrange(term) %>% 
  print_table()


### Normality judgments 


fit_normality_judgment %>% 
  gather_draws(b_Intercept, 
               b_structure1,
               b_norm1,
               `b_structure1:norm1`) %>% 
  mean_hdi() %>% 
  rename(term = .variable,
         estimate = .value,
         `lower 95% CI` = .lower,
          `upper 95% CI` = .upper) %>% 
  select(-c(.width, .point, .interval)) %>% 
  mutate(term = tolower(term),
         term = str_remove_all(term, "b_"),
         term = str_remove_all(term, "1"),
         term = factor(term, levels = c("intercept", "structure", "norm", "structure:norm"))) %>% 
  arrange(term) %>% 
  print_table()


### Relationship between selections and normality inference 


df.normality %>% 
  mutate(judgment_binary = ifelse(judgment < 50, "normal", "abnormal")) %>% 
  group_by(structure, norm) %>% 
  summarize(selection = sum(selection == "abnormal")/n(),
            inference = sum(judgment_binary == "abnormal")/n()) %>% 
  select(norm, structure, everything()) %>% 
  arrange(desc(norm), structure) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), ~ round(.,2))) %>% 
  print_table()


# Experiment 2: Structure inference

## Read in data 


# ABNORMAL EXPLANATION: STATISTICAL NORM 
df.stat_abnormal_data = read.csv(file = "../../data/statistical_abnormal.csv",
                   stringsAsFactors = F) %>% 
  filter(row_number() > 2) %>% # additional rows in qualtrics csv
  clean_names() %>% 
  filter(!status == "Survey Preview", # exclude preview trials, 
         !ethnicity == "") # exclude people who did not make until it demographics

# Reference clip Learning Trial and Judgment: Darkrm Motion Block is top, Ball A 
# is abnormal (Recode values of DB condition)
# Reference Slider: Conjunctive on the right side (100 - Definitely Conjunctive, 
# 0 - Definitely Disjunctive) (Recode Conj_Left Condition)

df.stat_abnormal_judgments = df.stat_abnormal_data %>% 
  mutate(participant = 1:n()) %>% 
  select(conj_a_db_1,
         conj_b_db_1,
         conj_db_1,
         conj_a_dt_1,
         conj_b_dt_1,
         conj_dt_1,
         disj_a_db_1,
         disj_b_db_1,
         disj_db_1,
         disj_a_dt_1,
         disj_b_dt_1,
         disj_dt_1,
         exp_conj_db,
         exp_conj_dt,
         exp_disj_db,
         exp_disj_dt,
         abnormal_b_conj_left_1,
         abnormal_b_con_right_1,
         abnormal_a_conj_left_1,
         abnormal_b_con_righ_1,
         condition,
         structure,
         participant) %>% 
  pivot_longer(cols = -c(participant, condition, structure),
               names_to = "index",
               values_to = "value") %>%
  filter(value != "") %>% 
  mutate(index = ifelse(str_detect(index, "abnormal"),
                        "judgment",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "conj_a_db_1|conj_a_dt_1"),
                        "conjunctive_ball_a",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "conj_b_db_1|conj_b_dt_1"),
                        "conjunctive_ball_b",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "conj_db_1|conj_dt_1"),
                        "conjunctive",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "disj_a_db_1|disj_a_dt_1"),
                        "disjunctive_ball_a",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "disj_b_db_1|disj_b_dt_1"),
                        "disjunctive_ball_b",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "disj_db_1|disj_dt_1"),
                        "disjunctive",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "exp_conj_db|exp_conj_dt"),
                        "conjunctive_selection",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "exp_disj_db|exp_disj_dt"),
                        "disjunctive_selection",
                        index))%>%
  mutate(value = as.numeric(value),
         value = ifelse(test = (condition == "DB" & str_detect(index, "ball")),
                        yes = 100 - value,
                        no = value),
         value = ifelse((condition == "DB" & str_detect(index, "selection")),
                        3 - value,
                        value)) %>% 
  pivot_wider(names_from = index,
              values_from = value) %>% 
  mutate(conjunctive_selection = factor(conjunctive_selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal")))%>% 
  mutate(disjunctive_selection = factor(disjunctive_selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal")))%>%
  mutate(judgment = ifelse(structure =="Conj_Left",
                           100 - judgment,
                           judgment)) %>% 
  rename(conjunctive_ball_abnormal = conjunctive_ball_a,
         conjunctive_ball_normal = conjunctive_ball_b,
         disjunctive_ball_abnormal = disjunctive_ball_a,
         disjunctive_ball_normal = disjunctive_ball_b) %>% 
  arrange(participant) %>% 
  # define exclusion criteria 
  mutate(exclude = ifelse((conjunctive_ball_abnormal < conjunctive_ball_normal) &
         (disjunctive_ball_abnormal < disjunctive_ball_normal) & 
         (conjunctive < 50) &
         (disjunctive > 50), 0, 1))

# ABNORMAL EXPLANATION: PRESCRIPTIVE NORM 
df.pres_abnormal_data = read.csv(file = "../../data/prescriptive_abnormal.csv",
                   stringsAsFactors = F) %>% 
  filter(row_number() > 2) %>% # additional rows in qualtrics csv
  clean_names() %>% 
  filter(!status == "Survey Preview", #exclude preview trials, 
         !ethnicity == "") # exclude people who did not make until demographics

# reference clip: 
# Billy is the abnormal agent (Recode Norm Condition ="2")
# Cojunctive Structure is at the top scale (100) (Recode ConjLeft Condition)

df.pres_abnormal_judgments = df.pres_abnormal_data %>% 
  mutate(participant = 1:n()) %>% 
  select(b_abnorm_conj_billy1_1:s_selected_conj_right_1,
         structure,
         norm_condition,
         participant,
         -starts_with("x45"),
         -starts_with("s89"),
         -starts_with("q"),
         -starts_with("check"),
         -starts_with("timing")) %>% 
  pivot_longer(cols = -c(participant, structure, norm_condition),
               names_to = "index",
               values_to = "value") %>%
  filter(value != "") %>%
  mutate(index = ifelse(str_detect(index, "selected"),
                        "judgment",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "b_abnorm_conj_billy|s_abnorm_conj_billy"),
                        "conjunctive_agreement_billy",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "b_abnorm_conj_suzy|s_abnorm_conj_suzy"),
                        "conjunctive_agreement_suzy",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "b_abnorm_conjunctive|s_abnorm_conjunctive"),
                        "conjunctive",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "b_abnorm_conj_exp|s_abnorm_conj_exp"),
                        "conjunctive_selection",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "b_abnorm_disj_billy|s_abnorm_disj_billy"),
                        "disjunctive_agreement_billy",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "b_abnorm_disj_suzy|s_abnorm_disj_suzy"),
                        "disjunctive_agreement_suzy",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "b_abnorm_disjunctive|s_abnorm_disjunctive"),
                        "disjunctive",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "b_abnorm_disj_exp|s_abnorm_disj_exp"),
                        "disjunctive_selection",
                        index))%>%
  mutate(value = as.numeric(value),
         value = ifelse(test = (norm_condition == "2" & str_detect(index, "agreement")),
                        yes = 100 - value,
                        no = value),
         value = ifelse((norm_condition == "2" & str_detect(index, "selection")),
                        3 - value,
                        value))%>% 
  pivot_wider(names_from = index,
              values_from = value) %>% 
  mutate(conjunctive_selection = factor(conjunctive_selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal"))) %>%
  mutate(disjunctive_selection = factor(disjunctive_selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal")))%>%
  mutate(judgment = ifelse(structure =="ConjLeft",
                           100 - judgment,
                           judgment)) %>% 
  rename(conjunctive_agent_abnormal = conjunctive_agreement_billy,
        conjunctive_agent_normal = conjunctive_agreement_suzy,
        disjunctive_agent_abnormal = disjunctive_agreement_billy,
        disjunctive_agent_normal = disjunctive_agreement_suzy) %>% 
  arrange(participant) %>% 
  # define exclusion criteria
  mutate(exclude = ifelse(
    test = (conjunctive_agent_abnormal < conjunctive_agent_normal) &
         (disjunctive_agent_abnormal < disjunctive_agent_normal) &
         (conjunctive < 50) &
         (disjunctive > 50),
    yes = 0, 
    no = 1))

# NORMAL EXPLANATION: STATISTICAL NORMS 
df.stat_normal_data = read.csv(file = "../../data/statistical_normal.csv",
                   stringsAsFactors = F) %>% 
  filter(row_number() > 2) %>% # additional rows in qualtrics csv
  clean_names() %>% 
  filter(!status == "Survey Preview", # exclude preview trials, 
         !ethnicity == "") # exclude people who did not make until it demographics, i.e. incomplete data

# Reference clip Learning Trial and Judgment: Dark Motion Block is top, Ball A is abnormal 
# (Recode values of DB condition)
# Reference Slider: Conjunctive on the right side (100 - Definitely Conjunctive, 
# 0 - Definitely Disjunctive) (Recode Conj_Left Condition)
df.stat_normal_judgments = df.stat_normal_data %>% 
  mutate(participant = 1:n()) %>% 
  select(conj_a_db_1,
         conj_b_db_1,
         conj_db_1,
         conj_a_dt_1,
         conj_b_dt_1,
         conj_dt_1,
         disj_a_db_1,
         disj_b_db_1,
         disj_db_1,
         disj_a_dt_1,
         disj_b_dt_1,
         disj_dt_1,
         exp_conj_db,
         exp_conj_dt,
         exp_disj_db,
         exp_disj_dt,
         normal_a_conj_left_1,
         normal_a_con_right_1,
         normal_b_conj_left_1,
         normal_b_con_right_1,
         condition,
         structure,
         participant) %>%
  pivot_longer(cols = -c(participant, condition, structure),
               names_to = "index",
               values_to = "value") %>%
  filter(value != "") %>% 
  mutate(index = ifelse(str_detect(index, "normal"),
                        "judgment",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "conj_a_db_1|conj_a_dt_1"),
                        "conjunctive_ball_a",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "conj_b_db_1|conj_b_dt_1"),
                        "conjunctive_ball_b",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "conj_db_1|conj_dt_1"),
                        "conjunctive",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "disj_a_db_1|disj_a_dt_1"),
                        "disjunctive_ball_a",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "disj_b_db_1|disj_b_dt_1"),
                        "disjunctive_ball_b",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "disj_db_1|disj_dt_1"),
                        "disjunctive",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "exp_conj_db|exp_conj_dt"),
                        "conjunctive_selection",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "exp_disj_db|exp_disj_dt"),
                        "disjunctive_selection",
                        index))%>%
  mutate(value = as.numeric(value),
         value = ifelse(test = (condition == "DB" & str_detect(index, "ball")),
                        yes = 100 - value,
                        no = value),
         value = ifelse((condition == "DB" & str_detect(index, "selection")),
                        3 - value,
                        value)) %>% 
  pivot_wider(names_from = index,
              values_from = value) %>% 
  mutate(conjunctive_selection = factor(conjunctive_selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal")))%>% 
  mutate(disjunctive_selection = factor(disjunctive_selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal")))%>%
  mutate(judgment = ifelse(structure =="Conj_Left",
                           100 - judgment,
                           judgment)) %>% 
  rename(conjunctive_ball_abnormal = conjunctive_ball_a,
        conjunctive_ball_normal = conjunctive_ball_b,
        disjunctive_ball_abnormal = disjunctive_ball_a,
        disjunctive_ball_normal = disjunctive_ball_b) %>% 
  arrange(participant) %>% 
  # define exclusion criteria 
  mutate(exclude = ifelse((conjunctive_ball_abnormal < conjunctive_ball_normal) &
         (disjunctive_ball_abnormal < disjunctive_ball_normal) & 
         (conjunctive < 50) &
         (disjunctive > 50), 0, 1))

# NORMAL EXPLANATION: PRESCRIPTIVE NORMS 
df.pres_normal_data = read.csv(file = "../../data/prescriptive_normal.csv",
                   stringsAsFactors = F) %>% 
  filter(row_number() > 2) %>% # additional rows in qualtrics csv
  clean_names() %>% 
  filter(!status == "Survey Preview", # exclude preview trials, 
         !ethnicity == "") # exclude people who did not make until demographics

# reference clip: 
# Billy is the abnormal agent, Suzy the normal agent (recode Norm Condition 2)
# Conjunctive Structure is at the top scale (100) (Recode Structure Condition 'ConjLeft')

df.pres_normal_judgments = df.pres_normal_data %>% 
  mutate(participant = 1:n()) %>% 
  select(s_norm_conj_billy1_1:b_selected_conj_right_1,
         structure,
         norm_condition,
         participant,
         -starts_with("x45"),
         -starts_with("s89"),
         -starts_with("q"),
         -starts_with("check"),
         -starts_with("timing")) %>% 
  pivot_longer(cols = -c(participant, structure, norm_condition),
               names_to = "index",
               values_to = "value") %>%
  filter(value != "") %>%
  mutate(index = ifelse(str_detect(index, "selected"),
                        "judgment",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "s_norm_conj_billy|b_norm_conj_billy"),
                        "conjunctive_agreement_billy",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "s_norm_conj_suzy|b_norm_conj_suzy"),
                        "conjunctive_agreement_suzy",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "s_norm_conjunctive|b_norm_conjunctive"),
                        "conjunctive",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "s_norm_conj_exp|b_norm_conj_exp"),
                        "conjunctive_selection",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "s_norm_disj_billy|b_norm_disj_billy"),
                        "disjunctive_agreement_billy",
                        index))%>% 
  mutate(index = ifelse(str_detect(index, "s_norm_disj_suzy|b_norm_disj_suzy"),
                        "disjunctive_agreement_suzy",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "s_norm_disjunctive|b_norm_disjunctive"),
                        "disjunctive",
                        index))%>%
  mutate(index = ifelse(str_detect(index, "s_norm_disj_exp|b_norm_disj_exp"),
                        "disjunctive_selection",
                        index))%>%
  mutate(value = as.numeric(value),
         value = ifelse(test = (norm_condition == "2" & str_detect(index, "agreement")),
                        yes = 100 - value,
                        no = value),
         value = ifelse(test = (norm_condition == "2" & str_detect(index, "selection")),
                        yes = 3 - value,
                        no = value))%>% 
  pivot_wider(names_from = index,
              values_from = value) %>% 
  mutate(conjunctive_selection = factor(conjunctive_selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal"))) %>%
  mutate(disjunctive_selection = factor(disjunctive_selection,
                            levels = 1:2,
                            labels = c("abnormal", "normal")))%>%
  mutate(judgment = ifelse(structure =="ConjLeft",
                           100 - judgment,
                           judgment)) %>% 
  rename(conjunctive_agent_abnormal = conjunctive_agreement_billy,
        conjunctive_agent_normal = conjunctive_agreement_suzy,
        disjunctive_agent_abnormal = disjunctive_agreement_billy,
        disjunctive_agent_normal = disjunctive_agreement_suzy) %>% 
  arrange(participant) %>%
  # define exclusion criteria
  mutate(exclude = ifelse(
    test = (conjunctive_agent_abnormal < conjunctive_agent_normal) &
         (disjunctive_agent_abnormal < disjunctive_agent_normal) &
         (conjunctive < 50) &
         (disjunctive > 50),
    yes = 0,
    no = 1))

# COMBINE DATA 
df.structure = df.stat_abnormal_judgments %>% 
  mutate(explanation = "abnormal") %>% 
  bind_rows(df.stat_normal_judgments %>% 
              mutate(explanation = "normal")) %>% 
  rename(conjunctive_abnormal = conjunctive_ball_abnormal,
         conjunctive_normal = conjunctive_ball_normal,
         disjunctive_abnormal = disjunctive_ball_abnormal,
         disjunctive_normal = disjunctive_ball_normal) %>% 
  mutate(norm = "statistical") %>% 
  bind_rows(df.pres_abnormal_judgments %>% 
              mutate(explanation = "abnormal") %>% 
              bind_rows(df.pres_normal_judgments %>% 
                          mutate(explanation = "normal")) %>% 
              rename(conjunctive_abnormal = conjunctive_agent_abnormal,
                     conjunctive_normal = conjunctive_agent_normal,
                     disjunctive_abnormal = disjunctive_agent_abnormal,
                     disjunctive_normal = disjunctive_agent_normal) %>% 
              mutate(norm = "prescriptive")) %>% 
  filter(exclude == 0) %>% # filter based on exclusion criteria
  select(-c(norm_condition, structure)) %>% 
  rename(conjunctive_cause = conjunctive, 
         disjunctive_cause = disjunctive) %>% 
  mutate(participant = 1:n(),
         across(c(contains("disjunctive"), contains("conjunctive")), ~ as.character(.))) %>% 
  pivot_longer(names_to = c("structure", "index"),
               names_sep = "_",
               cols = c(contains("disjunctive"), contains("conjunctive"))) %>% 
  pivot_wider(names_from = index,
              values_from = value) %>% 
  mutate(across(c(cause, abnormal, normal), ~ as.numeric(.))) %>% 
  mutate(selection = factor(selection, levels = c("abnormal", "normal")),
    structure = factor(structure, levels = c("conjunctive", "disjunctive")),
         norm = factor(norm, levels = c("statistical", "prescriptive")))


## Demographics


df.structure.demo = df.stat_abnormal_data %>% 
  select(feedback,
         age_1_text,
         gender,
         race_1_text,
         ethnicity) %>% 
  mutate(norm = "statistical",
         normality = "abnormal",
         participant = 1:n()) %>% 
  left_join(df.stat_abnormal_judgments %>% 
              select(participant, exclude),
            by = "participant") %>% 
  bind_rows(df.stat_normal_data %>% 
              select(feedback,
                     age_1_text,
                     gender,
                     race_1_text,
                     ethnicity) %>% 
              mutate(norm = "statistical",
                     normality = "normal",
                     participant = 1:n()) %>% 
              left_join(df.stat_normal_judgments %>% 
                          select(participant, exclude),
                        by = "participant")) %>% 
  bind_rows(df.pres_abnormal_data %>% 
              select(feedback,
                     age_1_text,
                     gender,
                     race_1_text,
                     ethnicity) %>% 
              mutate(norm = "prescriptive",
                     normality = "abnormal",
                     participant = 1:n()) %>% 
              left_join(df.pres_abnormal_judgments %>% 
                          select(participant, exclude),
                        by = "participant")) %>% 
  bind_rows(df.pres_normal_data %>% 
              select(feedback,
                     age_1_text,
                     gender,
                     race_1_text,
                     ethnicity) %>% 
              mutate(norm = "prescriptive",
                     normality = "normal",
                     participant = 1:n()) %>% 
              left_join(df.pres_normal_judgments %>% 
                          select(participant, exclude),
                        by = "participant")) %>% 
  mutate(age = as.numeric(age_1_text),
         age = ifelse(age > 1000, 2019 - age, age),
         gender = ifelse(gender == 1, "Female", gender),
         gender = ifelse(gender == 2, "Male", gender),
         gender = ifelse(gender == 3, "Non-binary", gender),
         gender = ifelse(gender == 4, "Prefer not to say.", gender))

df.structure.demo %>% 
  summarize(n_excluded = sum(exclude == 1))

df.structure.demo %>% 
  filter(exclude == 0) %>%
  count(norm, normality) %>% 
  print_table()

df.structure.demo %>% 
  # filter(exclude == 0) %>% 
  summarize(n_total = n(),
            n_female = sum(gender == "Female"),
            n_male = sum(gender == "Male"),
            n_not_say = sum(gender == "Prefer not to say."),
            n_non_binary = sum(gender == "Non-binary"),
            mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T)) %>% 
  mutate(across(contains("age"), round)) %>% 
  pivot_longer(cols = everything()) %>% 
  print_table()

# remove unnecessary variables
rm(list = ls()[!ls() %in% c("df.normality",
                            "df.normality.demo",
                            "df.structure",
                            "df.structure.demo",
                            "print_table",
                            "fit_normality_selection",
                            "fit_normality_judgment",
                            "p.exp1.selections")])


## Plots 

### Causal selections


set.seed(1)
df.plot = df.structure %>% 
  mutate(selection = ifelse(selection == "abnormal", 1, 0),
         norm = factor(norm,
                       levels = c("statistical", "prescriptive"),
                       labels = c("statistical\nnorm", "prescriptive\nnorm")))

df.text = df.plot %>% 
  distinct(participant, norm) %>% 
  count(norm) %>% 
  mutate(label = str_c("n = ", n),
         selection = 1.05,
         structure = NA)

df.line = df.plot %>% 
  group_by(norm, structure) %>% 
  summarize(selection = mean(selection)) %>% 
  ungroup() %>% 
  mutate(x = c(0.75, 1.25, 1.75, 2.25))

p.exp2.selections = ggplot(data = df.plot,
                           mapping = aes(x = norm, 
                                         y = selection,
                                         group = structure,
                                         fill = structure)) +
  geom_hline(yintercept = 0.5, 
             linetype = 2) + 
  stat_summary(fun.data = "mean_cl_boot",
               geom = "pointrange",
               position = position_dodge(width = 0.5),
               size = 1.5,
               shape = 21,
               alpha = 0) +
  geom_text(data = df.text,
            mapping = aes(label = label),
            position = position_dodge(width = 0.5),
            size = 6) +
  labs(x = "type of norm",
       y = "% abnormal cause selections",
       title = "Experiment 2") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     labels = str_c(seq(0, 100, 25), "%"),
                     expand = expansion(mult = c(0.05, 0))) +
  coord_cartesian(clip = "off",
                  ylim = c(0, 1)) + 
  theme(text = element_text(size = 24),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 1, l = 0.2, r = 0.2, unit = "cm"),
        plot.title = element_text(vjust = 4,
                                  color = "gray40",
                                  hjust = 0.5))

# hack to link the dodged points:
# https://stackoverflow.com/questions/51737201/how-to-draw-line-between-dodged-geometry-in-ggplot

p.exp2.selections = p.exp2.selections + geom_line(data = layer_data(p.exp2.selections, 2L),
                              mapping = aes(x = x,
                                            y = y,
                                            group = rep(1:2, 2),
                                            fill = NA),
                              size = 1,
                              linetype = 1) + 
  stat_summary(fun.data = "mean_cl_boot",
               geom = "pointrange",
               position = position_dodge(width = 0.5),
               size = 1.5,
               shape = 21)

p.exp2.selections

ggsave(filename = "../../figures/plots/structure_selections.pdf",
       plot = p.exp2.selections,
       width = 8,
       height = 6)


### Main judgment data 


set.seed(1)

df.plot = df.structure %>% 
  select(participant, norm, explanation, judgment) %>% 
  distinct() %>% 
  mutate(norm = factor(norm,
                       levels = c("statistical", "prescriptive"),
                       labels = c("statistical\nnorm", "prescriptive\nnorm")),
         judgment = judgment - 50)

df.text = df.plot %>% 
  count(explanation, norm) %>% 
  mutate(label = str_c("n = ", n),
         judgment = 55)

p.exp2.judgments = ggplot(data = df.plot,
       mapping = aes(x = norm, 
                     y = judgment,
                     group = explanation,
                     fill = explanation)) +
  geom_hline(yintercept = 0, 
             linetype = 2) + 
  geom_point(mapping = aes(color = explanation), 
             alpha = 0.2,
             size = 2,
             position = position_jitterdodge(jitter.width = 0.1,
                                             dodge.width = 0.5),
             show.legend = F) + 
  stat_summary(fun.data = "mean_cl_boot",
               geom = "pointrange",
               position = position_dodge(width = 0.5),
               size = 1.5,
               shape = 21) + 
  annotate(x = c(0.5, 0.5),
           y = c(-50, 50),
           geom = "text",
           label = c("preference for\ndisjunctive structure", "preference for\nconjunctive structure"),
           hjust = c(0, 1),
           angle = 90,
           size = 5) + 
  geom_text(data = df.text,
            mapping = aes(label = label),
            position = position_dodge(width = 0.5),
            size = 5) + 
  labs(x = "type of norm",
       y = "structure inference") + 
  scale_fill_manual(values = c("#984EA3", "#4DAF4A")) +
  scale_color_manual(values = c("#984EA3", "#4DAF4A")) +
  scale_y_continuous(breaks = seq(-50, 50, 25),
                     labels = seq(-50, 50, 25),
                     expand = expansion(mult = c(0.05, 0))) + 
  coord_cartesian(clip = "off",
                  ylim = c(-50, 50)) +
  theme(text = element_text(size = 24),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        plot.margin = margin(t = 1, l = 0.2, r = 0.2, unit = "cm"))

ggsave("../../figures/plots/structure_judgments.pdf",
       width = 8,
       height = 6)


### Individual differences 


set.seed(1)

df.plot = df.structure %>% 
  select(participant, explanation, judgment, norm, structure, selection) %>% 
  pivot_wider(names_from = "structure",
              values_from = "selection") %>%
  mutate(disjunctive = str_c("D: ", disjunctive),
         conjunctive = str_c("C: ", conjunctive)) %>% 
  unite("selection", c(disjunctive, conjunctive), sep = "&", remove = F) %>% 
  mutate(judgment = judgment - 50,
         selection = str_replace(selection, "&", "\n"))

df.text = df.plot %>% 
  count(selection) %>% 
  mutate(judgment = 55,
         label = str_c("n = ", n),
         explanation = NA)

p.exp2.individual = ggplot(data = df.plot,
                           mapping = aes(x = selection, 
                                         y = judgment,
                                         group = explanation,
                                         fill = explanation)) +
  geom_hline(yintercept = 0, 
             linetype = 2) + 
  geom_point(mapping = aes(color = explanation),
             alpha = 0.2,
             size = 2,
             position = position_jitterdodge(jitter.width = 0.1,
                                             dodge.width = 0.5),
             show.legend = F) + 
  stat_summary(fun.data = "mean_cl_boot",
               geom = "pointrange",
               position = position_dodge(width = 0.5),
               size = 1.5,
               shape = 21) + 
  annotate(x = c(0.54, 0.54),
           y = c(-50, 50),
           geom = "text",
           label = c("preference for\ndisjunctive structure", "preference for\nconjunctive structure"),
           hjust = c(0, 1),
           angle = 90,
           size = 4.5) + 
  geom_text(data = df.text,
            mapping = aes(label = label),
            position = position_dodge(width = 0.5),
            size = 5) +
  labs(x = "participants' causal selections",
       y = "structure inference") + 
  scale_fill_manual(values = c("#984EA3", "#4DAF4A")) +
  scale_color_manual(values = c("#984EA3", "#4DAF4A")) +
  scale_y_continuous(breaks = seq(-50, 50, 25),
                     labels = seq(-50, 50, 25),
                     expand = expansion(mult = c(0.05, 0))) + 
  coord_cartesian(clip = "off",
                  ylim = c(-50, 50)) +
  theme(text = element_text(size = 24),
        legend.position = "bottom",
        axis.title.x = element_text(margin = margin(t = 0.5, unit = "cm")),
        plot.margin = margin(t = 1, l = 0.2, r = 0.2, unit = "cm"))

p.exp2.individual

ggsave(filename = "../../figures/plots/structure_judgments_based_on_selections.pdf",
       plot = p.exp2.individual,
       width = 8,
       height = 6)


## Stats 

### Causal selections 


df.structure %>% 
  count(structure, selection) %>% 
  group_by(structure) %>% 
  mutate(perc = n/sum(n)) %>% 
  filter(selection == "abnormal") %>% 
  select(-n) %>% 
  print_table()


### Means and standard deviations 


df.structure %>% 
  group_by(explanation) %>% 
  summarize(mean = mean(judgment),
            sd = sd(judgment)) %>% 
  print_table()



### Bayesian regression on selections


fit_structure_selection = brm(formula = selection ~ 1 + structure * norm + (1 | participant),
                              family = "bernoulli",
                              data = df.structure,
                              seed = 1,
                              cores = 2,
                              file = "cache/fit_structure_selection")
fit_structure_selection

pp_check(fit_structure_selection, ndraws = 100, type = "scatter_avg")
ppc_stat(y, yrep, stat = "median") + grid_lines()
brms::mcmc_plot(fit_structure_selection)
brms::mcmc_plot(fit_normality_selection)

vcov(fit_normality_selection)
### Bayesian regression on judgment data


fit_structure_judgment = brm(formula = judgment ~ explanation * norm,
                             data = df.structure %>% 
                               select(participant, norm, explanation, judgment) %>% 
                               distinct(),
                             seed = 1,
                             cores = 2,
                             file = "cache/fit_structure_judgment")
fit_structure_judgment

pp_check(fit_structure_judgment, ndraws = 100)


## Tables

### Causal selections 


fit_structure_selection %>% 
  tidy(conf.method = "HPDinterval",
         fix.intercept = F) %>% 
    filter(effect == "fixed") %>% 
    mutate(term = tolower(term),
           term = str_remove_all(term, "1"),
           across(where(is.numeric), ~ round(., 2))) %>% 
    rename(`lower 95% CI` = conf.low,
           `upper 95% CI` = conf.high) %>% 
    select(-c(effect:group,std.error)) %>% 
  print_table()


### Structure judgments 


fit_structure_judgment %>% 
  gather_draws(b_Intercept, 
               b_explanation1,
               b_norm1,
               `b_explanation1:norm1`) %>% 
  mean_hdi() %>% 
  rename(term = .variable,
         estimate = .value,
         `lower 95% CI` = .lower,
          `upper 95% CI` = .upper) %>% 
  select(-c(.width, .point, .interval)) %>% 
  mutate(term = tolower(term),
         term = str_remove_all(term, "b_"),
         term = str_remove_all(term, "1"),
         term = factor(term, levels = c("intercept", "explanation", "norm", "explanation:norm"))) %>% 
  arrange(term) %>% 
  print_table()


### Selection patterns


df.structure %>% 
  select(participant, judgment, norm, structure, selection) %>% 
  pivot_wider(names_from = "structure",
              values_from = "selection") %>% 
  count(disjunctive, conjunctive) %>% 
  print_table()


### Difference scores in structure inference based on causal selections 


df.structure %>% 
  select(participant, explanation, judgment, norm, structure, selection) %>% 
  pivot_wider(names_from = "structure",
              values_from = "selection") %>%
  mutate(disjunctive = str_c("D: ", disjunctive),
         conjunctive = str_c("C: ", conjunctive)) %>% 
  unite("selection", c(disjunctive, conjunctive), sep = " & ", remove = F) %>% 
  group_by(explanation, selection) %>% 
  summarize(judgment = mean(judgment),
            n = n()) %>% 
  pivot_wider(names_from = explanation,
              values_from = c(judgment, n)) %>% 
  ungroup() %>% 
  mutate(difference = judgment_abnormal - judgment_normal,
         n = n_abnormal + n_normal) %>% 
  select(-c(n_abnormal, n_normal)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2))) %>% 
  print_table()


# Comparisons 


# experiment 1: percentage of choosing the abnormal cause as a function of causal structure
df.normality %>% 
  group_by(structure) %>% 
  summarize(percentage = sum(selection == "abnormal")/n()) %>% 
  mutate(across(where(is.numeric), ~ round(., 2) * 100))

# experiment 1: normality inference as a function of causal structure 
df.normality %>% 
  group_by(structure) %>% 
  summarize(judgment_mean = mean(judgment),
            judgment_sd = sd(judgment)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

# experiment 2: percentage of choosing the abnormal cause as a function of causal structure
df.structure %>% 
  group_by(structure) %>% 
  summarize(percentage = sum(selection == "abnormal")/n()) %>% 
  mutate(across(where(is.numeric), ~ round(., 2) * 100))

# experiment 2: structure inference as a function of normality
df.structure %>% 
  group_by(explanation) %>% 
  summarize(judgment_mean = mean(judgment),
            judgment_sd = sd(judgment)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))


# Appendix 

## Frequentist analysis 

### Experiment 1

#### Causal selections 


# model fit 


# inconsistency between bayesian and freq analysis. by participant random slope missing here.
# power analysis done post stats


ffit_normality_selection = glm(formula = selection ~ structure * norm,
                               family = "binomial",
                               data = df.normality)
summary(ffit_normality_selection)
#plot(ffit_normality_selection)




performance::model_performance(ffit_normality_selection)
performance::check_model(ffit_normality_selection)

sjPlot::plot_model(ffit_normality_selection, type = "pred")

ffit_normality_selection_null = glm(formula = selection ~ 1,
                               family = "binomial",
                               data = df.normality)
summary(ffit_normality_selection_null)

ffit_normality_selection_m1 = glm(formula = selection ~ structure,
                               family = "binomial",
                               data = df.normality)
summary(ffit_normality_selection_m1)

ffit_normality_selection_m2 = glm(formula = selection ~ norm,
                               family = "binomial",
                               data = df.normality)
summary(ffit_normality_selection_m2)

ffit_normality_selection_m3 = glm(formula = selection ~ structure + norm,
                               family = "binomial",
                               data = df.normality)
summary(ffit_normality_selection_m3)

ffit_normality_selection_m4 = glm(formula = selection ~ structure : norm,
                               family = "binomial",
                               data = df.normality)
summary(ffit_normality_selection_m4)



# analysis of deviance & effect size
aov_normality_selection = Anova(ffit_normality_selection,
                                type = 3) %>% 
  as_tibble(rownames = "Term") %>% 
  mutate(cramers_v = map(`LR Chisq`, ~ chisq_to_cramers_v(chisq = .,
                                                          n = nrow(df.normality),
                                                          nrow = 2, 
                                                          ncol = 2))) %>% 
  unnest(cramers_v)

aov_normality_selection %>% 
  print_table()



##### Power analysis 


table_normality_selection = table(df.normality$structure,
                                  df.normality$selection)

pwr.chisq.test(w = table_normality_selection %>% 
                 chisq.test() %>% 
                 .[1] %>% 
                 as.numeric(),
               N = sum(table_normality_selection),
               df = 1)


#### Normality inference 


# model fit 
ffit_normality_judgment = lm(formula = judgment ~ structure * norm,
                             data = df.normality)

plot(ffit_normality_judgment)

# anova 
aov_normality_judgment = Anova(ffit_normality_judgment,
                               type = 3)

aov_normality_judgment %>%
  as_tibble(rownames = "Term") %>% 
  print_table()

# partial eta squared 
eta_squared(ffit_normality_judgment,
            ci = 0.95) %>% 
  print_table()


##### Power analysis 


# post-hoc power analysis 
pwr.anova.test(k = 2,
               n = df.normality %>% 
                 count(structure) %>%
                 .$n %>% 
                 min(),
               f = aov_normality_judgment$`F value`[1])



### Experiment 2

#### Causal selections 


# model fit 
ffit_structure_selection = glmer(formula = selection ~ 1 + structure * norm + (1 | participant),
                                 family = "binomial",
                                 data = df.structure)

# analysis of deviance & effect size
aov_structure_selection = Anova(ffit_structure_selection,
                                type = 3) %>% 
  as_tibble(rownames = "Term") %>% 
  mutate(cramers_v = map(`Chisq`, ~ chisq_to_cramers_v(chisq = .,
                                                          n = nrow(df.structure),
                                                          nrow = 2, 
                                                          ncol = 2))) %>% 
  unnest(cramers_v)

aov_structure_selection %>% 
  print_table()

##### Power analysis 


# post-hoc power analysis on the effect of structure on selections
table_structure_selection = table(df.structure$structure,
                                  df.structure$selection)

pwr.chisq.test(w = table_structure_selection %>% 
                 chisq.test() %>% 
                 .[1] %>% 
                 as.numeric(),
               N = sum(table_normality_selection),
               df = 1)


#### Structure inference
 


# model fit 

# Why simple linear model instead of glm?

ffit_structure_judgment = lm(formula = judgment ~ explanation * norm,
                             data = df.structure %>% 
                               select(participant,
                                      norm,
                                      explanation,
                                      judgment) %>% 
                               distinct())

# anova 
aov_structure_judgment = Anova(ffit_structure_judgment,
                               type = 3)

aov_structure_judgment %>%
  as_tibble(rownames = "Term") %>% 
  print_table()
  
# partial eta squared 
eta_squared(ffit_structure_judgment,
            ci = 0.95) %>% 
  print_table()


##### Power analysis 


# post-hoc power analysis 
pwr.anova.test(k = 2,
               n = df.structure %>% 
                 select(participant, norm, explanation, judgment) %>% 
                 distinct() %>% 
                 count(explanation) %>%
                 .$n %>% 
                 min(),
               f = aov_structure_judgment$`F value`[1])


# Combined plots 

## Selections 


p.exp1.selections + p.exp2.selections + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(face = "bold"),
        plot.margin = margin(t = 0.1, r = 0.1, l = 0.1, b = 0, unit = "cm"))

ggsave(filename = "../../figures/plots/selections.pdf",
       width = 16,
       height = 6)


write.csv(df.normality, "dfnorm.csv", row.names = F)

write.csv(df.structure, "dfstructure.csv", row.names = F)
# Session Info 


sessionInfo()
skimr::skim(df.structure)


