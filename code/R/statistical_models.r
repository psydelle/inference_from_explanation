## DOCUMENT DETAILS ----------------------------------------------------------
#
# Project: DR. NLP Replication Study
# Working Title: Replicating Kirfel et al. (2022)
# Authors: Sydelle de Souza & Ivan Vegner
# Created: 2022/10/29 # nolint
# R version: 4.1.3 (2022-03-10) -- "One Push-Up"

#-----------------------------------------------------------------------------#

## COMMENTS -------------------------------------------------------------------
#
# Kirfel, L., Icard, T., & Gerstenberg, T. (2022). Inference from explanation.
# Journal of Experimental Psychology: General, 151(7), 1481–1501.
# https://doi.org/10.1037/xge0001151
#
# this file contains the statistical analysis code forked from 
# Kirfel et al.'s (2022) github repo 
# (https://github.com/cicl-stanford/inference_from_explanation.git)
# alongside the code for our own analysis.
# the objective here is to reproduce their statistical results, check their
# assumptions, and run our own staistical analyses.
#
# comments with #### were left by Kirfel et al., everything else is ours.

#-----------------------------------------------------------------------------#

## SET UP ---------------------------------------------------------------------
#
# load libraries
# we do no need all these libraries for the running the model, but loading them 
# as their code is quite messy and has unconventional dependencies,
# will comment them out later
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

# added some of my own packages
#
library("here")        # for setting up project-oriented workflow
library("skimr")       # for quick descriptives
library("xtable")      # for nicer latex tables
library("ggpubr")      # fpr publication ready plots

## load dataset for experiment 1
#
df.norm <- read.csv(here("dfnorm.csv"))

head(df.norm)

nrow(df.norm) # no. of observations = 154

## load dataset for experiment 2
#
df.struc <- read.csv(here("dfstructure.csv"))

head(df.struc)

nrow(df.struc) # no. of observations = 286

## skimming to look for missing data and get a descriptive overview
#
skim(df.norm) # alles ist in ordnung

skim(df.struc) # kein ordnung hier! @vanya, please check

view(df.struc)

### use effects contrast coding to interpret effects from categorical variables  
#   as main effects
#
options(contrasts = c("contr.sum", "contr.poly"))

#-----------------------------------------------------------------------------#


## ORIGINAL ANALYSIS-----------------------------------------------------------

## EXPERIMENT 1: first, let's run the the models as is and check if the results 
#  match the ones reported in the paper
#

### Bayesian regression on selection data

fit_normality_selection = brm(formula = selection ~ structure * norm,
                              family = "bernoulli",
                              data = df.norm %>%
                                mutate(selection = ifelse(selection == 
                                                            "normal", 
                                                             0, 1)),
                              seed = 1,
                              cores = 4,
                              file = "cache/fit_normality_selection_test")


summary(fit_normality_selection)

## NOTE: in the above model, only half the dataset is being used (see data = )
#  @vanya, can you look at the paper and figure out why? it's prob nothing,
#  but worth checking

### Bayesian regression on judgment data

fit_normality_judgment = brm(formula = judgment ~ structure * norm,
                             data = df.norm,
                             seed = 1,
                             cores = 2,
                             file = "cache/fit_normality_judgment_t")

summary(fit_normality_judgment)

## CHECKPOINT: the results match the tables in the paper

## generate model summaries as a latex tables
#
xtable(fixef(fit_normality_selection), 
            caption = "exp 1 regression results for selection")

xtable(fixef(fit_normality_judgment), 
       caption = "exp 1 regression results for judgment")


## EXPERIMENT 2: let's run the the models as is and check if the results 
#  match the ones reported in the paper
#

### Bayesian regression on selections

fit_structure_selection = brm(formula = selection ~ 1 + structure * norm 
                              + (1 | participant), # random intercept
                              family = "bernoulli",
                              data = df.struc,
                              seed = 1,
                              cores = 2,
                              file = "cache/fit_structure_selection")

summary(fit_structure_selection)


### Bayesian regression on judgment data


fit_structure_judgment = brm(formula = judgment ~ explanation * norm,
                             data = df.struc %>% 
                               select(participant, norm, explanation, judgment) %>% 
                               distinct(),
                             seed = 1,
                             cores = 2,
                             file = "cache/fit_structure_judgment")

summary(fit_structure_judgment)

## NOTE: in the above model, n = 143 (see data = )
#  @vanya, can you look at the paper and figure out why? worth checking
#  solved. subjects provide 1 response to slidey scale and 2 for selection

## CHECKPOINT: these results also match the tables in the paper

#-----------------------------------------------------------------------------#

## DESCRIPTIVES ---------------------------------------------------------------

##  now let's get our hands dirty and look at their data. 
#   we're going to start with visual descriptive checks, 
#   get summaries of the priors, and perform posterior predictive checks
#  

## start by converting all char columns to factors for both experiments
#

df.norm <- as.data.frame(unclass(df.norm),  
                       stringsAsFactors = TRUE)

df.norm$selection_num <- ifelse(df.norm$selection == "normal", 0, 1)

sum(df.norm$selection_num) # abnormal = 93
nrow(df.norm) - sum(df.norm$selection_num) # normal = 61

hist(df.norm$selection_num) # binary, as expected
gghistogram(df.norm$judgment) # u-shaped with zeros and ones inflated (scale 0-100)


df.struc <- as.data.frame(unclass(df.struc),  
                         stringsAsFactors = TRUE)
skim(df.struc)


df.struc$selection_num <- ifelse(df.struc$selection == "normal", 0, 1)

sum(df.struc$selection_num) # abnormal = 171
nrow(df.struc) - sum(df.struc$selection_num) # normal = 115

hist(df.struc$selection_num) # binary, as expected
hist(df.struc$judgment) # u-shaped with zeros and ones inflated (scale 0-100)

## CHECKPOINT: @vanya, can you check if this jives with what they report?

#-----------------------------------------------------------------------------#

## MODEL CHECKS ---------------------------------------------------------------

## first we look at the predictive posterior distributions to see how well the 
#  model has fit. then we look at whether the priors specified are informative, 
#  and examine the mcmc plots
#

pp_check(fit_normality_selection, ndraws = 1000)
pp_check(fit_normality_judgment, ndraws = 1000)

pp_check(fit_structure_selection, ndraws = 1000)
pp_check(fit_structure_judgment, ndraws = 1000)


prior_summary(fit_normality_selection)
prior_summary(fit_normality_judgment)

prior_summary(fit_structure_selection)
prior_summary(fit_structure_judgment)

mcmc_plot(fit_normality_selection)
mcmc_plot(fit_normality_judgment)

mcmc_plot(fit_structure_selection) 
mcmc_plot(fit_structure_judgment)


## we found that 

