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

## load dataset for experiment 1
#
df.norm <- read.csv(here("dfnorm.csv"))

head(df.norm)

nrow(df.norm) # no. of observations = 154

## load dataset for experiment 2
#
df.structure <- read.csv(here("dfstructure.csv"))

head(df.structure)

nrow(df.structure) # no. of observations = 286

## skimming to look for missing data and get a descriptive overview
#
skim(df.norm) # alles ist in ordnung

skim(df.struc) # kein ordnung hier! 

view(df.struc)

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

## CHECKPOINT: the results match
#

## NOTE: in the above model, only half the dataset is being used (see data = )
#  @vanya, can you look at the paper and figure out why? it's prob nothing,
#  but worth checking

### Bayesian regression on judgment data

fit_normality_judgment = brm(formula = judgment ~ structure * norm,
                             data = df.norm,
                             seed = 1,
                             cores = 2,
                             file = "cache/fit_normality_judgment_test")

summary(fit_normality_judgment)


## generate model summaries as a latex tables

xtable(fixef(fit_normality_selection), 
            caption = "exp 1 regression results for selection")

xtable(fixef(fit_normality_judgment), 
       caption = "exp 1 regression results for judgment")




#-----------------------------------------------------------------------------#




## EXPERIMENT 2 ---------------------------------------------------------------


#-----------------------------------------------------------------------------#



#  now let's get our hands dirty and look at their data. we're going to start
#  visual descriptive checks, get summaries of the priors, and perform posterior 
#  predictive checks

prior_summary(fit_normality_judgment)
fit_normality_judgment
pp_check(fit_normality_judgment)
??pp_check
