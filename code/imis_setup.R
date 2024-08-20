
library(here)
library(tidyverse)
library(tidycensus)
library(readxl)
source("~/prep_need/code/imis_fun.R")

source(here('code/acs_fun.R'))

# populations sizes needed for the model outpus
pop_asr <- read_csv(here("data/acs5_state_sex_age_race.csv"))

source("~/prep_need/code/prep_model_pars_imis.R")
imis_out <- IMIS(B=1000, B.re=3000, number_k=100, D=0)
