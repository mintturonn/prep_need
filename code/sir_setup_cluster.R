
#        rm(list = ls())
#        .rs.restartR()

library(dplyr)
library(tidyr)
library(readr)
library(readxl)

# source(here('code/acs_fun.R')) ## old ACS definition
source("~/prep_need/code/prep_model_pars_sir.R")
source("~/prep_need/code/calibration_data.R")
source("~/prep_need/code/prior_sir_funs.R")
source("~/prep_need/code/prep_model_sir_fun.R")
source("~/prep_need/code/ll_sir_fun.R")

# populations sizes needed for the model outpus
read_csv("~/prep_need/data/acs5_state_sex_age_race.csv") %>%
  mutate(state = gsub("_", " ", state)) -> pop_asr

pop_asr %>%
  filter(sex=="Male") %>%
  left_join(msmpr, by = ("state")) %>%
  mutate( pop = n *prev_extract ) %>%
  dplyr::select(-n, -n_se, -prev_extract) -> asr_msm

pop_asr %>%
  filter(sex=="Male") %>%
  left_join(msmpr, by = ("state")) %>%
  mutate( pop = n *(1-prev_extract) ) %>%
  dplyr::select(-n, -n_se, -prev_extract) -> asr_msw

pop_asr %>%
  filter(sex=="Female") %>%
  rename(pop = n) %>%
  dplyr::select(-n_se) -> asr_wsm


# start_time <- Sys.time()  
nsim <- 200000
# Sys.time() - start_time

priors <-  sample.prior(nsim, pars_nat, pars_state, id_beta, id_lnrm, id_gamma, id_nrm, id_unif, id_fixed)

print("priors selected")

ll_out <- likelihood(priors)

print("ll calcs")

temp <- Sys.time()

save(priors, ll_out, file = paste0("sir_", temp, ".RData"))

q()
