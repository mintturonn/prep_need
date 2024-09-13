
#        rm(list = ls())
#        .rs.restartR()
library(here)
library(tidyverse)
library(tidycensus)
library(readxl)
# source("~/prep_need/code/imis_fun.R")

source(here('code/acs_fun.R')) ## old ACS definition
source("~/prep_need/code/prep_model_pars_imis.R")
source("~/prep_need/code/calibration_data.R")
source("~/prep_need/code/prior_sir_funs.R")
source("~/prep_need/code/prep_model_sir_fun.R")
source("~/prep_need/code/ll_sir_fun.R")

# populations sizes needed for the model outpus
read_csv(here("data/acs5_state_sex_age_race.csv")) %>%
  mutate(state = gsub("_", " ", state)) -> pop_asr

pop_asr %>%
  filter(sex=="Male") %>%
  left_join(msmpr, by = ("state")) %>%
  mutate( pop = n *prev_extract ) %>%
  select(-n, -n_se, -prev_extract) -> asr_msm

pop_asr %>%
  filter(sex=="Male") %>%
  left_join(msmpr, by = ("state")) %>%
  mutate( pop = n *(1-prev_extract) ) %>%
  select(-n, -n_se, -prev_extract) -> asr_msw

pop_asr %>%
  filter(sex=="Female") %>%
  rename(pop = n) %>%
  select(-n_se) -> asr_wsm


start_time <- Sys.time()  
nsim <- 400000
Sys.time() - start_time

priors <-  sample.prior(nsim, pars_nat, id_beta, id_lognr,  id_unif, id_fixed, pars_state)
ll_out <- likelihood(priors)

Sys.time() - start_time

prop_w <- ll_out / sum(ll_out)

# ll_out_all <- c(ll_out, ll_out2, ll_out3)
# prop_w <- ll_out_all / sum(ll_out_all)

# hist(prop_w, breaks = nsim/10)
# Resample based on the importance weights
sir_id <- sample(1:length(ll_out), size = 10000000, replace = TRUE, prob = prop_w)
table(sir_id)
# sir_id <- unique(sir_id)[1:5]
sir_ll <- ll_out[sir_id]
prop_w[sir_id]

# hist(sir_ll, breaks =10)
# 
pnat <- rbind(priors$params_nat[sir_id,],         priors2$params_nat) #[sir_id,]
pr.msm <- rbind(priors$params_st_msm_pr[sir_id,], priors2$params_st_msm_pr) #[sir_id,]
pr.wsm <- rbind(priors$params_st_wsm_pr[sir_id,], priors2$params_st_wsm_pr) # [sir_id,]
pr.msw <- rbind(priors$params_st_msw_pr[sir_id,], priors2$params_st_msw_pr) #[sir_id,]
vs.msm <- rbind(priors$params_st_msm_vs[sir_id,], priors2$params_st_msm_vs) #[sir_id,]
vs.wsm <- rbind(priors$params_st_wsm_vs[sir_id,], priors2$params_st_wsm_vs) #[sir_id,]
vs.msw <- rbind(priors$params_st_msw_vs[sir_id,], priors2$params_st_msw_vs) #[sir_id,]
#
pnat <- priors$params_nat[sir_id,]
pr.msm <- priors$params_st_msm_pr[sir_id,]
pr.wsm <- priors$params_st_wsm_pr[sir_id,]
pr.msw <- priors$params_st_msw_pr[sir_id,]
vs.msm <- priors$params_st_msm_vs[sir_id,]
vs.wsm <- priors$params_st_wsm_vs[sir_id,]
vs.msw <- priors$params_st_msw_vs[sir_id,]

write.csv(pnat,    here("output_data/pnat.csv"), row.names = FALSE)
write.csv(pr.msm,  here("output_data/pr_msm.csv"), row.names = FALSE)
write.csv(pr.wsm,  here("output_data/pr_wsm.csv"), row.names = FALSE)
write.csv(pr.msw,  here("output_data/pr_msw.csv"), row.names = FALSE)
write.csv(vs.msm,  here("output_data/vs_msm.csv"), row.names = FALSE)
write.csv(vs.wsm,  here("output_data/vs_wsm.csv"), row.names = FALSE)
write.csv(vs.msw,  here("output_data/vs_msw.csv"), row.names = FALSE)

priors2 <- NULL
priors2$params_nat <- as.matrix(read_csv(here("output_data/pnat.csv")))
priors2$params_st_msm_pr <- as.matrix(read_csv(here("output_data/pr_msm.csv")))
priors2$params_st_wsm_pr <- as.matrix(read_csv(here("output_data/pr_wsm.csv")))
priors2$params_st_msw_pr <- as.matrix(read_csv(here("output_data/pr_msw.csv")))
priors2$params_st_msm_vs <- as.matrix(read_csv(here("output_data/vs_msm.csv")))
priors2$params_st_wsm_vs <- as.matrix(read_csv(here("output_data/vs_wsm.csv")))
priors2$params_st_msw_vs <- as.matrix(read_csv(here("output_data/vs_msw.csv")))
