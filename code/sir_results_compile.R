

#        rm(list = ls())
#        .rs.restartR()

library(tidyverse)
library(readxl)
library(here)

source("~/prep_need/code/prep_model_sir_fun.R")
source("~/prep_need/code/calibration_data.R")

directory <- "~/prep_need/sir_results"
files <- list.files(path = directory, pattern = "^sir_.*\\.RData$", full.names = TRUE)

#files <- files[c(1:3, 5:21)]
#     print(files)

params_nat_all <- ll_out_logs <- ll_all <- NULL
st_msm_pr_all <- st_wsm_pr_all <- st_msw_pr_all <- st_pwid_pr_all <- NULL
st_msm_vs_all <- st_wsm_vs_all <- st_msw_vs_all <- st_pwid_vs_all <- NULL

for (file in files) {
  # Load the file
  load(file)
  
  # Store the loaded variables into the lists
  params_nat_all <- rbind(params_nat_all, priors$params_nat)
  
  st_msm_pr_all  <- rbind(st_msm_pr_all , priors$params_st_msm_pr)
  st_wsm_pr_all  <- rbind(st_wsm_pr_all , priors$params_st_wsm_pr)
  st_msw_pr_all  <- rbind(st_msw_pr_all , priors$params_st_msw_pr)
  st_pwid_pr_all <- rbind(st_pwid_pr_all , priors$params_st_pwid_pr)
  st_msm_vs_all  <- rbind(st_msm_vs_all , priors$params_st_msm_vs)
  st_wsm_vs_all  <- rbind(st_wsm_vs_all , priors$params_st_wsm_vs)
  st_msw_vs_all  <- rbind(st_msw_vs_all , priors$params_st_msw_vs)
  st_pwid_vs_all <- rbind(st_pwid_vs_all , priors$params_st_pwid_vs)
  
  ll_out_logs <- rbind(ll_out_logs, ll_out$llm)
  ll_all <- c(ll_all, ll_out$exp_ll)
}


# prop_w <- ll_all / sum(ll_all)
# hist(prop_w, breaks = nsim/10)
# Resample based on the importance weights
# 

adjust_ll <- exp( ll_out_logs[,1]/10+   # age
                    ll_out_logs[,2]/10+ # race
                    ll_out_logs[,3]/30+ # trnsm
                    ll_out_logs[,4]/20+ # age_msm
                    ll_out_logs[,5]/20+  #state
                    ll_out_logs[,6]/10+ # state_nd
                    ll_out_logs[,7]/10+ # nat
                    ll_out_logs[,8]/10)  # pwid

prop_w <- adjust_ll / sum(adjust_ll)

set.seed(0.1)
sir_id <- sample(1:length(ll_all), size = 400, replace = TRUE, prob = prop_w)
table(sir_id)

sir_ll <- adjust_ll[sir_id]
prop_w[sir_id]

# hist(sir_ll, breaks =10)
# 
pnat <- params_nat_all [sir_id,]
pr.msm <- st_msm_pr_all[sir_id,]
pr.wsm <- st_wsm_pr_all[sir_id,]
pr.msw <- st_msw_pr_all[sir_id,]
pr.pwid <- st_pwid_pr_all[sir_id,]
vs.msm <-  st_msm_vs_all[sir_id,]
vs.wsm <-  st_wsm_vs_all[sir_id,]
vs.msw <-  st_msw_vs_all[sir_id,]
vs.pwid <- st_pwid_vs_all[sir_id,]

source("~/prep_need/code/calibration_figures.R")


idp <- pnat[,1] >0.25
pnat <- pnat [idp,]
pr.msm <- pr.msm[idp,]
pr.wsm <- pr.wsm[idp,]
pr.msw <- pr.msw[idp,]
pr.pwid <- pr.pwid[idp,]
vs.msm <-  vs.msm[idp,]
vs.wsm <-  vs.wsm[idp,]
vs.msw <-  vs.msw[idp,]
vs.pwid <- vs.pwid[idp,]

###

write.csv(pnat,    here("output_data/pnat.csv"), row.names = FALSE)
write.csv(pr.msm,  here("output_data/pr_msm.csv"), row.names = FALSE)
write.csv(pr.wsm,  here("output_data/pr_wsm.csv"), row.names = FALSE)
write.csv(pr.msw,  here("output_data/pr_msw.csv"), row.names = FALSE)
write.csv(pr.pwid,  here("output_data/pr_pwid.csv"), row.names = FALSE)
write.csv(vs.msm,  here("output_data/vs_msm.csv"), row.names = FALSE)
write.csv(vs.wsm,  here("output_data/vs_wsm.csv"), row.names = FALSE)
write.csv(vs.msw,  here("output_data/vs_msw.csv"), row.names = FALSE)
write.csv(vs.pwid,  here("output_data/vs_pwid.csv"), row.names = FALSE)

