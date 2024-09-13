

# define sample.prior to sample from the prior
######################################################
sample.prior <- function (B0, pars_nat, id_beta, id_lognr, id_unif, id_fixed, pars_state) {

  params0 <- array(dim = c(B0, length(pars_nat$params)), dimnames = list(NULL, pars_nat$params))
  params.msm1 <-   array(dim = c(B0, length(pars_state$state)), dimnames = list(NULL, paste0(pars_state$state, "_pr.msm") ))
  params.wsm1 <-   array(dim = c(B0, length(pars_state$state)), dimnames = list(NULL, paste0(pars_state$state, "_pr.wsm") )) 
  params.msw1 <-   array(dim = c(B0, length(pars_state$state)), dimnames = list(NULL, paste0(pars_state$state, "_pr.msw") ))
  params.msm2 <-   array(dim = c(B0, length(pars_state$state)), dimnames = list(NULL, paste0(pars_state$state, "_vs.msm") ))
  params.wsm2 <-   array(dim = c(B0, length(pars_state$state)), dimnames = list(NULL, paste0(pars_state$state, "_vs.wsm") ))
  params.msw2 <-   array(dim = c(B0, length(pars_state$state)), dimnames = list(NULL, paste0(pars_state$state, "_vs.msw") ))

  for (i in id_beta) {
    params0[,i] <- rbeta(B0, as.numeric(pars_nat$shape1[i]), as.numeric(pars_nat$shape2[i]))
  }
  
  for (i in id_lnrm) {
    params0[,i] <- rlnorm(B0, as.numeric(pars_nat$shape1[i]), as.numeric(pars_nat$shape2[i]))
  }
  
  for (i in id_unif) {
    params0[,i] <- runif(B0, as.numeric(pars_nat$shape1[i]), as.numeric(pars_nat$shape2[i]))
  }
  
 
  params0[,"prep.msm2"] <- params0[,"prep.msm1"] * pars_nat$shape1[pars_nat$params=="prep.msm2"]
  params0[,"prep.msm3"] <- params0[,"prep.msm1"] * pars_nat$shape1[pars_nat$params=="prep.msm3"]
  params0[,"prep.msm5"] <- params0[,"prep.msm4"] * pars_nat$shape1[pars_nat$params=="prep.msm5"]
  
  params0[,"prep.wsm2"] <- params0[,"prep.wsm1"] * pars_nat$shape1[pars_nat$params=="prep.wsm2"]
  params0[,"prep.wsm3"] <- params0[,"prep.wsm1"] * pars_nat$shape1[pars_nat$params=="prep.wsm3"]
  params0[,"prep.wsm4"] <- params0[,"prep.wsm1"] * pars_nat$shape1[pars_nat$params=="prep.wsm4"]
  params0[,"prep.wsm5"] <- params0[,"prep.wsm1"] * pars_nat$shape1[pars_nat$params=="prep.wsm5"]
  
  params0[,"prep.msw2"] <- params0[,"prep.msw1"] * pars_nat$shape1[pars_nat$params=="prep.msw2"]
  params0[,"prep.msw3"] <- params0[,"prep.msw1"] * pars_nat$shape1[pars_nat$params=="prep.msw3"]
  params0[,"prep.msw4"] <- params0[,"prep.msw1"] * pars_nat$shape1[pars_nat$params=="prep.msw4"]
  params0[,"prep.msw5"] <- params0[,"prep.msw1"] * pars_nat$shape1[pars_nat$params=="prep.msw5"]

  
  for(s in 1:nrow(pars_state)){
    params.msm1[,s] <- runif(B0, pars_state$msm_prev_min[s], pars_state$msm_prev_max[s])
    params.wsm1[,s] <- runif(B0, pars_state$wsm_prev_min[s], pars_state$wsm_prev_max[s])
    params.msw1[,s] <- runif(B0, pars_state$msw_prev_min[s], pars_state$msw_prev_max[s])

    params.msm2[,s] <- runif(B0, pars_state$msm_vsupp_min[s], pars_state$msm_vsupp_max[s])
    params.wsm2[,s] <- runif(B0, pars_state$wsm_vsupp_min[s], pars_state$wsm_vsupp_max[s])
    params.msw2[,s] <- runif(B0, pars_state$msw_vsupp_min[s], pars_state$msw_vsupp_max[s])

  }

  return(list(params_nat = params0, 
              params_st_msm_pr = params.msm1,
              params_st_wsm_pr = params.wsm1,
              params_st_msw_pr = params.msw1,
              params_st_msm_vs = params.msm2,
              params_st_wsm_vs = params.wsm2,
              params_st_msw_vs = params.msw2))
}

