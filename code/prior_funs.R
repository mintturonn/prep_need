# 
# prior <- function(theta)	dmvnorm(theta, c(0,0), diag(3,2))
# 
# 
# sample.prior <- function(n)	rmvnorm(n, c(0,0), diag(3,2))

# Need to define prior 


# define sample.prior to sample from the prior
######################################################
sample.prior <- function (B0, pars_nat, id_beta, id_lognr, pars_state) {

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
  
  for(s in 1:nrow(pars_state)){
    params.msm1[,s] <- runif(B0, pars_state$msm_prev_min, pars_state$msm_prev_max)
    params.wsm1[,s] <- runif(B0, pars_state$wsm_prev_min, pars_state$wsm_prev_max)
    params.msw1[,s] <- runif(B0, pars_state$msw_prev_min, pars_state$msw_prev_max)

    params.msm2[,s] <- runif(B0, pars_state$msm_vsupp_min, pars_state$msm_vsupp_max)
    params.wsm2[,s] <- runif(B0, pars_state$wsm_vsupp_min, pars_state$wsm_vsupp_max)
    params.msw2[,s] <- runif(B0, pars_state$msw_vsupp_min, pars_state$msw_vsupp_max)

  }

  return(list(params_nat = params0, 
              params_st_msm_pr = params.msm1,
              params_st_wsm_pr = params.wsm1,
              params_st_msw_pr = params.msw1,
              params_st_msm_vs = params.msm2,
              params_st_wsm_vs = params.wsm2,
              params_st_msw_vs = params.msw2))
}

# define sample.prior to sample from the prior
######################################################
prior <- function(X_k, 
                  pars_nat, id_beta, id_lognr, pars_state,  
                  pmsmpr_lgt, pwsmpr_lgt, pmswpr_lgt, pmsmvs_lgt, pwsmvs_lgt, pmswvs_lgt) {
  
  # 
  priord <- matrix(data=NA, nrow(X_k), ncol(X_k))

  for (i in id_beta) {
    priord[,i] <- dbeta(X_k[,i], as.numeric(pars_nat$shape1[i]), as.numeric(pars_nat$shape2[i]))
  }
  
  for (i in id_lognrm) {
    priord[,i] <- dlnorm(X_k[,i], as.numeric(pars_nat$shape1[i]), as.numeric(pars_nat$shape2[i]))
  }
  
strcol <-  ncol(pars_nat)+1
  for (i in strcol:(strcol+pmsmpr_lgt)) {
    priord[,i] <- dunif(X_k[,i], as.numeric(pars_state$msm_prev_min[i]), as.numeric(pars_state$msm_prev_max[i]))
  }
  
strcol <-  strcol+pmsmpr_lgt+1
  for (i in strcol:(strcol+pwsmpr_lgt)) {
    priord[,i] <- dunif(X_k[,i], as.numeric(pars_state$wsm_prev_min[i]), as.numeric(pars_state$wsm_prev_max[i]))
  }

strcol <-  strcol+pwsmpr_lgt+1
  for (i in strcol:(strcol+pmswpr_lgt)) {
    priord[,i] <- dunif(X_k[,i], as.numeric(pars_state$msw_prev_min[i]), as.numeric(pars_state$msw_prev_max[i]))
  }

strcol <-  strcol+pmswpr_lgt+1
  for (i in strcol:(strcol+pmsmvs_lgt)) {
    priord[,i] <- dunif(X_k[,i], as.numeric(pars_state$msm_vsupp_min[i]), as.numeric(pars_state$msm_vsupp_max[i]))
  }

strcol <-  strcol+pmsmvs_lgt+1
for (i in strcol:(strcol+pwsmvs_lgt)) {
  priord[,i] <- dunif(X_k[,i], as.numeric(pars_state$wsm_vsupp_min[i]), as.numeric(pars_state$wsm_vsupp_max[i]))
}

strcol <-  strcol+pwsmvs_lgt+1
for (i in strcol:(strcol+pmswvs_lgt)) {
  priord[,i] <- dunif(X_k[,i], as.numeric(pars_state$msw_vsupp_min[i]), as.numeric(pars_state$msw_vsupp_max[i]))
}
  

priorprod <- (apply(priord, 1, prod))
  return(priorprod)
}
