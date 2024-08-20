
       
likelihood <- function (X_k, pnat_lgt, pmsmpr_lgt, pwsmpr_lgt, pmswpr_lgt, pmsmvs_lgt, pwsmvs_lgt, pmswvs_lgt) {
 
  ##################################################################
  # reshape parameters for the model
for (i in 1:nrow(X_k)) {
  
  # cbind(sp0$params_nat, sp0$params_st_msm_pr, sp0$params_st_wsm_pr, sp0$params_st_msw_pr, sp0$params_st_msm_vs, sp0$params_st_wsm_vs, sp0$params_st_msw_vs)	
  
  pnat <- X_k[i,1:pnat_lgt]
  
  strcol <- pnat_lgt
  pr.msm <- X_k[i,(strcol+1):(strcol+pmsmpr_lgt)]
  
  strcol <- strcol+pmsmpr_lgt
  pr.wsm <- X_k[i,(strcol+1):(strcol+pwsmpr_lgt)]
  
  strcol <- strcol+pwsmpr_lgt
  pr.msw <- X_k[i,(strcol+1):(strcol+pmswpr_lgt)]
  
  strcol <- strcol+pmswpr_lgt
  vs.msm <- X_k[i,(strcol+1):(strcol+pmsmvs_lgt)]
  
  strcol <- strcol+pmsmvs_lgt
  vs.wsm <- X_k[i,(strcol+1):(strcol+pwsmvs_lgt)]
  
  strcol <- strcol+pwsmvs_lgt
  vs.msw <- X_k[i,(strcol+1):(strcol+pmswvs_lgt)]
    
  
  out <- pwsex_imis(pnat, pr.msm, pr.wsm, pr.msw, vs.msm, vs.wsm, vs.msw)
  
  msm <- as.data.frame(as.table(out$msm.inc))
  wsm <- as.data.frame(as.table(out$wsm.inc))
  msw <- as.data.frame(as.table(out$msw.inc))
   
   colnames(msm) <- colnames(wsm) <- colnames(msw) <- c("state", "race", "age", "inc")
  
  msm %>%
    left_join(asr_msm, by=c("state", "race", "age")) %>%
    mutate(inc = 0.3*pop*inc) %>%
    filter(!is.na(inc)) -> msm_out
  
  wsm %>%
    left_join(asr_wsm, by=c("state", "race", "age")) %>%
    mutate(inc = 0.3*pop*inc) %>%
    filter(!is.na(inc)) -> wsm_out
  
  msw %>%
    left_join(asr_msw, by=c("state", "race", "age")) %>%
    mutate(inc = 0.3*pop*inc)  %>%
    filter(!is.na(inc)) -> msw_out
  
# State level incidence
  rbind(msm_out, wsm_out, msw_out) %>% 
    group_by(state) %>%
     summarise(pop = sum(pop),
               inc = sum(inc)) %>%
    ungroup() %>%
    mutate(rate = inc/pop) -> state_out
    
# by transmission risk group (all groups, national)
  msm_out$trnsm <- "msm"
  wsm_out$trnsm <- "wsm"
  msw_out$trnsm <- "msw"
  
  rbind(msm_out, wsm_out, msw_out) %>% 
    group_by(trnsm) %>%
    summarise(pop = sum(pop),
              inc = sum(inc)) %>%
    ungroup() %>%
    mutate(rate = inc/pop) -> trnsm_out
  
# by age (all groups, national)
  rbind(msm_out, wsm_out, msw_out) %>% 
    group_by(race) %>%
    summarise(pop = sum(pop),
              inc = sum(inc)) %>%
    ungroup() %>%
    mutate(rate = inc/pop) -> race_out
  
# by race/ethnicity
  
  
  

}  
  

  
# 
#   
#   # years - all - WOMEN
#   crW15to18 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='15-18']
#   crW19to24 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='19-24']
#   crW15to24 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='15-24']
#   crW25to39 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='25-39']
#   crW40to54 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='40-54']
#   
#   sdcrW15to18 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='15-18']*sdbase*sdmultipsht
#   sdcrW19to24 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='19-24']*sdbase*sdmultipsht
#   sdcrW15to24 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='15-24']*sdbase*sdmultiplng
#   sdcrW25to39 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='25-39']*sdbase*sdmultiplng
#   sdcrW40to54 <- CRRW$`Rate per 100,000`[CRRW$`Age group`=='40-54']*sdbase*sdmultiplng
#   
#   # years - all - MEN
#   crM15to18 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='15-18']
#   crM19to24 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='19-24']
#   crM15to24 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='15-24']
#   crM25to39 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='25-39']
#   crM40to54 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='40-54']
#   
#   sdcrM15to18 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='15-18']*sdbase*sdmultipsht
#   sdcrM19to24 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='19-24']*sdbase*sdmultipsht
#   sdcrM15to24 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='15-24']*sdbase*sdmultiplng
#   sdcrM25to39 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='25-39']*sdbase*sdmultiplng
#   sdcrM40to54 <- CRRM$`HS Rate per 100,000`[CRRM$`Age group`=='40-54']*sdbase*sdmultiplng
# 
#   ###################################################################
#   # Model outpus
#   
# out <- statefun_imis(data.matrix(Params))
#   
#   if (((any(any(is.nan(out$diag)==1)==TRUE) || any(any(is.nan(out$prev)==1)==TRUE))==TRUE ||
#       (any(any(is.numeric(out$diag)==FALSE)==FALSE) || any(any(is.numberic(out$prev)==FALSE)==FALSE))==FALSE )==TRUE) {
#     
#     LLhood =-10^8;
#   } else {
#   #length of outputs (number of years)
#   yr <- nrow(out$prev)
#   
#   mprW15to18 <- out$prev[seq((yr-14), (yr-1), 4), 2]
#   mprW19to24 <- out$prev[seq((yr-14), (yr-1), 4), 3]
#   mprW25to39 <- out$prev[seq((yr-14), (yr-1), 4), 4]
#   mprW15to24 <- out$prev[seq((yr-15), (yr-1), 2), 15]
#   # mprW40to54 <- out$prev[seq((yr-14), (yr-1), 4), 5]
#     
#   mprM15to18 <- out$prev[seq((yr-15), (yr-1), 4), 6]
#   mprM19to24 <- out$prev[seq((yr-14), (yr-1), 4), 7]
#   mprM25to39 <- out$prev[seq((yr-14), (yr-1), 4), 8]
#   mprM15to24 <- out$prev[seq((yr-15), (yr-1), 2), 16]
#   # mprM40to54 <- out$prev[seq((yr-14), (yr-1), 4), 9]
#   
#   mcrW15to18 <- out$diag[seq((yr-10), (yr), 1), 1]
#   mcrW19to24 <- out$diag[seq((yr-10), (yr), 1), 2]
#   mcrW25to39 <- out$diag[seq((yr-15), (yr), 1), 3]
#   mcrW40to54 <- out$diag[seq((yr-15), (yr), 1), 4]
#   mcrW15to24 <- out$diag[seq((yr-15), (yr), 1), 9]
#   
#   mcrM15to18 <- out$diag[seq((yr-10), (yr), 1), 5]
#   mcrM19to24 <- out$diag[seq((yr-10), (yr), 1), 6]
#   mcrM25to39 <- out$diag[seq((yr-15), (yr), 1), 7]
#   mcrM40to54 <- out$diag[seq((yr-15), (yr), 1), 8]
#   mcrM15to24 <- out$diag[seq((yr-15), (yr), 1), 10]
#     
#   mEvSWpr <- out$eversex[seq((yr-16), (yr), 2), 1]
#   mEvSMpr <- out$eversex[seq((yr-16), (yr), 2), 2]  
#   
#   ###################################################################
#   # Prevalence likelihoods are beta likelihoods
#   BetaParamsW15to18 <- estBetaParams(mprW15to18, sdW15to18^2)
#   BetaParamsW19to24 <- estBetaParams(mprW19to24, sdW19to24^2)
#   BetaParamsW25to39 <- estBetaParams(mprW25to39, sdW25to39^2)
#   BetaParamsW15to24 <- estBetaParams(mprW15to24, sdW15to24^2)
#   
#   BetaParamsM15to18 <- estBetaParams(mprM15to18, sdM15to18^2)
#   BetaParamsM19to24 <- estBetaParams(mprM19to24, sdM19to24^2)
#   BetaParamsM25to39 <- estBetaParams(mprM25to39, sdM25to39^2)
#   BetaParamsM15to24 <- estBetaParams(mprM15to24, sdM15to24^2)
#   
#   # Ever sex likelihoods are beta likelihoods
#   BetaParamsWEvS <- estBetaParams(mEvSWpr, EvSWsd^2)
#   BetaParamsMEvS <- estBetaParams(mEvSMpr, EvSMsd^2)
#   
#   # Women
# 
#   llprW15to18 <- tryCatch( {  sum(log(dbeta(prW15to18, BetaParamsW15to18$alpha, BetaParamsW15to18$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
# 
#   llprW19to24 <- tryCatch( {  sum(log(dbeta(prW19to24, BetaParamsW19to24$alpha, BetaParamsW19to24$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
# 
#   llprW25to39 <- tryCatch( {sum(log(dbeta(prW25to39, BetaParamsW25to39$alpha, BetaParamsW25to39$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
# 
#   llprW15to24 <- tryCatch( {sum(log(dbeta(prW15to24, BetaParamsW15to24$alpha, BetaParamsW15to24$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
#   
#   # Men
#  
#   llprM15to18 <- tryCatch( { sum(log(dbeta(prM15to18, BetaParamsM15to18$alpha, BetaParamsM15to18$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
#  
#   llprM19to24 <- tryCatch( { sum(log(dbeta(mprM19to24, BetaParamsM19to24$alpha, BetaParamsM19to24$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
#  
#   llprM25to39 <- tryCatch( { sum(log(dbeta(prM25to39, BetaParamsM25to39$alpha, BetaParamsM25to39$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
#  
#   llprM15to24 <- tryCatch( {sum(log(dbeta(prM15to24, BetaParamsM15to24$alpha, BetaParamsM15to24$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
#   
#   # Ever sex likelihoods
#   
#     llEvSWpr <- tryCatch( { sum(log(dbeta(EvSWpr, BetaParamsWEvS$alpha, BetaParamsWEvS$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
# 
#     llEvSMpr <- tryCatch( { sum(log(dbeta(EvSMpr, BetaParamsMEvS$alpha, BetaParamsMEvS$beta)))  }  ,warning= function(w) { -10^10 }  ,error=function(e) { -10^10 }) 
# 
#   
#   ###########################################################
#   # Case Report Rate
#   
#   llcrW15to18 <- sum(log(dnorm(crW15to18, mcrW15to18, sdcrW15to18)))
#   llcrW19to24 <- sum(log(dnorm(crW19to24, mcrW19to24, sdcrW19to24)))
#   llcrW15to24 <- sum(log(dnorm(crW15to24, mcrW15to24, sdcrW15to24)))
#   llcrW25to39 <- sum(log(dnorm(crW25to39, mcrW25to39, sdcrW25to39)))
#   llcrW40to54 <- sum(log(dnorm(crW40to54, mcrW40to54, sdcrW40to54)))
#   
#   llcrM15to18 <- sum(log(dnorm(crM15to18, mcrM15to18, sdcrM15to18)))
#   llcrM19to24 <- sum(log(dnorm(crM19to24, mcrM19to24, sdcrM19to24)))
#   llcrM15to24 <- sum(log(dnorm(crM15to24, mcrM15to24, sdcrM15to24)))
#   llcrM25to39 <- sum(log(dnorm(crM25to39, mcrM25to39, sdcrM25to39)))
#   llcrM40to54 <- sum(log(dnorm(crM40to54, mcrM40to54, sdcrM40to54)))
#   
#   ###########################################################
#   # Divided by the number of years there is data (or number of pooled cycles) / this divided by 2
#   
#   sumLLprW <- (llprW15to18+llprW19to24+llprW25to39+llprW15to24)
#   sumLLprM <- (llprM15to18+llprM19to24+llprM25to39+llprM15to24)
#   
#   sumLLcrW <- (llcrW15to18+llcrW19to24)+(llcrW15to24+llcrW25to39+llcrW40to54)/5
#   sumLLcrM <- (llcrM15to18+llcrM19to24)+(llcrM15to24+llcrM25to39+llcrM40to54)/5
#   
#   sumLLEvSex <- (llEvSWpr+llEvSMpr) /10
#   ###########################################################  
#   LLhood <- tryCatch( { (sumLLprW+sumLLprM+sumLLEvSex+sumLLcrW+sumLLcrM) }  ,warning= function(w) { NA }  ,error=function(e) { NA }) #this is part of the trycatch function trycatch ()
# #   
#   if (is.na(LLhood))  { LLhood <- -10^8   }
#   if (((LLhood))%in%c(-Inf,Inf))  { LLhood <- -10^8  }
#   }
#   
#  print(-LLhood) 
 ## NEEDS TO BE IN NORMAL SCALE -- WILL BE LOGGED IN IMIS
  #return(exp(LLhood))
}
