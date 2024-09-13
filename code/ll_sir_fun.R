
       
likelihood <- function (sp0) {
 
llhood <- matrix(NA, nrow(sp0$params_nat), 7) 

for (i in 1:nrow(sp0$params_nat)) {

  pnat <- sp0$params_nat[i,]
  pr.msm <- sp0$params_st_msm_pr[i,]
  pr.wsm <- sp0$params_st_wsm_pr[i,]
  pr.msw <- sp0$params_st_msw_pr[i,]
  vs.msm <- sp0$params_st_msm_vs[i,]
  vs.wsm <- sp0$params_st_wsm_vs[i,]
  vs.msw <- sp0$params_st_msw_vs[i,]
  
  pr.prep <- pnat[grep("prep.", names(pnat))]
  pr.prep.msm <-  pr.prep[grep("msm", names(pr.prep))]
  pr.prep.wsm <-  pr.prep[grep("wsm", names(pr.prep))]
  pr.prep.msw <-  pr.prep[grep("msw", names(pr.prep))]
    

   out <- pwsex_sir(pnat, pr.msm, pr.wsm, pr.msw, vs.msm, vs.wsm, vs.msw)

   msm <- as.data.frame(as.table(out$msm.inc))
   wsm <- as.data.frame(as.table(out$wsm.inc))
   msw <- as.data.frame(as.table(out$msw.inc))

   colnames(msm) <- colnames(wsm) <- colnames(msw) <- c("state", "race", "age", "inc")

   msm %>%
     left_join(asr_msm, by=c("state", "race", "age")) %>%
     mutate(age_prep = case_when(
                               age == "13-24" ~ pr.prep.msm["prep.msm1"],
                               age == "25-34" ~ pr.prep.msm["prep.msm2"],
                               age == "35-44" ~ pr.prep.msm["prep.msm3"],
                               age == "45-54" ~ pr.prep.msm["prep.msm4"],
                               age == "55+"   ~ pr.prep.msm["prep.msm5"],
                               TRUE ~ NA_real_ ),
            inc = age_prep*pop*inc,
            pop_prep = age_prep*pop) %>%
     filter(!is.na(inc)) -> msm_out

   wsm %>%
     left_join(asr_wsm, by=c("state", "race", "age")) %>%
     mutate(age_prep = case_when(
       age == "13-24" ~ pr.prep.wsm["prep.wsm1"],
       age == "25-34" ~ pr.prep.wsm["prep.wsm2"],
       age == "35-44" ~ pr.prep.wsm["prep.wsm3"],
       age == "45-54" ~ pr.prep.wsm["prep.wsm4"],
       age == "55+"   ~ pr.prep.wsm["prep.wsm5"],
       TRUE ~ NA_real_  ),
     inc = age_prep*pop*inc,
     pop_prep = age_prep*pop) %>%
     filter(!is.na(inc)) -> wsm_out

   msw %>%
     left_join(asr_msw, by=c("state", "race", "age")) %>%
     mutate(age_prep = case_when(
       age == "13-24" ~ pr.prep.msw["prep.msw1"],
       age == "25-34" ~ pr.prep.msw["prep.msw2"],
       age == "35-44" ~ pr.prep.msw["prep.msw3"],
       age == "45-54" ~ pr.prep.msw["prep.msw4"],
       age == "55+"   ~ pr.prep.msw["prep.msw5"],
       TRUE ~ NA_real_ ),
       inc = age_prep*pop*inc,
       pop_prep = age_prep*pop) %>%
     filter(!is.na(inc)) -> msw_out

# State level incidence
  rbind(msm_out, wsm_out, msw_out) %>%
    group_by(state) %>%
     summarise(pop_prep = sum(pop_prep),
               inc = sum(inc)) %>%
    ungroup() %>%
    mutate(inc_out = inc)  %>%
    left_join(df_incid_state, by=c("state")) %>%
    mutate(data_inc = cases) %>%
    filter(!is.na(data_inc))  -> state_out
  
# State level - summarized for states without state-level data
  rbind(msm_out, wsm_out, msw_out) %>%
    group_by(state) %>%
    summarise(pop_prep = sum(pop_prep),
              inc = sum(inc)) %>%
    ungroup() %>%
    left_join(df_incid_state, by=c("state")) %>%
    mutate(data_inc = cases) %>%
    filter(is.na(data_inc)) %>%
    summarise(inc_out = sum(inc)) %>%
    mutate(data_inc =df_incid_nat$cases_subs) -> state_no_data_out
  
# National
  rbind(msm_out, wsm_out, msw_out) %>%
    summarise(inc_out = sum(inc)) %>%
   mutate(data_inc =df_incid_nat$cases) -> nat_out
  

# by transmission risk group (all groups, national)
  msm_out$trnsm <- "msm"
  wsm_out$trnsm <- "wsm"
  msw_out$trnsm <- "msw"

  rbind(msm_out, wsm_out, msw_out) %>%
    group_by(trnsm) %>%
    summarise(pop_prep = sum(pop_prep),
              inc = sum(inc)) %>%
    ungroup() %>%
    mutate(inc_out = inc/sum(inc)) %>%
    left_join(df_incid_trnsm, by=c("trnsm")) %>%
    mutate(data_inc = cases) %>%
    filter(!is.na(data_inc)) -> trnsm_out

# by age (all groups, national)
  rbind(msm_out, wsm_out, msw_out) %>%
    group_by(age) %>%
    summarise(pop_prep = sum(pop_prep),
              inc = sum(inc)) %>%
    ungroup() %>%
    mutate(inc_out = inc/sum(inc)) %>%
    left_join(df_incid_age, by=c("age")) %>%
    mutate(data_inc = cases) -> age_out
  
  # by age  for msm (national)
  msm_out %>%
    group_by(age) %>%
    summarise(pop_prep = sum(pop_prep),
              inc = sum(inc)) %>%
    ungroup() %>%
    mutate(inc_out = inc/sum(inc)) %>%
    left_join(df_incid_msm_age, by=c("age")) %>%
    mutate(data_inc = cases) -> age_msm_out

# by race/ethnicity
  rbind(msm_out, wsm_out, msw_out) %>%
    group_by(race) %>%
    summarise(pop_prep = sum(pop_prep),
              inc = sum(inc)) %>%
    ungroup() %>%
    mutate(inc_out = inc/sum(inc)) %>%
    left_join(df_incid_raceth, by=c("race")) %>%
    mutate(data_inc = cases) %>%
    filter(!is.na(data_inc)) -> race_out

# age_ll <- race_ll <- age_msm_ll <-  trnsm_ll <- state_ll <- state_nd_ll <- nat_ll <- NULL

# AGE LL
   # result1 <- tryCatch({
     result1 <- dmultinom(age_out$data_inc/10,  prob=age_out$inc_out, log=TRUE)
     # if (is.nan(value) || is.infinite(value)) {-10^10 } else { value }
     # }, warning = function(w) { -10^10 }, error = function(e) { -10^10  })
     # 
   age_ll <-  result1 

# RACE/ETH LL
  # result2 <- tryCatch({
   result2 <- dmultinom(race_out$data_inc/15, prob=race_out$inc_out, log=TRUE)
  #   if (is.nan(value) || is.infinite(value)) {-10^10 } else { value }
  # }, warning = function(w) { -10^10 }, error = function(e) { -10^10  })
  # 
  race_ll <-   result2 

# STATE LL 
# result3 <- tryCatch({
  result3 <- dnorm(state_out$data_inc, mean=state_out$inc_out, sd = state_out$data_inc*0.4, log=TRUE)
  #   if (is.nan(value) || is.infinite(value)) {-10^10 } else { value }
  # }, warning = function(w) { -10^10 }, error = function(e) { -10^10  })
 
 state_ll <-   result3 

 # Sum STATE without daa 
 # result4 <- tryCatch({
   result4 <- dnorm(state_no_data_out$data_inc, state_no_data_out$inc_out,  sd = state_no_data_out$data_inc*0.1,  log=TRUE)
 #   if (is.nan(value) || is.infinite(value)) {-10^10 } else { value }
 # }, warning = function(w) { -10^10 }, error = function(e) { -10^10  })
 # 
 state_nd_ll <- result4 
 
 # National
 # result5 <- tryCatch({
 result5 <- dnorm(nat_out$data_inc, nat_out$inc_out,  sd = nat_out$data_inc*0.1, log=TRUE)
 #   if (is.nan(value) || is.infinite(value)) {-10^10 } else { value }
 # }, warning = function(w) { -10^10 }, error = function(e) { -10^10  })
 # 
 nat_ll <-  result5


# TRNSM LL 
  # result4 <- tryCatch({
  result6 <- dmultinom(trnsm_out$data_inc/15,  prob=trnsm_out$inc_out, log=TRUE)
  #   if (is.nan(value) || is.infinite(value)) {-10^10 } else { value }
  # }, warning = function(w) { -10^10 }, error = function(e) { -10^10  })
  # 
  trnsm_ll <-  result6 

# MSM AGE LL
  # result5 <- tryCatch({
  result7 <-  dmultinom(age_msm_out$data_inc/10, prob=age_msm_out$inc_out, log=TRUE)
  #   if (is.nan(value) || is.infinite(value)) {-10^10 } else { value }
  # }, warning = function(w) { -10^10 }, error = function(e) { -10^10  })

  age_msm_ll <-   result7 

  
  llhood[i,1] <- age_ll
  llhood[i,2] <- race_ll  
  llhood[i,3] <- trnsm_ll 
  llhood[i,4] <- age_msm_ll 
  llhood[i,5] <- sum(state_ll) 
  llhood[i,6] <- state_nd_ll
  llhood[i,7] <- nat_ll

}  
  return(exp(rowSums(llhood)))
}

