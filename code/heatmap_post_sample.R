
heat_post <- function(priors, numbr){
  
  msm <- wsm <- msw <- NULL
  
  for (i in 1:nrow(priors$params_nat)){
    
    pnat0 <- priors$params_nat[i,]
    pr.msm0 <- priors$params_st_msm_pr[i,]
    pr.wsm0 <- priors$params_st_wsm_pr[i,]
    pr.msw0 <- priors$params_st_msw_pr[i,]
    vs.msm0 <- priors$params_st_msm_vs[i,]
    vs.wsm0 <- priors$params_st_wsm_vs[i,]
    vs.msw0 <- priors$params_st_msw_vs[i,]
    
    out <- pwsex_sir(pnat0, pr.msm0, pr.wsm0, pr.msw0, vs.msm0, vs.wsm0, vs.msw0)
    
    # prop with indicators
    pr.prep <- pnat0[grep("prep.", names(pnat0))]
    pr.prep.msm <-  pr.prep[grep("msm", names(pr.prep))]
    pr.prep.wsm <-  pr.prep[grep("wsm", names(pr.prep))]
    pr.prep.msw <-  pr.prep[grep("msw", names(pr.prep))]
    
    # incidence per capita
    as.data.frame(as.table(out$msm.inc)) %>%
      rename(state = Var1, race = Var2, age = Var3, inc = Freq) %>%
      mutate(prep.ind = case_when(
        age == "13-24" ~ pr.prep.msm["prep.msm1"],
        age == "25-34" ~ pr.prep.msm["prep.msm2"],
        age == "35-44" ~ pr.prep.msm["prep.msm3"],
        age == "45-54" ~ pr.prep.msm["prep.msm4"],
        age == "55+"   ~ pr.prep.msm["prep.msm5"],
        TRUE ~ NA_real_ )) -> msm0
    
    as.data.frame(as.table(out$wsm.inc)) %>%
      rename(state = Var1, race = Var2, age = Var3, inc = Freq) %>%
      mutate(prep.ind = case_when(
        age == "13-24" ~ pr.prep.wsm["prep.wsm1"],
        age == "25-34" ~ pr.prep.wsm["prep.wsm2"],
        age == "35-44" ~ pr.prep.wsm["prep.wsm3"],
        age == "45-54" ~ pr.prep.wsm["prep.wsm4"],
        age == "55+"   ~ pr.prep.wsm["prep.wsm5"],
        TRUE ~ NA_real_ )) -> wsm0
    
    as.data.frame(as.table(out$msw.inc)) %>%
      rename(state = Var1, race = Var2, age = Var3, inc = Freq) %>%
      mutate(prep.ind = case_when(
        age == "13-24" ~ pr.prep.msw["prep.msw1"],
        age == "25-34" ~ pr.prep.msw["prep.msw2"],
        age == "35-44" ~ pr.prep.msw["prep.msw3"],
        age == "45-54" ~ pr.prep.msw["prep.msw4"],
        age == "55+"   ~ pr.prep.msw["prep.msw5"],
        TRUE ~ NA_real_ )) -> msw0
    
    
    if(i==1){
      msm <- cbind(msm0)
      wsm <- cbind(wsm0)
      msw <- cbind(msw0)
    } else{
      col_name <- paste0("inc", i)
      col_name2 <- paste0("prep.ind", i)
      
      msm <- cbind(msm, setNames(data.frame(msm0$inc), col_name), setNames(data.frame(msm0$prep.ind), col_name2))
      wsm <- cbind(wsm, setNames(data.frame(wsm0$inc), col_name), setNames(data.frame(wsm0$prep.ind), col_name2))
      msw <- cbind(msw, setNames(data.frame(msw0$inc), col_name), setNames(data.frame(msw0$prep.ind), col_name2))
    }
  }
  
  #########################
  
  msm$trnsm <- "msm"
  wsm$trnsm <- "wsm"
  msw$trnsm <- "msw"
  
  inputs <-  as.data.frame(priors$params_nat)
  inputs$simulation <- paste0("inc", 1:nrow(priors$params_nat))
  inputs$simulation[1] <- "inc"
  
  # condomless acts     
  inputs$msm.c.nact1 <- (1-inputs$msm.cond1)*inputs$msm.nact + (1-inputs$msm.cond_eff)*inputs$msm.cond1*inputs$msm.nact
  inputs$msm.c.nact2 <- (1-inputs$msm.cond2)*inputs$msm.nact + (1-inputs$msm.cond_eff)*inputs$msm.cond2*inputs$msm.nact
  inputs$msm.c.nact3 <- (1-inputs$msm.cond3)*inputs$msm.nact + (1-inputs$msm.cond_eff)*inputs$msm.cond3*inputs$msm.nact
  inputs$msm.c.nact4 <- (1-inputs$msm.cond4)*inputs$msm.nact + (1-inputs$msm.cond_eff)*inputs$msm.cond4*inputs$msm.nact
  inputs$msm.c.nact5 <- (1-inputs$msm.cond5)*inputs$msm.nact + (1-inputs$msm.cond_eff)*inputs$msm.cond5*inputs$msm.nact
  
  inputs$wsm.c.nact1 <- (1-inputs$wsm.cond1)*inputs$wsm.nact + (1-inputs$wsm.cond_eff)*inputs$wsm.cond1*inputs$wsm.nact
  inputs$wsm.c.nact2 <- (1-inputs$wsm.cond2)*inputs$wsm.nact + (1-inputs$wsm.cond_eff)*inputs$wsm.cond2*inputs$wsm.nact
  inputs$wsm.c.nact3 <- (1-inputs$wsm.cond3)*inputs$wsm.nact + (1-inputs$wsm.cond_eff)*inputs$wsm.cond3*inputs$wsm.nact
  inputs$wsm.c.nact4 <- (1-inputs$wsm.cond4)*inputs$wsm.nact + (1-inputs$wsm.cond_eff)*inputs$wsm.cond4*inputs$wsm.nact
  inputs$wsm.c.nact5 <- (1-inputs$wsm.cond5)*inputs$wsm.nact + (1-inputs$wsm.cond_eff)*inputs$wsm.cond5*inputs$wsm.nact
  
  inputs$msw.c.nact1 <- (1-inputs$msw.cond1)*inputs$msw.nact + (1-inputs$msw.cond_eff)*inputs$msw.cond1*inputs$msw.nact
  inputs$msw.c.nact2 <- (1-inputs$msw.cond2)*inputs$msw.nact + (1-inputs$msw.cond_eff)*inputs$msw.cond2*inputs$msw.nact
  inputs$msw.c.nact3 <- (1-inputs$msw.cond3)*inputs$msw.nact + (1-inputs$msw.cond_eff)*inputs$msw.cond3*inputs$msw.nact
  inputs$msw.c.nact4 <- (1-inputs$msw.cond4)*inputs$msw.nact + (1-inputs$msw.cond_eff)*inputs$msw.cond4*inputs$msw.nact
  inputs$msw.c.nact5 <- (1-inputs$msw.cond5)*inputs$msw.nact + (1-inputs$msw.cond_eff)*inputs$msw.cond5*inputs$msw.nact
  
  age_group_map <- c("1" = "13-24", "2" = "25-34", "3" = "35-44", "4" = "45-54", "5" = "55+")
  
  inputs %>%
    select(simulation, contains("contact"), contains("c.nact")) %>%
    pivot_longer(
      cols = -simulation,
      names_to = c("trnsm", "variable", "age_group"),
      names_pattern = "([a-z]+).(contact|c.nact)(\\d+)",
      values_to = "value") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(age_group = as.numeric(age_group)) %>%
    arrange(simulation, trnsm, age_group) %>%
    mutate(age = age_group_map[as.character(age_group)]) %>%
    arrange(simulation, trnsm, age_group) -> beh.params
  
  
  inputs %>%
    select(simulation, contains("RR")) %>%
    pivot_longer(
      cols = -simulation,
      names_to = c("variable", "race", "trnsm"),
      names_pattern = "(RR)\\.([^.]+)\\.([^.]+)",
      values_to = "prevRR")  %>%
    mutate(race = str_to_title(race),
           race = str_replace(race, "Multi", "Multiracial"),
           race = str_replace(race, "Aian", "AIAN"),
           race = str_replace(race, "Nhopi", "NHOPI")) -> re.params0
  
  additional_white <- re.params0 %>%
    select(simulation, trnsm) %>%
    distinct() %>%
    mutate(
      variable = "RR",
      race = "White",
      prevRR = 1
    )
  
  re.params <- bind_rows(re.params0, additional_white)
  
  priors$params_st_msm_pr %>%
    as_tibble() %>%
    mutate(simulation = ifelse(row_number() == 1, "inc", paste0("inc", row_number()))) %>%
    pivot_longer(
      cols = -simulation,
      names_to = c("state", "pr", "trnsm"),
      names_pattern = "([^.]+)_([^.]+)\\.([^.]+)",
      values_to = "prev") %>% select(-pr)  -> msm.pr
  
  priors$params_st_wsm_pr %>%
    as_tibble() %>%
    mutate(simulation = ifelse(row_number() == 1, "inc", paste0("inc", row_number()))) %>%
    pivot_longer(
      cols = -simulation,
      names_to = c("state", "pr", "trnsm"),
      names_pattern = "([^.]+)_([^.]+)\\.([^.]+)",
      values_to = "prev")  %>% select(-pr)  -> wsm.pr
  
  priors$params_st_msw_pr %>%
    as_tibble() %>%
    mutate(simulation = ifelse(row_number() == 1, "inc", paste0("inc", row_number()))) %>%
    pivot_longer(
      cols = -simulation, 
      names_to = c("state", "pr", "trnsm"),
      names_pattern = "([^.]+)_([^.]+)\\.([^.]+)",
      values_to = "prev")  %>% select(-pr)  -> msw.pr
  
  priors$params_st_msm_vs %>%
    as_tibble() %>%
    mutate(simulation = ifelse(row_number() == 1, "inc", paste0("inc", row_number()))) %>%
    pivot_longer(
      cols = -simulation,
      names_to = c("state", "-", "trnsm"),
      names_pattern = "([^.]+)_([^.]+)\\.([^.]+)",
      values_to = "vs") %>% select(-`-`) -> msm.vs
  
  priors$params_st_wsm_vs %>%
    as_tibble() %>%
    mutate(simulation = ifelse(row_number() == 1, "inc", paste0("inc", row_number()))) %>%
    pivot_longer(
      cols = -simulation, 
      names_to = c("state", "-", "trnsm"),
      names_pattern = "([^.]+)_([^.]+)\\.([^.]+)",
      values_to = "vs") %>% select(-`-`) -> wsm.vs
  
  priors$params_st_msw_vs %>%
    as_tibble() %>%
    mutate(simulation = ifelse(row_number() == 1, "inc", paste0("inc", row_number()))) %>%
    pivot_longer(
      cols = -simulation, 
      names_to = c("state", "-", "trnsm"),
      names_pattern = "([^.]+)_([^.]+)\\.([^.]+)",
      values_to = "vs") %>% select(-`-`) -> msw.vs
  
  
  #######################
  
  # numbr = 50 # number of bins to 
  
  rbind(msm, wsm, msw) %>%
    select(-contains("prep.ind")) %>%
    pivot_longer(cols = starts_with("inc"), 
                 names_to = "simulation", 
                 values_to = "inc_value") %>%
    left_join(beh.params, by=c("age", "trnsm", "simulation"))  %>%
    left_join(re.params, by=c("trnsm", "race", "simulation"))  %>%
    left_join(rbind(msm.pr, msw.pr, wsm.pr), by=c("trnsm", "state", "simulation"))  %>%
    left_join(rbind(msm.vs, msw.vs, wsm.vs), by=c("trnsm", "state", "simulation")) %>%
    mutate(prev_adjusted = ifelse((1-vs)*prev*prevRR >1, 0.999, (1-vs)*prev*prevRR)) %>%
    mutate(inc_log10 = log10(inc_value)) -> df
  
  df$sample <- "posterior"

  return(df)
}
