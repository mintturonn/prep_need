
msm <- wsm <- msw <- NULL

for (i in 1:nrow(pnat)){
  
  pnat0 <- pnat[i,]
  pr.msm0 <- pr.msm[i,]
  pr.wsm0 <- pr.wsm[i,]
  pr.msw0 <- pr.msw[i,]
  vs.msm0 <- vs.msm[i,]
  vs.wsm0 <- vs.wsm[i,]
  vs.msw0 <- vs.msw[i,]
  
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

#### OUTPUTS --> number of incident cases

msm %>%
  left_join(asr_msm, by=c("state", "race", "age")) %>%
  mutate(across(
    starts_with("inc"), 
    .fns = ~ .x * pop * get(sub("inc", "prep.ind", cur_column())), # Multiply corresponding prep.ind column
    .names = "pop{col}" )) %>% 
  mutate(across(
    starts_with("prep.ind"), 
    .fns = ~ .x * pop, 
    .names = "tot.{col}")) -> msm_out


wsm %>%
  left_join(asr_wsm, by=c("state", "race", "age")) %>%
  mutate(trnsm = "wsm") %>%
  mutate(across(
    starts_with("inc"), 
    .fns = ~ .x * pop * get(sub("inc", "prep.ind", cur_column())), # Multiply corresponding prep.ind column
    .names = "pop{col}" )) %>% 
  mutate(across(
    starts_with("prep.ind"), 
    .fns = ~ .x * pop, 
    .names = "tot.{col}")) -> wsm_out


msw %>%
  left_join(asr_msw, by=c("state", "race", "age")) %>%
  mutate(trnsm = "msw") %>%
  mutate(across(
    starts_with("inc"), 
    .fns = ~ .x * pop * get(sub("inc", "prep.ind", cur_column())), # Multiply corresponding prep.ind column
    .names = "pop{col}" )) %>% 
  mutate(across(
    starts_with("prep.ind"), 
    .fns = ~ .x * pop, 
    .names = "tot.{col}")) -> msw_out

msm_out$trnsm <- "msm"
wsm_out$trnsm <- "wsm"
msw_out$trnsm <- "msw"

#### FIGURES // State

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"), -starts_with("tot.prep.ind")) %>%
  group_by(state) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    state = state,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%
  left_join(df_incid_state, by=c("state")) %>%
  mutate(calibration=ifelse(!is.na(cases), "Calibrated States", "States with no incidence estimates")) %>%
  ggplot() + 
  geom_point(aes(y= state, x=cases), color="steelblue") +
  geom_pointrange(aes(y=state, xmin=q025, x=q50, xmax=q975 ), size=0.4, color="orange", alpha=0.5) +
  theme_bw() + xlab("Number of new HIV infections") + xlim(c(0,5000)) +
  facet_wrap(~calibration, ncol=2) + xlim(c(0, NA))


### NATIONAL
# State level - summarized for states without state-level data
# rbind(msm_out, wsm_out, msw_out) %>%
#   group_by(state) %>%
#   summarise(pop_prep = sum(pop_prep),
#             inc = sum(inc)) %>%
#   ungroup() %>%
#   left_join(df_incid_state, by=c("state")) %>%
#   mutate(data_inc = cases) %>%
#   filter(is.na(data_inc)) %>%
#   summarise(inc_out = sum(inc)) %>%
#   mutate(data_inc =df_incid_nat$cases_subs) -> state_no_data_out

# National
rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"), -starts_with("tot.prep.ind")) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  rowwise() %>%
  summarise(
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%
  mutate(cases =df_incid_nat$cases) %>%
  ggplot() + 
  geom_point(aes(y="National", x=cases), color="steelblue") +
  geom_pointrange(aes(y="National", xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw() + xlab("Number of new HIV infections")  + xlim(c(0, NA))

#### FIGURES // Race/ethnicity

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"), -starts_with("tot.prep.ind")) %>%
  group_by(race) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    race = race,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%
  left_join(df_incid_raceth, by=c("race")) %>%
  ggplot() + 
  geom_point(aes(y=race, x=cases), color="steelblue") +
  geom_pointrange(aes(y=race, xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw() + xlab("Number of new HIV infections") + ylab("Race/Ethnicity")


#### FIGURES // Transmission risk group

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"), -starts_with("tot.prep.ind")) %>%
  group_by(trnsm) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    trnsm = trnsm,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%
  left_join(df_incid_trnsm, by=c("trnsm")) %>%
  ggplot() + 
  geom_point(aes(y=trnsm, x=cases), color="steelblue") +
  geom_pointrange(aes(y=trnsm, xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw()  + xlim(c(0, NA)) + xlab("Number of new HIV infections") + ylab("Transmission risk group")

#### FIGURES // Age

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"), -starts_with("tot.prep.ind")) %>%
  group_by(age) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    age = age,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%
  left_join(df_incid_age, by=c("age")) %>%
  ggplot() + 
  geom_point(aes(y=age, x=cases), color="steelblue") +
  geom_pointrange(aes(y=age, xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw() + xlim(c(0, NA))


#### FIGURES // Age X MSM

rbind(msm_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"), -starts_with("tot.prep.ind")) %>%
  group_by(age) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    age = age,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%
  left_join(df_incid_msm_age, by=c("age")) %>%
  ggplot() + 
  geom_point(aes(y=age, x=cases), color="steelblue") +
  geom_pointrange(aes(y=age, xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw() + xlim(c(0, NA)) + xlab("Number of new HIV infections") + ylab("Age group")

  

