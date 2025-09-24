
# population MSM
msmpr <- read_excel('~/prep_need/data/params.xlsx', sheet = 'grey_2014')

msmpr %>% 
  rename(state = State) %>%
  dplyr::select(state, prev_extract) -> msmpr

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

## --> TO DO (this should account for the prevalence in pop!)
## same for in SIR function ! 
##########################

for (i in 1:nrow(pnat)){
  
  pnat0 <- pnat[i,]
  pr.msm0 <- pr.msm[i,]
  pr.wsm0 <- pr.wsm[i,]
  pr.msw0 <- pr.msw[i,]
  pr.pwid0 <- pr.pwid[i,]
  vs.msm0 <- vs.msm[i,]
  vs.wsm0 <- vs.wsm[i,]
  vs.msw0 <- vs.msw[i,]
  vs.pwid0 <- vs.pwid[i,]

##########################
  
  out <- pwsex_sir(pnat0, pr.msm0, pr.wsm0, pr.msw0, vs.msm0, vs.wsm0, vs.msw0)
  
  # prop with indicators
  pr.prep <- pnat0[grep("prep.", names(pnat0))]
  pr.prep.msm <-  pr.prep[grep("msm", names(pr.prep))]
  pr.prep.wsm <-  pr.prep[grep("wsm", names(pr.prep))]
  pr.prep.msw <-  pr.prep[grep("msw", names(pr.prep))]
  
 #########################

  out2 <- pwid_model(pnat0, pr.pwid0, vs.pwid0)
 
 #########################  
  
  # incidence per capita
  as.data.frame(as.table(out$msm.inc)) %>%
    rename(state = Var1, race = Var2, age = Var3, inc = Freq) %>%
    mutate(prep.ind = case_when(
      age == "13-24" ~ (7/12)*pr.prep.msm["prep.msm1"]+(5/12)*pnat0["prep.young"]*pr.prep.msm["prep.msm1"],
      age == "25-34" ~ pr.prep.msm["prep.msm2"],
      age == "35-44" ~ pr.prep.msm["prep.msm3"],
      age == "45-54" ~ pr.prep.msm["prep.msm4"],
      age == "55+"   ~ pr.prep.msm["prep.msm5"],
      TRUE ~ NA_real_ )) -> msm0
  
  as.data.frame(as.table(out$wsm.inc)) %>%
    rename(state = Var1, race = Var2, age = Var3, inc = Freq) %>%
    mutate(prep.ind = case_when(
      age == "13-24" ~ (7/12)*pr.prep.wsm["prep.wsm1"]+(5/12)*pnat0["prep.young"]*pr.prep.wsm["prep.wsm1"],
      age == "25-34" ~ pr.prep.wsm["prep.wsm2"],
      age == "35-44" ~ pr.prep.wsm["prep.wsm3"],
      age == "45-54" ~ pr.prep.wsm["prep.wsm4"],
      age == "55+"   ~ pr.prep.wsm["prep.wsm5"],
      TRUE ~ NA_real_ )) -> wsm0
  
  as.data.frame(as.table(out$msw.inc)) %>%
    rename(state = Var1, race = Var2, age = Var3, inc = Freq) %>%
    mutate(prep.ind = case_when(
      age == "13-24" ~ (7/12)*pr.prep.msw["prep.msw1"]+(5/12)*pnat0["prep.young"]*pr.prep.msw["prep.msw1"],
      age == "25-34" ~ pr.prep.msw["prep.msw2"],
      age == "35-44" ~ pr.prep.msw["prep.msw3"],
      age == "45-54" ~ pr.prep.msw["prep.msw4"],
      age == "55+"   ~ pr.prep.msw["prep.msw5"],
      TRUE ~ NA_real_ )) -> msw0

  ################  
  
  as.data.frame(as.table(out2$pwid_m)) %>%
    rename(state = Var1,inc = Freq) %>%
    mutate(
      sex = "Male",
      age = "All",
      prep.ind = pnat0["prop.pwid.m"]*pnat0["pwid.r.m"],
      race = "All" ) %>%
    filter(!is.na(inc)) -> pwid_m0
  
  as.data.frame(as.table(out2$pwid_f)) %>%
    rename(state = Var1,inc = Freq) %>%
    mutate(
      sex ="Female",
      age = "All",
      prep.ind = pnat0["prop.pwid.f"]*pnat0["pwid.r.f"],
      race = "All") %>%
    filter(!is.na(inc)) -> pwid_f0  

  if(i==1){
    msm <- msm0
    wsm <- wsm0
    msw <- msw0
    
    pwid_f <- pwid_f0
    pwid_m <- pwid_m0
    
  } else{
    col_name <- paste0("inc", i)
    col_name2 <- paste0("prep.ind", i)
    
    msm <- cbind(msm, setNames(data.frame(msm0$inc), col_name), setNames(data.frame(msm0$prep.ind), col_name2))
    wsm <- cbind(wsm, setNames(data.frame(wsm0$inc), col_name), setNames(data.frame(wsm0$prep.ind), col_name2))
    msw <- cbind(msw, setNames(data.frame(msw0$inc), col_name), setNames(data.frame(msw0$prep.ind), col_name2))
    
    pwid_f <- cbind(pwid_f, setNames(data.frame(pwid_f0$inc), col_name), setNames(data.frame(pwid_f0$prep.ind), col_name2))
    pwid_m <- cbind(pwid_m, setNames(data.frame(pwid_m0$inc), col_name), setNames(data.frame(pwid_m0$prep.ind), col_name2))
  }
}

#### OUTPUTS --> number of incident cases

msm %>%
  left_join(asr_msm, by=c("state", "race", "age")) %>%
  mutate(trnsm = "msm") %>%
  mutate(across(
    starts_with("inc"), 
    .fns = ~ .x * pop * get(sub("inc", "prep.ind", cur_column())), # Multiply corresponding prep.ind column
    .names = "pop{col}" )) %>% 
  mutate(across(
    starts_with("prep.ind"), 
    .fns = ~ .x * pop, 
    .names = "tot.{col}")) %>%
  filter(!is.na(pop)) -> msm_out

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
    .names = "tot.{col}")) %>%
  filter(!is.na(pop)) -> wsm_out

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
    .names = "tot.{col}")) %>%
  filter(!is.na(pop)) -> msw_out

#####################

pop.m0 <- (read_csv(here("output_data/pop_m.csv")))
pop.f0 <- (read_csv(here("output_data/pop_f.csv")))

pop.f0 %>%
  mutate(pop = `18-24`+ `25-34`+ `35-44`) %>%
  select(state, FIPS, pop) -> pop.f.18_44

pop.m0 %>%
  mutate(pop = `18-24`+ `25-34`+ `35-44`) %>%
  select(state, FIPS, pop) -> pop.m.18_44

pwid_f %>%
  left_join(pop.f.18_44, by=c("state")) %>%
  mutate(trnsm = "pwid") %>%
  mutate(across(
    starts_with("inc"), 
    .fns = ~ .x * pop * get(sub("inc", "prep.ind", cur_column())), # Multiply corresponding prep.ind column
    .names = "pop{col}" )) %>% 
  mutate(across(
    starts_with("prep.ind"), 
    .fns = ~ .x * pop, 
    .names = "tot.{col}")) -> pwid_f_out


pwid_m %>%
  left_join(pop.m.18_44, by=c("state")) %>%
  mutate(trnsm = "pwid") %>%
  mutate(across(
    starts_with("inc"), 
    .fns = ~ .x * pop * get(sub("inc", "prep.ind", cur_column())), # Multiply corresponding prep.ind column
    .names = "pop{col}" )) %>% 
  mutate(across(
    starts_with("prep.ind"), 
    .fns = ~ .x * pop, 
    .names = "tot.{col}")) -> pwid_m_out
  
##########################################
#### FIGURES // State

rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out) %>%
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
  geom_pointrange(aes(y=state, x=cases, xmin=cases_ll, xmax=cases_ul), color="steelblue", alpha=0.9) +
  geom_pointrange(aes(y=state, xmin=q025, x=q50, xmax=q975 ), color="darkorange", alpha=0.7) +
  theme_minimal() + xlab("Number of new HIV infections") + xlim(c(0,5000)) +
  facet_wrap(~calibration, ncol=2) + xlim(c(0, NA)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.major.y = element_line(color = "black", size = 0.1),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black") 
  ) -> p_st

ggsave(here("output_figures/calib_state.png"), plot = p_st, width = 8, height = 6, dpi = 300)


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
rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"), -starts_with("tot.prep.ind")) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  rowwise() %>%
  summarise(
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%
  mutate(cases =df_incid_nat$cases,
         cases_ll =df_incid_nat$cases_ll,
         cases_ul =df_incid_nat$cases_ul) %>%
  ggplot() + 
  geom_pointrange(aes(y="National", x=cases, xmin=cases_ll, xmax=cases_ul), color="steelblue", alpha=0.5) +
  geom_pointrange(aes(y="National", xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw() + xlab("Number of new HIV infections")  + xlim(c(0, 60000)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black") 
  ) -> p_nat

ggsave(here("output_figures/calib_national.png"), plot = p_nat, width = 5, height = 5, dpi = 300)


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
  geom_pointrange(aes(y=race, x=cases, xmin=cases_ll, xmax=cases_ul), color="steelblue", alpha=0.5) +
  geom_pointrange(aes(y=race, xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw() + xlab("Number of new HIV infections") + ylab("Race/Ethnicity")  +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black") 
  ) -> p_re

ggsave(here("output_figures/calib_re.png"), plot = p_re, width = 5, height = 4, dpi = 300)


#### FIGURES // Transmission risk group

rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out) %>%
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
  geom_pointrange(aes(y=trnsm, x=cases, xmin=cases_ll, xmax=cases_ul), color="steelblue", alpha=0.5) +
  geom_pointrange(aes(y=trnsm, xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw()  + xlim(c(0, NA)) + xlab("Number of new HIV infections") + ylab("Transmission risk group") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black") 
  ) -> p_trnsm

ggsave(here("output_figures/calib_trnsm.png"), plot = p_trnsm, width = 5, height = 5, dpi = 300)


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
  geom_pointrange(aes(y=age, x=cases, xmin=cases_ll, xmax=cases_ul), color="steelblue", alpha=0.5) +
  geom_pointrange(aes(y=age, xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw() + xlim(c(0, NA)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black") 
  ) -> p_age

ggsave(here("output_figures/calib_age.png"), plot = p_age, width = 5, height = 4, dpi = 300)

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
  geom_pointrange(aes(y=age, x=cases, xmin=cases_ll, xmax=cases_ul), color="steelblue", alpha=0.5) +
  geom_pointrange(aes(y=age, xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw() + xlim(c(0, NA)) + xlab("Number of new HIV infections") + ylab("Age group") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black") 
  ) -> p_msm_age

ggsave(here("output_figures/calib_msm_age.png"), plot = p_msm_age, width = 5, height = 4, dpi = 300)

#### FIGURES // PWID

rbind(pwid_f_out, pwid_m_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"), -starts_with("tot.prep.ind")) %>%
  group_by(sex) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    sex = sex,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%
  left_join(df_incid_pwid, by=c("sex")) %>%
  ggplot() + 
  geom_pointrange(aes(y=sex, x=cases, xmin=cases_ll, xmax=cases_ul), color="steelblue", alpha=0.5) +
  geom_pointrange(aes(y=sex, xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
  theme_bw() + xlim(c(0, NA)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black") 
  ) -> p_pwid_sex

ggsave(here("output_figures/calib_pwid_sex.png"), plot = p_pwid_sex, width = 5, height = 4, dpi = 300)

# 
# rbind(pwid_f_out, pwid_m_out) %>%
#   select(-starts_with("inc"), -starts_with("prep.in"), -starts_with("tot.prep.ind")) %>%
#   group_by(sex) %>%
#   summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
#   pivot_longer(cols = starts_with("popinc"), names_to = "variable", values_to = "value") %>%
#   group_by(variable) %>%
#   summarize(total_sum = sum(value), 
#             female_sum = sum(value[sex == "Female"])) %>%
#   ungroup() %>%
#   mutate(female_proportion = female_sum / total_sum) %>%
#   summarise(
#     q025 = quantile(female_proportion, 0.025, na.rm = TRUE),
#     q50 = quantile(female_proportion, 0.50, na.rm = TRUE),
#     q975 = quantile(female_proportion, 0.975, na.rm = TRUE)
#   ) %>%
#   #left_join(df_incid_pwid, by=c("sex")) %>%
#   ggplot() + 
#   geom_pointrange(aes(y="Female", xmin=q025, x=q50, xmax=q975 ), size=0.5, color="orange", alpha=0.5) +
#   geom_pointrange(aes(y="Female", x=df_incid_pwid$cases[df_incid_pwid$sex=="Female"]/sum(df_incid_pwid$cases), 
#                       xmin=df_incid_pwid$cases_ll[df_incid_pwid$sex=="Female"]/sum(df_incid_pwid$cases_ll), 
#                       xmax=df_incid_pwid$cases_ul[df_incid_pwid$sex=="Female"]/sum(df_incid_pwid$cases_ul)), 
#                   color="steelblue", alpha=0.5) +
#   theme_bw() + xlim(c(0, 1)) +
#   theme(
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(), 
#     axis.line = element_line(color = "black") 
#   ) -> p_pwid_sex


  

