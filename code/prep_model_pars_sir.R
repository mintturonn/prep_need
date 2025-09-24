
# parameters
pars_nat <- read_excel('~/prep_need/data/params.xlsx', sheet = 'nat_hist')
msmpr <- read_excel('~/prep_need/data/params.xlsx', sheet = 'grey_2014')

id_beta <- which(pars_nat$prior_shape == "beta")
id_gamma <- which(pars_nat$prior_shape == "gamma")
id_nrm <- which(pars_nat$prior_shape == "normal")
id_lnrm <- which(pars_nat$prior_shape == "log-normal")
id_unif <- which(pars_nat$prior_shape == "uniform")
id_fixed <- which(pars_nat$prior_shape == "fixed")

# population MSM
msmpr %>% 
  rename(state = State) %>%
  dplyr::select(state, prev_extract) -> msmpr


## PREVALENCE DATA
# cdc hiv prealence data state level by mode 
read_csv("~/prep_need/data/AtlasPlusTableData_hivprev_state_msm.csv", skip=10) %>%
  filter(Year == "2022") %>%
  mutate(
    cases = as.numeric(gsub(",", "", Cases)),
    state = gsub("[[:punct:]]", "", Geography),
    trans_cat = case_when(
      `Transmission Category` == "Male-to-male sexual contact" ~ "msm",   
      `Transmission Category` == "Male-to-male sexual contact and injection drug use" ~ "msm",
      `Transmission Category` == "Heterosexual contact" ~ "msw",
      `Transmission Category` == "Injection drug use" ~ "pwid",
      FALSE ~ NA)) %>%
  filter(trans_cat == "msm") %>%
  dplyr::select(state, cases, trans_cat, FIPS, Sex) %>%
  group_by(state, FIPS, trans_cat, Sex) %>%
  summarize(
    cases = sum(cases)) %>%
  ungroup()  -> hiv_msm_prev0 

 read_csv("~/prep_need/data/AtlasPlusTableData_hivprev_state_phet.csv", skip=10) %>%
    filter(Year == "2022") %>%
    mutate(
      cases = as.numeric(gsub(",", "", Cases)),
      state = gsub("[[:punct:]]", "", Geography) ) %>%
    dplyr::select(state, cases, `Transmission Category`, `Age Group`, Sex, FIPS) %>%
    rename(
      Age.Group = `Age Group`,
      trans_cat = `Transmission Category`) %>%
   mutate(
      trans_cat = case_when(
        trans_cat == "Heterosexual contact" & Sex=="Male" ~ "msw",
        trans_cat == "Heterosexual contact" & Sex=="Female" ~ "wsm",
        FALSE ~ trans_cat), 
      cases = case_when(
        state == "New Hampshire" & is.na(cases) & Sex == "Female" ~ 
          1385 * sum(cases[state == "Vermont" & Sex == "Female"]) / 
          (sum(hiv_msm_prev0$cases[hiv_msm_prev0$state == "Vermont"]) + sum(cases[state == "Vermont"])),
        state == "New Hampshire" & is.na(cases) & Sex == "Male" ~ 
          1385 * sum(cases[state == "Vermont" & Sex == "Male"]) / 
          (sum(hiv_msm_prev0$cases[hiv_msm_prev0$state == "Vermont"]) + sum(cases[state == "Vermont"])),
        TRUE ~ cases) )   %>%
    filter(!is.na(cases)) -> hiv_het_prev 
 
 hiv_msm_prev0 %>%
   mutate(cases = case_when(
     state == "New Hampshire" & is.na(cases)  ~ 
       1385 * sum(cases[state == "Vermont"]) / 
       (sum(cases[state == "Vermont"]) + sum(hiv_het_prev$cases[hiv_het_prev$state == "Vermont"]) ) ,
     TRUE ~ cases)) %>%
   filter(!is.na(cases)) -> hiv_msm_prev 


## DIAGNOSIS DATA 
  # cdc hiv pwid data
  read_csv("~/prep_need/data/AtlasPlusTableData_hivdiag_pwid.csv", skip=8) %>%
    filter(Year == "2022" | Year == "2021" | Year ==  "2020 (COVID-19 Pandemic)") %>%
    mutate(
      cases = as.numeric(gsub(",", "", Cases)),
      state = gsub("[[:punct:]]", "", Geography)) %>%
    dplyr::select(state, cases, FIPS) %>%
    group_by(state, FIPS) %>%
    summarize(cases = sum(cases, na.rm = TRUE)) %>%
    ungroup() %>%
    filter( FIPS<60) %>%
    filter(!is.na(cases)) -> hiv_pwid_diag
 
## VIRAL SUPPRESSION   
  
  read_csv("~/prep_need/data/AtlasPlusTableData_vs_state_female_trnsm.csv", skip=9) %>%
    filter(Year=="2022") %>%
    mutate(vsupp_cases = as.numeric(gsub(",", "", Cases)),
           vsupp_perc = as.numeric(Percent)/100,
           state = gsub("[[:punct:]]", "", Geography),
           trans_cat = `Transmission Category`,
           trans_cat = case_when(
             trans_cat == "Heterosexual contact" ~ "wsm",
             trans_cat == "Injection drug use" ~ "pwid",
             FALSE ~ trans_cat)) %>% 
    filter(trans_cat != "Other") %>%
    filter( FIPS<60) %>%
    mutate( vsupp_perc = case_when(
      is.na(vsupp_perc) & trans_cat == "pwid" ~ median(vsupp_perc[trans_cat == "pwid"], na.rm=T),
      is.na(vsupp_perc) & trans_cat == "wsm"  ~ median(vsupp_perc[trans_cat == "wsm"], na.rm=T) ,
      TRUE ~ vsupp_perc )) %>%
    dplyr::select(state,  FIPS, Sex, trans_cat,vsupp_perc)  -> hiv_f_trnsm_vsuppr 
  
  
  read_csv("~/prep_need/data/AtlasPlusTableData_vs_state_male_trnsm.csv", skip=9) %>%
    filter(Year=="2022") %>%
    mutate(vsupp_cases = as.numeric(gsub(",", "", Cases)),
           vsupp_pop = as.numeric(gsub(",", "", Population)),
           vsupp_perc = as.numeric(Percent)/100,
           state = gsub("[[:punct:]]", "", Geography),
           trans_cat = `Transmission Category`,
           trans_cat = case_when(
             trans_cat == "Male-to-male sexual contact" ~ "msm",   
             trans_cat == "Male-to-male sexual contact and injection drug use" ~ "msm",
             trans_cat == "Heterosexual contact" ~ "msw",
             trans_cat == "Injection drug use" ~ "pwid",
             FALSE ~ trans_cat)) %>% # leaves "other" out
    filter(trans_cat != "Other") %>%
    group_by(state, FIPS, trans_cat, Sex) %>%
    summarize(vsupp_perc = sum(vsupp_cases) / sum(vsupp_pop)) %>%
    ungroup() %>%
    filter( FIPS<60) %>%
    mutate( vsupp_perc = case_when(
      is.na(vsupp_perc) & trans_cat == "msm" ~ median(vsupp_perc[trans_cat == "msm"], na.rm=T),
      is.na(vsupp_perc) & trans_cat == "pwid" ~ median(vsupp_perc[trans_cat == "pwid"], na.rm=T),
      is.na(vsupp_perc) & trans_cat == "msw" ~ median(vsupp_perc[trans_cat == "msw"], na.rm=T),
      TRUE ~ vsupp_perc )) %>%
    dplyr::select(state,  FIPS, Sex, trans_cat, vsupp_perc)  -> hiv_m_trnsm_vsuppr 
  

## POPULATION SIZE  
# denominator by state
# pop_asr <- read_csv(here("data/acs5_state_sex_age_race.csv"))
# pop_as <- read_csv(here("data/acs5_state_sex_age.csv"))
# pop <- pop_fun(2019) # this is v. similar to PUMS

# write.csv(pop$m,    here("output_data/pop_m.csv"), row.names = FALSE)
# write.csv(pop$f,    here("output_data/pop_f.csv"), row.names = FALSE)

pop.m0 <- (read_csv("~/prep_need/output_data/pop_m.csv"))
pop.f0 <- (read_csv("~/prep_need/output_data/pop_f.csv"))

pop.m0 %>%
  left_join(msmpr, by = ("state") ) %>%
  pivot_longer(cols = c("13-24", "25-34", "35-44", "45-54", "55+")) %>%
  mutate( msm = value * prev_extract) %>%
  mutate( msw = value * (1-prev_extract)) %>%
  mutate( agecat = ifelse(name == "18-24" | name == "13-17" | name =="25-34" | name == "35-44", "younger", "older")) %>%
  rename(`Age.Group` = name) %>%
  group_by(FIPS, state) %>%
  summarize(msm_denom = sum(msm),
            msw_denom = sum(msw)) %>%
  ungroup() -> pop.m.all   

pop.f0 %>%
  pivot_longer(cols = c("13-24", "25-34", "35-44", "45-54", "55+")) %>%
  mutate( agecat = ifelse(name == "13-17" | name == "18-24" | name =="25-34" | name == "35-44", "younger", "older")) %>%
  rename(`Age.Group` = name) %>%
 # group_by(FIPS, state, agecat) %>%
  group_by(FIPS, state) %>%
  summarize(wsm_denom = sum(value)) %>%
  ungroup()  -> pop.f.all

###############################################################
## COMBINING DATA

hiv_het_prev  %>%
  filter(Sex != "Female") %>% # PARTNER pool prevalence 
  left_join(pop.m.all, by = c("FIPS",  "state")) %>%
  mutate(prev = cases/ msw_denom) %>% 
  left_join(hiv_f_trnsm_vsuppr[hiv_f_trnsm_vsuppr$trans_cat == "wsm",], by = c("FIPS", "state")) %>%
  mutate(wsm_prev_min = prev,
         wsm_prev_max = prev * 2,
         wsm_vsupp_min = vsupp_perc *0.9,
         wsm_vsupp_max = ifelse(vsupp_perc *1.1> 0.99, 0.99, vsupp_perc *1.1)) %>% 
  dplyr::select(FIPS, state,  wsm_prev_min, wsm_prev_max, wsm_vsupp_min, wsm_vsupp_max) %>%
  arrange(FIPS) -> pop.wsm

hiv_het_prev  %>%
  filter(Sex != "Male") %>% # PARTNER pool prevalence 
  group_by(state, FIPS, trans_cat) %>%
  mutate(pr.num = sum(cases)) %>%
  ungroup()  %>%
  left_join(pop.f.all, by = c("FIPS",  "state")) %>%
  mutate(prev = pr.num/ wsm_denom,
         trans_cat = "msw",
         Sex = "Male")  %>%
  left_join(hiv_m_trnsm_vsuppr[hiv_m_trnsm_vsuppr$trans_cat == "msw",], by = c("FIPS","state","trans_cat")) %>%
  mutate(msw_prev_min = prev ,
         msw_prev_max = prev * 2,
         msw_vsupp_min = vsupp_perc *0.9,
         msw_vsupp_max = ifelse(vsupp_perc *1.1> 0.99, 0.99, vsupp_perc *1.1)) %>% 
  dplyr::select(FIPS, state, msw_prev_min, msw_prev_max, msw_vsupp_min, msw_vsupp_max) %>%
  arrange(FIPS) -> pop.msw

hiv_msm_prev  %>%
  group_by(state, FIPS) %>% 
  mutate(pr.num = sum(cases)) %>%
  ungroup()  %>%
  left_join(pop.m.all, by = c("FIPS", "state")) %>%
  mutate(prev = pr.num/ msm_denom) %>%
  mutate(susc = msm_denom-pr.num) %>%
  left_join(hiv_m_trnsm_vsuppr[hiv_m_trnsm_vsuppr$trans_cat == "msm",], by = c("FIPS", "state", "trans_cat")) %>%
  mutate(msm_prev_min = prev *0.8,
         msm_prev_max = prev * 2,
         msm_vsupp_min = vsupp_perc *0.9,
         msm_vsupp_max = ifelse(vsupp_perc *1.1> 0.99, 0.99, vsupp_perc *1.1)) %>% 
  dplyr::select(FIPS, state,  msm_prev_min, msm_prev_max, msm_vsupp_min, msm_vsupp_max) %>%
  arrange(FIPS)-> pop.msm

rbind(pop.m0, pop.f0) %>%
  mutate(pop =  `18-24`+ `25-34`+ `35-44`) %>%
  group_by(state, FIPS) %>%
    summarize(pop=sum(pop)) %>%
  ungroup() %>%
  dplyr::select(FIPS, state, pop) %>%
  mutate(pwid_pop = 0.0146*pop,
         trans_cat = "pwid") %>%
  left_join(hiv_m_trnsm_vsuppr[hiv_m_trnsm_vsuppr$trans_cat == "pwid",], by = c("FIPS", "state", "trans_cat")) %>%
  left_join(hiv_pwid_diag, by = c("FIPS", "state")) %>%
  mutate(
         prev = cases/pwid_pop,
         pwid_prev_min = prev ,
         pwid_prev_max = prev * 2,
         pwid_vsupp_min = vsupp_perc *0.9,
         pwid_vsupp_max = ifelse(vsupp_perc *1.1> 0.99, 0.99, vsupp_perc *1.1)) %>% 
  dplyr::select(-pop) -> pop.pwid

## this is calculated for the ll function [ pwid ]
pop.f0 %>%
  mutate(pop = `18-24`+ `25-34`+ `35-44`) %>%
  dplyr::select(state, FIPS, pop) -> pop.f.18_44

pop.m0 %>%
  mutate(pop = `18-24`+ `25-34`+ `35-44`) %>%
  dplyr::select(state, FIPS, pop) -> pop.m.18_44

###################

pop.msm %>%
  left_join(pop.msw, by = c("FIPS", "state")) %>%
  left_join(pop.wsm, by = c("FIPS", "state")) %>%
  left_join(pop.pwid, by = c("FIPS", "state")) %>%
  relocate(msm_prev_min, msm_prev_max, wsm_prev_min, wsm_prev_max, msw_prev_min, msw_prev_max, pwid_prev_min, pwid_prev_max, 
           msm_vsupp_min, msm_vsupp_max, wsm_vsupp_min, wsm_vsupp_max, msw_vsupp_min, msw_vsupp_max, pwid_vsupp_min, pwid_vsupp_max) %>%
  filter(state != "Puerto Rico") -> pars_state


