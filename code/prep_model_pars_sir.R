
# parameters
pars_nat <- read_excel(here('data/params.xlsx'), sheet = 'nat_hist')
msmpr <- read_excel(here('data/params.xlsx'), sheet = 'grey_2014')

id_beta <- which(pars_nat$prior_shape == "beta")
id_lnrm <- which(pars_nat$prior_shape == "log-normal")
id_unif <- which(pars_nat$prior_shape == "uniform")
id_fixed <- which(pars_nat$prior_shape == "fixed")

# rr_re_msm <- c(0.93, 0.81, 4.6, 2.42, 1.48, 1)
# rr_re_msw <- c(2.25, 1.25, 25.75, 5.75, 0.75, 1)
# rr_re_wsm <- c(2.11, 1.11, 20.44, 4.11, 2.67, 1)

msmpr %>% 
  rename(state = State) %>%
  select(state, prev_extract) -> msmpr

## PREVALENCE
read_csv(here("data", "hiv_prevalence_prediction.csv")) %>%
           rename(Age.Group = age) %>%
  filter(year == 2022) %>%
  group_by(FIPS, state, Sex, transcat, Age.Group) %>%
  summarize(hpr = sum(m7)) %>%
  ungroup() -> hiv_pred


# cdc hiv data state level by mode 
read_excel(here("data/AtlasPlusTableData_hivprev_2019_age_state.xlsx"), skip=10) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  select(state, cases, `Transmission Category`, `Age Group`, FIPS) %>%
  filter(`Transmission Category` == "Male-to-male sexual contact" |
           `Transmission Category` ==   "Male-to-male sexual contact and injection drug use") %>%
  mutate( agecat = ifelse(`Age Group` == "13-24" | `Age Group` =="25-34" | `Age Group` == "35-44", "younger", "older")) %>%
  rename(Age.Group = `Age Group`) %>%
  mutate(transcat = "Male-to-male sexual contact") %>%
  select(-`Transmission Category`) %>%
  mutate(Sex = "Male") %>%
  left_join(hiv_pred , by = c("FIPS", "state", "Sex", "transcat", "Age.Group")) %>%
  mutate(cases = ifelse(is.na(cases), hpr, cases)) %>%
  filter(!is.na(cases)) %>%
  group_by(state, Age.Group,  FIPS, agecat, Sex) %>% 
  summarize(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(transcat = "Male-to-male sexual contact") -> hiv_age_msm_prev 

# this is awkward
hiv_age_msm_prev_13_17 <- hiv_age_msm_prev_18_24 <- hiv_age_msm_prev[hiv_age_msm_prev$Age.Group=="13-24",] 
hiv_age_msm_prev_13_17$Age.Group <- "13-17"
hiv_age_msm_prev_18_24$Age.Group <- "18-24"
hiv_age_msm_prev <- rbind(hiv_age_msm_prev,hiv_age_msm_prev_13_17, hiv_age_msm_prev_18_24)

read_excel(here("data/AtlasPlusTableData_hivprev_2019_age_state_het.xlsx"), skip=10) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  select(state, cases, `Transmission Category`, `Age Group`, Sex, FIPS) %>%
  filter(`Transmission Category` == "Heterosexual contact" |
           `Transmission Category` ==   "Injection drug use")  %>%
  mutate( agecat = ifelse(`Age Group` == "13-24" | `Age Group` =="25-34" | `Age Group` == "35-44", "younger", "older")) %>%
  rename(Age.Group = `Age Group`) %>% 
  rename(transcat = `Transmission Category`) %>%
  left_join(hiv_pred , by = c("FIPS", "state", "Sex", "transcat", "Age.Group")) %>%
  mutate(cases = ifelse(is.na(cases), hpr, cases)) %>%
  filter(!is.na(cases)) -> hiv_age_het_prev 

# this is awkward
hiv_age_het_prev_13_17 <- hiv_age_het_prev_18_24 <- hiv_age_het_prev[hiv_age_het_prev$Age.Group=="13-24",] 
hiv_age_het_prev_13_17$Age.Group <- "13-17"
hiv_age_het_prev_18_24$Age.Group <- "18-24"
hiv_age_het_prev <- rbind(hiv_age_het_prev,hiv_age_het_prev_13_17, hiv_age_het_prev_18_24)


# read_excel(here("data/AtlasPlusTableData_hivprev_2019_state.xlsx"), skip=10) %>%
#   mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
#   mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
#   select(state, cases, `Transmission Category`, FIPS)   -> hiv_prev


## HIV diagnoses
## I think this is not used ??
read_excel(here("data/AtlasPlusTableData_hivdiag_2019_age_state_msm.xlsx"), skip=10) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  select(state, cases, `Transmission Category`, `Age Group`, FIPS) %>%
  filter(`Transmission Category` == "Male-to-male sexual contact" |
           `Transmission Category` ==   "Male-to-male sexual contact and injection drug use")  %>%
  select(-`Transmission Category`) %>%
  mutate( agecat = ifelse(`Age Group` == "13-24" | `Age Group` =="25-34" | `Age Group` == "35-44", "younger", "older")) %>%
  rename(Age.Group = `Age Group`) %>%
  group_by(state, Age.Group,  FIPS, agecat) %>% 
  summarize(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(transcat = "Male-to-male sexual contact") -> hiv_age_msm_diag 

# this is awkward
hiv_age_msm_diag_13_17 <- hiv_age_msm_diag_18_24 <- hiv_age_msm_diag[hiv_age_msm_diag$Age.Group=="13-24",] 
hiv_age_msm_diag_13_17$Age.Group <- "13-17"
hiv_age_msm_diag_18_24$Age.Group <- "18-24"
hiv_age_msm_diag <- rbind(hiv_age_msm_diag,hiv_age_msm_diag_13_17, hiv_age_msm_diag_18_24)


# read_excel(here("data/AtlasPlusTableData_hivdiag_2019_age_state_msm.xlsx"), skip=10) %>%
#   mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
#   mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
#   select(state, cases, `Transmission Category`, `Age Group`, FIPS) %>%
#   filter(`Transmission Category` == "Other")  -> other_msm_test

read_excel(here("data/AtlasPlusTableData_hivdiag_2019_age_state_het.xlsx"), skip=10) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  select(state, cases, `Transmission Category`, `Age Group`, Sex, FIPS) %>%
  filter(`Transmission Category` == "Heterosexual contact" |
           `Transmission Category` ==   "Injection drug use")  %>%
  mutate( agecat = ifelse(`Age Group` == "13-24" | `Age Group` =="25-34" | `Age Group` == "35-44", "younger", "older")) %>%
  rename(Age.Group = `Age Group`) %>%
  rename(transcat = `Transmission Category`) -> hiv_age_het_diag

# this is awkward
hiv_age_het_diag_13_17 <- hiv_age_het_diag_18_24 <- hiv_age_het_diag[hiv_age_het_diag$Age.Group=="13-24",] 
hiv_age_het_diag_13_17$Age.Group <- "13-17"
hiv_age_het_diag_18_24$Age.Group <- "18-24"
hiv_age_het_diag <- rbind(hiv_age_het_diag,hiv_age_het_diag_13_17, hiv_age_het_diag_18_24)


# HIV viral suppression
read_excel(here("data/AtlasPlusTableData_treat_2019_state_age.xlsx"), skip=9) %>%
  mutate(vsupp_cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(vsupp_perc = as.numeric(Percent)/100) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  select(state, vsupp_cases, vsupp_perc, Indicator, Sex, `Age Group`, FIPS) %>%
  filter(Indicator == "HIV viral suppression")  %>%
  mutate( agecat = ifelse(`Age Group` == "13-24" | `Age Group` =="25-34" | `Age Group` == "35-44", "younger", "older")) %>%
  rename(Age.Group = `Age Group`) %>%
  mutate(vsupp_perc = ifelse(is.na(vsupp_perc), median(vsupp_perc, na.rm=T), vsupp_perc))-> hiv_vsuppr

# this is awkward
hiv_vsuppr_13_17 <- hiv_vsuppr_18_24 <- hiv_vsuppr[hiv_vsuppr$Age.Group=="13-24",] 
hiv_vsuppr_13_17$Age.Group <- "13-17"
hiv_vsuppr_18_24$Age.Group <- "18-24"
hiv_vsuppr <- rbind(hiv_vsuppr,hiv_vsuppr_13_17, hiv_vsuppr_18_24)



# denominator by state
# pop_asr <- read_csv(here("data/acs5_state_sex_age_race.csv"))
# pop_as <- read_csv(here("data/acs5_state_sex_age.csv"))
 pop <- pop_fun(2019) # this is similar to PUMS

pop$m %>%
  left_join(msmpr, by = ("state") ) %>%
  pivot_longer(cols = c("13-24","13-17","18-24", "25-34", "35-44", "45-54", "55+")) %>%
  mutate( msm = value * prev_extract) %>%
  mutate( msw = value * (1-prev_extract)) %>%
  mutate( agecat = ifelse(name == "18-24" | name == "13-17" | name =="25-34" | name == "35-44", "younger", "older")) %>%
  rename(`Age.Group` = name) %>%
  group_by(FIPS, state, `Age.Group`) %>%
  summarize(msm = sum(msm),
         msw = sum(msw)) %>%
  ungroup() %>%
  group_by(FIPS, state) %>%
 mutate(msm_denom = sum(msm),
        msw_denom = sum(msw)) -> pop.m.all #   

pop$f %>%
  pivot_longer(cols = c("13-24","13-17","18-24", "25-34", "35-44", "45-54", "55+")) %>%
  mutate( agecat = ifelse(name == "13-17" | name == "18-24" | name =="25-34" | name == "35-44", "younger", "older")) %>%
  rename(`Age.Group` = name) %>%
 # group_by(FIPS, state, agecat) %>%
  group_by(FIPS, state, `Age.Group`) %>%
  summarize(wsm = sum(value)) %>%
  ungroup() %>%
  group_by(FIPS, state) %>%
  mutate(wsm_denom = sum(wsm)) -> pop.f.all

# pop.f.all$wsm_denom <- pop.f.all$wsm
# 
# pop.f.all$wsm_denom[pop.f.all$Age.Group=="13-17"] <- pop.f.all$wsm[pop.f.all$Age.Group=="13-24"]
# pop.f.all$wsm_denom[pop.f.all$Age.Group=="18-24"] <- pop.f.all$wsm[pop.f.all$Age.Group=="13-24"]


hiv_age_het_prev  %>%
  filter(Sex != "Female") %>% # PARTNER pool prevalence 
  left_join(hiv_age_het_diag[hiv_age_het_diag$Sex !="Female",] , by = c("FIPS", "Age.Group", "transcat", "agecat")) %>%
#  group_by(state.x, FIPS, agecat, `Transmission Category`) %>%
  group_by(state.x, FIPS, transcat) %>%
  mutate(pr.num = sum(cases.x),
        di.num = sum(cases.y)) %>%
  ungroup()  %>%
  left_join(pop.m.all, by = c("FIPS",  "Age.Group")) %>%
  mutate(prev = pr.num/ msw_denom) %>% # 13-24 years as denominator for adolescents
  mutate(susc = msw-pr.num) %>%
  mutate(diag = di.num/ susc) %>%
  filter(transcat != "Injection drug use" ) %>%
  filter(!is.na(Age.Group)) %>%
 # select(FIPS, state.x, agecat, prev, diag, msw) %>%
  select(FIPS, state.x, Age.Group, prev, diag, msw) %>%
  left_join(hiv_vsuppr[hiv_vsuppr$Sex!="Female",], by = c("FIPS", "Age.Group")) %>%
  mutate(wsm_prev_min = prev * 0.9,
         wsm_prev_max = prev * 2,
         wsm_vsupp_min = vsupp_perc *0.9,
         wsm_vsupp_max = ifelse(vsupp_perc *1.1> 0.99, 0.99, vsupp_perc *1.1)) %>% 
  select(FIPS, state, Age.Group, wsm_prev_min, wsm_prev_max, wsm_vsupp_min, wsm_vsupp_max) %>%
  arrange(FIPS, Age.Group) -> pop.wsm

hiv_age_het_prev  %>%
  filter(Sex != "Male") %>% # PARTNER pool prevalence 
  left_join(hiv_age_het_diag[hiv_age_het_diag$Sex !="Male",] , by = c("FIPS", "Age.Group", "transcat", "agecat")) %>%
 # group_by(state.x, FIPS, agecat, `Transmission Category`) %>%
  group_by(state.x, FIPS, transcat) %>%
  mutate(pr.num = sum(cases.x),
            di.num = sum(cases.y)) %>%
  ungroup()  %>%
  left_join(pop.f.all, by = c("FIPS", "Age.Group")) %>%
  mutate(prev = pr.num/ wsm_denom) %>%
  mutate(susc = wsm-pr.num) %>%
  mutate(diag = di.num/ susc) %>%
  filter(transcat != "Injection drug use" ) %>%
  filter(!is.na(Age.Group)) %>%
  select(FIPS, state.x, Age.Group, prev, diag, wsm) %>%
  left_join(hiv_vsuppr[hiv_vsuppr$Sex!="Male",], by = c("FIPS", "Age.Group")) %>%
  mutate(msw_prev_min = prev ,
         msw_prev_max = prev * 2,
         msw_vsupp_min = vsupp_perc *0.9,
         msw_vsupp_max = ifelse(vsupp_perc *1.1> 0.99, 0.99, vsupp_perc *1.1)) %>% 
  select(FIPS, state, Age.Group, msw_prev_min, msw_prev_max, msw_vsupp_min, msw_vsupp_max) %>%
  arrange(FIPS, Age.Group) -> pop.msw

hiv_age_msm_prev  %>%
  left_join(hiv_age_msm_diag, by = c("FIPS", "state", "Age.Group", "transcat", "agecat"))  %>%
 # group_by(state.x, FIPS, agecat) %>% # attribute all to MSM contact (combines IDU and MSM)
  group_by(state, FIPS) %>% # attribute all to MSM contact (combines IDU and MSM)
  mutate(pr.num = sum(cases.x),
            di.num = sum(cases.y)) %>%
  ungroup()  %>%
  left_join(pop.m.all, by = c("FIPS", "Age.Group")) %>%
  mutate(prev = pr.num/ msm_denom) %>%
  mutate(susc = msm_denom-pr.num) %>%
  mutate(diag = di.num/ susc)  %>%
  filter(!is.na(Age.Group)) %>%
  select(FIPS, state.x, Age.Group, prev, diag, msm) %>%
  left_join(hiv_vsuppr[hiv_vsuppr$Sex=="Male",], by = c("FIPS", "Age.Group")) %>%
  mutate(msm_prev_min = prev ,
         msm_prev_max = prev * 2,
         msm_vsupp_min = vsupp_perc *0.9,
         msm_vsupp_max = ifelse(vsupp_perc *1.1> 0.99, 0.99, vsupp_perc *1.1)) %>% 
  select(FIPS, state, Age.Group, msm_prev_min, msm_prev_max, msm_vsupp_min, msm_vsupp_max) %>%
  arrange(FIPS, Age.Group)-> pop.msm

pop.msm %>%
  left_join(pop.msw, by = c("FIPS", "state", "Age.Group")) %>%
  left_join(pop.wsm, by = c("FIPS", "state", "Age.Group")) %>%
  filter(Age.Group=="55+") %>%
  select(-Age.Group) %>% 
  relocate(msm_prev_min, msm_prev_max, wsm_prev_min, wsm_prev_max, msw_prev_min, msw_prev_max, msm_vsupp_min, msm_vsupp_max, wsm_vsupp_min, wsm_vsupp_max, msw_vsupp_min, msw_vsupp_max) -> pars_state



################### MSM
# hivdat, beta0, nact, safe, safe_eff, contact
# 
# pop.msm$bact <- par$base_case[par$params == "msm.bact"]
# pop.msm$bact_min <- par$base_min[par$params == "msm.bact"]
# pop.msm$bact_max <- par$base_max[par$params == "msm.bact"]
# # pop.msm$bact_pair <- par$base_case[par$params == "msm.bact_pair"]
# pop.msm$cond_eff <- par$base_case[par$params == "msm.cond_eff"]
# pop.msm$cond_eff_min <- par$base_min[par$params == "msm.cond_eff"]
# pop.msm$cond_eff_max <- par$base_max[par$params == "msm.cond_eff"]
# 
# pop.msm$cond[pop.msm$`Age.Group` == "13-17"]   <- par$base_case[par$params == "msm.cond0"]
# pop.msm$cond[pop.msm$`Age.Group` == "18-24"]   <- par$base_case[par$params == "msm.cond1"]
# pop.msm$cond[pop.msm$`Age.Group` == "13-24"]   <- par$base_case[par$params == "msm.cond1"]
# pop.msm$cond[pop.msm$`Age.Group` == "25-34"]   <- par$base_case[par$params == "msm.cond2"]
# pop.msm$cond[pop.msm$`Age.Group` == "35-44"]   <- par$base_case[par$params == "msm.cond3"]
# pop.msm$cond[pop.msm$`Age.Group` == "45-54"]   <- par$base_case[par$params == "msm.cond4"]
# pop.msm$cond[pop.msm$`Age.Group` == "55+"]     <- par$base_case[par$params == "msm.cond5"]
# 
# pop.msm$cond_min[pop.msm$`Age.Group` == "13-17"]   <- par$base_min[par$params == "msm.cond0"]
# pop.msm$cond_min[pop.msm$`Age.Group` == "18-24"]   <- par$base_min[par$params == "msm.cond1"]
# pop.msm$cond_min[pop.msm$`Age.Group` == "13-24"]   <- par$base_min[par$params == "msm.cond1"]
# pop.msm$cond_min[pop.msm$`Age.Group` == "25-34"]   <- par$base_min[par$params == "msm.cond2"]
# pop.msm$cond_min[pop.msm$`Age.Group` == "35-44"]   <- par$base_min[par$params == "msm.cond3"]
# pop.msm$cond_min[pop.msm$`Age.Group` == "45-54"]   <- par$base_min[par$params == "msm.cond4"]
# pop.msm$cond_min[pop.msm$`Age.Group` == "55+"]     <- par$base_min[par$params == "msm.cond5"]
# 
# pop.msm$cond_max[pop.msm$`Age.Group` == "13-17"]   <- par$base_max[par$params == "msm.cond0"]
# pop.msm$cond_max[pop.msm$`Age.Group` == "18-24"]   <- par$base_max[par$params == "msm.cond1"]
# pop.msm$cond_max[pop.msm$`Age.Group` == "13-24"]   <- par$base_max[par$params == "msm.cond1"]
# pop.msm$cond_max[pop.msm$`Age.Group` == "25-34"]   <- par$base_max[par$params == "msm.cond2"]
# pop.msm$cond_max[pop.msm$`Age.Group` == "35-44"]   <- par$base_max[par$params == "msm.cond3"]
# pop.msm$cond_max[pop.msm$`Age.Group` == "45-54"]   <- par$base_max[par$params == "msm.cond4"]
# pop.msm$cond_max[pop.msm$`Age.Group` == "55+"]     <- par$base_max[par$params == "msm.cond5"]
# 
# pop.msm$contact[pop.msm$`Age.Group` == "13-17"]   <- par$base_case[par$params == "msm.contact0"]
# pop.msm$contact[pop.msm$`Age.Group` == "18-24"]   <- par$base_case[par$params == "msm.contact1"]
# pop.msm$contact[pop.msm$`Age.Group` == "13-24"]   <- par$base_case[par$params == "msm.contact1"]
# pop.msm$contact[pop.msm$`Age.Group` == "25-34"]   <- par$base_case[par$params == "msm.contact2"]
# pop.msm$contact[pop.msm$`Age.Group` == "35-44"]   <- par$base_case[par$params == "msm.contact3"]
# pop.msm$contact[pop.msm$`Age.Group` == "45-54"]   <- par$base_case[par$params == "msm.contact4"]
# pop.msm$contact[pop.msm$`Age.Group` == "55+"]     <- par$base_case[par$params == "msm.contact5"]
# 
# pop.msm$contact_min[pop.msm$`Age.Group` == "13-17"]   <- par$base_min[par$params == "msm.contact0"]
# pop.msm$contact_min[pop.msm$`Age.Group` == "18-24"]   <- par$base_min[par$params == "msm.contact1"]
# pop.msm$contact_min[pop.msm$`Age.Group` == "13-24"]   <- par$base_min[par$params == "msm.contact1"]
# pop.msm$contact_min[pop.msm$`Age.Group` == "25-34"]   <- par$base_min[par$params == "msm.contact2"]
# pop.msm$contact_min[pop.msm$`Age.Group` == "35-44"]   <- par$base_min[par$params == "msm.contact3"]
# pop.msm$contact_min[pop.msm$`Age.Group` == "45-54"]   <- par$base_min[par$params == "msm.contact4"]
# pop.msm$contact_min[pop.msm$`Age.Group` == "55+"]     <- par$base_min[par$params == "msm.contact5"]
# 
# pop.msm$contact_max[pop.msm$`Age.Group` == "13-17"]   <- par$base_max[par$params == "msm.contact0"]
# pop.msm$contact_max[pop.msm$`Age.Group` == "18-24"]   <- par$base_max[par$params == "msm.contact1"]
# pop.msm$contact_max[pop.msm$`Age.Group` == "13-24"]   <- par$base_max[par$params == "msm.contact1"]
# pop.msm$contact_max[pop.msm$`Age.Group` == "25-34"]   <- par$base_max[par$params == "msm.contact2"]
# pop.msm$contact_max[pop.msm$`Age.Group` == "35-44"]   <- par$base_max[par$params == "msm.contact3"]
# pop.msm$contact_max[pop.msm$`Age.Group` == "45-54"]   <- par$base_max[par$params == "msm.contact4"]
# pop.msm$contact_max[pop.msm$`Age.Group` == "55+"]     <- par$base_max[par$params == "msm.contact5"]
# 
# # pop.msm$cond_pair <- par$base_case[par$params == "msw.cond_pair"]
# 
# pop.msm$nact   <- par$base_case[par$params == "msm.nact"]
# pop.msm$nact_min   <- par$base_min[par$params == "msm.nact"]
# pop.msm$nact_max   <- par$base_max[par$params == "msm.nact"]
# 
# ################### WSM
# # hivdat, beta0, nact, safe, safe_eff, contact
#  
# pop.wsm$bact <- par$base_case[par$params == "wsm.bact"]
# pop.wsm$bact_min <- par$base_min[par$params == "wsm.bact"]
# pop.wsm$bact_max <- par$base_max[par$params == "wsm.bact"]
# # pop.wsm$bact_pair <- par$base_case[par$params == "wsm.bact_pair"]
# pop.wsm$cond_eff <- par$base_case[par$params == "wsm.cond_eff"]
# pop.wsm$cond_eff_min <- par$base_min[par$params == "wsm.cond_eff"]
# pop.wsm$cond_eff_max <- par$base_max[par$params == "wsm.cond_eff"]

# pop.wsm$cond[pop.wsm$agecat == "older"]   <- par$base_case[par$params == "wsm.cond_older"]
# pop.wsm$cond[pop.wsm$agecat == "younger"] <- par$base_case[par$params == "wsm.cond_younger"]
# pop.wsm$cond_pair <- par$base_case[par$params == "msw.cond_pair"]
# 
# pop.wsm$nact   <- par$base_case[par$params == "wsm.nact"]
# pop.wsm$nact_min    <- par$base_min [par$params == "wsm.nact"]
# pop.wsm$nact_max   <- par$base_max[par$params == "wsm.nact"]
# 
# pop.wsm$cond[pop.wsm$`Age.Group` == "13-17"]   <- par$base_case[par$params == "wsm.cond0"]
# pop.wsm$cond[pop.wsm$`Age.Group` == "18-24"]   <- par$base_case[par$params == "wsm.cond1"]
# pop.wsm$cond[pop.wsm$`Age.Group` == "13-24"]   <- par$base_case[par$params == "wsm.cond1"]
# pop.wsm$cond[pop.wsm$`Age.Group` == "25-34"]   <- par$base_case[par$params == "wsm.cond2"]
# pop.wsm$cond[pop.wsm$`Age.Group` == "35-44"]   <- par$base_case[par$params == "wsm.cond3"]
# pop.wsm$cond[pop.wsm$`Age.Group` == "45-54"]   <- par$base_case[par$params == "wsm.cond4"]
# pop.wsm$cond[pop.wsm$`Age.Group` == "55+"]     <- par$base_case[par$params == "wsm.cond5"]
# 
# pop.wsm$cond_min[pop.wsm$`Age.Group` == "13-17"]   <- par$base_min[par$params == "wsm.cond0"]
# pop.wsm$cond_min[pop.wsm$`Age.Group` == "18-24"]   <- par$base_min[par$params == "wsm.cond1"]
# pop.wsm$cond_min[pop.wsm$`Age.Group` == "13-24"]   <- par$base_min[par$params == "wsm.cond1"]
# pop.wsm$cond_min[pop.wsm$`Age.Group` == "25-34"]   <- par$base_min[par$params == "wsm.cond2"]
# pop.wsm$cond_min[pop.wsm$`Age.Group` == "35-44"]   <- par$base_min[par$params == "wsm.cond3"]
# pop.wsm$cond_min[pop.wsm$`Age.Group` == "45-54"]   <- par$base_min[par$params == "wsm.cond4"]
# pop.wsm$cond_min[pop.wsm$`Age.Group` == "55+"]     <- par$base_min[par$params == "wsm.cond5"]
# 
# pop.wsm$cond_max[pop.wsm$`Age.Group` == "13-17"]   <- par$base_max[par$params == "wsm.cond0"]
# pop.wsm$cond_max[pop.wsm$`Age.Group` == "18-24"]   <- par$base_max[par$params == "wsm.cond1"]
# pop.wsm$cond_max[pop.wsm$`Age.Group` == "13-24"]   <- par$base_max[par$params == "wsm.cond1"]
# pop.wsm$cond_max[pop.wsm$`Age.Group` == "25-34"]   <- par$base_max[par$params == "wsm.cond2"]
# pop.wsm$cond_max[pop.wsm$`Age.Group` == "35-44"]   <- par$base_max[par$params == "wsm.cond3"]
# pop.wsm$cond_max[pop.wsm$`Age.Group` == "45-54"]   <- par$base_max[par$params == "wsm.cond4"]
# pop.wsm$cond_max[pop.wsm$`Age.Group` == "55+"]     <- par$base_max[par$params == "wsm.cond5"]
# 
# pop.wsm$contact[pop.wsm$`Age.Group` == "13-17"]   <- par$base_case[par$params == "wsm.contact0"]
# pop.wsm$contact[pop.wsm$`Age.Group` == "18-24"]   <- par$base_case[par$params == "wsm.contact1"]
# pop.wsm$contact[pop.wsm$`Age.Group` == "13-24"]   <- par$base_case[par$params == "wsm.contact1"]
# pop.wsm$contact[pop.wsm$`Age.Group` == "25-34"]   <- par$base_case[par$params == "wsm.contact2"]
# pop.wsm$contact[pop.wsm$`Age.Group` == "35-44"]   <- par$base_case[par$params == "wsm.contact3"]
# pop.wsm$contact[pop.wsm$`Age.Group` == "45-54"]   <- par$base_case[par$params == "wsm.contact4"]
# pop.wsm$contact[pop.wsm$`Age.Group` == "55+"]     <- par$base_case[par$params == "wsm.contact5"]
# 
# pop.wsm$contact_min[pop.wsm$`Age.Group` == "13-17"]   <- par$base_min[par$params == "wsm.contact0"]
# pop.wsm$contact_min[pop.wsm$`Age.Group` == "18-24"]   <- par$base_min[par$params == "wsm.contact1"]
# pop.wsm$contact_min[pop.wsm$`Age.Group` == "13-24"]   <- par$base_min[par$params == "wsm.contact1"]
# pop.wsm$contact_min[pop.wsm$`Age.Group` == "25-34"]   <- par$base_min[par$params == "wsm.contact2"]
# pop.wsm$contact_min[pop.wsm$`Age.Group` == "35-44"]   <- par$base_min[par$params == "wsm.contact3"]
# pop.wsm$contact_min[pop.wsm$`Age.Group` == "45-54"]   <- par$base_min[par$params == "wsm.contact4"]
# pop.wsm$contact_min[pop.wsm$`Age.Group` == "55+"]     <- par$base_min[par$params == "wsm.contact5"]
# 
# pop.wsm$contact_max[pop.wsm$`Age.Group` == "13-17"]   <- par$base_max[par$params == "wsm.contact0"]
# pop.wsm$contact_max[pop.wsm$`Age.Group` == "18-24"]   <- par$base_max[par$params == "wsm.contact1"]
# pop.wsm$contact_max[pop.wsm$`Age.Group` == "13-24"]   <- par$base_max[par$params == "wsm.contact1"]
# pop.wsm$contact_max[pop.wsm$`Age.Group` == "25-34"]   <- par$base_max[par$params == "wsm.contact2"]
# pop.wsm$contact_max[pop.wsm$`Age.Group` == "35-44"]   <- par$base_max[par$params == "wsm.contact3"]
# pop.wsm$contact_max[pop.wsm$`Age.Group` == "45-54"]   <- par$base_max[par$params == "wsm.contact4"]
# pop.wsm$contact_max[pop.wsm$`Age.Group` == "55+"]     <- par$base_max[par$params == "wsm.contact5"]
# 
# ################### MSW
# # hivdat, beta0, nact, safe, safe_eff, contact
# 
# pop.msw$bact <- par$base_case[par$params == "msw.bact"]
# 
# pop.msw$bact_min <- par$base_min[par$params == "msw.bact"]
# pop.msw$bact_max <- par$base_max[par$params == "msw.bact"]
# # pop.msw$bact_pair <- par$base_case[par$params == "msw.bact_pair"]
# pop.msw$cond_eff <- par$base_case[par$params == "msw.cond_eff"]
# pop.msw$cond_eff_min <- par$base_min[par$params == "msw.cond_eff"]
# pop.msw$cond_eff_max <- par$base_max[par$params == "msw.cond_eff"]
# 
# #pop.msw$cond_pair <- par$base_case[par$params == "msw.cond_pair"]
# 
# pop.msw$nact   <- par$base_case[par$params == "msw.nact"]
# pop.msw$nact_min   <- par$base_min[par$params == "msw.nact"]
# pop.msw$nact_max   <- par$base_max[par$params == "msw.nact"]
# 
# pop.msw$cond[pop.msw$`Age.Group` == "13-17"]   <- par$base_case[par$params == "msw.cond0"]
# pop.msw$cond[pop.msw$`Age.Group` == "18-24"]   <- par$base_case[par$params == "msw.cond1"]
# pop.msw$cond[pop.msw$`Age.Group` == "13-24"]   <- par$base_case[par$params == "msw.cond1"]
# pop.msw$cond[pop.msw$`Age.Group` == "25-34"]   <- par$base_case[par$params == "msw.cond2"]
# pop.msw$cond[pop.msw$`Age.Group` == "35-44"]   <- par$base_case[par$params == "msw.cond3"]
# pop.msw$cond[pop.msw$`Age.Group` == "45-54"]   <- par$base_case[par$params == "msw.cond4"]
# pop.msw$cond[pop.msw$`Age.Group` == "55+"]     <- par$base_case[par$params == "msw.cond5"]
# 
# pop.msw$cond_min[pop.msw$`Age.Group` == "13-17"]   <- par$base_min[par$params == "msw.cond0"]
# pop.msw$cond_min[pop.msw$`Age.Group` == "18-24"]   <- par$base_min[par$params == "msw.cond1"]
# pop.msw$cond_min[pop.msw$`Age.Group` == "13-24"]   <- par$base_min[par$params == "msw.cond1"]
# pop.msw$cond_min[pop.msw$`Age.Group` == "25-34"]   <- par$base_min[par$params == "msw.cond2"]
# pop.msw$cond_min[pop.msw$`Age.Group` == "35-44"]   <- par$base_min[par$params == "msw.cond3"]
# pop.msw$cond_min[pop.msw$`Age.Group` == "45-54"]   <- par$base_min[par$params == "msw.cond4"]
# pop.msw$cond_min[pop.msw$`Age.Group` == "55+"]     <- par$base_min[par$params == "msw.cond5"]
# 
# pop.msw$cond_max[pop.msw$`Age.Group` == "13-17"]   <- par$base_max[par$params == "msw.cond0"]
# pop.msw$cond_max[pop.msw$`Age.Group` == "18-24"]   <- par$base_max[par$params == "msw.cond1"]
# pop.msw$cond_max[pop.msw$`Age.Group` == "13-24"]   <- par$base_max[par$params == "msw.cond1"]
# pop.msw$cond_max[pop.msw$`Age.Group` == "25-34"]   <- par$base_max[par$params == "msw.cond2"]
# pop.msw$cond_max[pop.msw$`Age.Group` == "35-44"]   <- par$base_max[par$params == "msw.cond3"]
# pop.msw$cond_max[pop.msw$`Age.Group` == "45-54"]   <- par$base_max[par$params == "msw.cond4"]
# pop.msw$cond_max[pop.msw$`Age.Group` == "55+"]     <- par$base_max[par$params == "msw.cond5"]
# 
# pop.msw$contact[pop.msw$`Age.Group` == "13-17"]   <- par$base_case[par$params == "msw.contact0"]
# pop.msw$contact[pop.msw$`Age.Group` == "18-24"]   <- par$base_case[par$params == "msw.contact1"]
# pop.msw$contact[pop.msw$`Age.Group` == "13-24"]   <- par$base_case[par$params == "msw.contact1"]
# pop.msw$contact[pop.msw$`Age.Group` == "25-34"]   <- par$base_case[par$params == "msw.contact2"]
# pop.msw$contact[pop.msw$`Age.Group` == "35-44"]   <- par$base_case[par$params == "msw.contact3"]
# pop.msw$contact[pop.msw$`Age.Group` == "45-54"]   <- par$base_case[par$params == "msw.contact4"]
# pop.msw$contact[pop.msw$`Age.Group` == "55+"]     <- par$base_case[par$params == "msw.contact5"]
# 
# pop.msw$contact_min[pop.msw$`Age.Group` == "13-17"]   <- par$base_min[par$params == "msw.contact0"]
# pop.msw$contact_min[pop.msw$`Age.Group` == "18-24"]   <- par$base_min[par$params == "msw.contact1"]
# pop.msw$contact_min[pop.msw$`Age.Group` == "13-24"]   <- par$base_min[par$params == "msw.contact1"]
# pop.msw$contact_min[pop.msw$`Age.Group` == "25-34"]   <- par$base_min[par$params == "msw.contact2"]
# pop.msw$contact_min[pop.msw$`Age.Group` == "35-44"]   <- par$base_min[par$params == "msw.contact3"]
# pop.msw$contact_min[pop.msw$`Age.Group` == "45-54"]   <- par$base_min[par$params == "msw.contact4"]
# pop.msw$contact_min[pop.msw$`Age.Group` == "55+"]     <- par$base_min[par$params == "msw.contact5"]
# 
# pop.msw$contact_max[pop.msw$`Age.Group` == "13-17"]   <- par$base_max[par$params == "msw.contact0"]
# pop.msw$contact_max[pop.msw$`Age.Group` == "18-24"]   <- par$base_max[par$params == "msw.contact1"]
# pop.msw$contact_max[pop.msw$`Age.Group` == "13-24"]   <- par$base_max[par$params == "msw.contact1"]
# pop.msw$contact_max[pop.msw$`Age.Group` == "25-34"]   <- par$base_max[par$params == "msw.contact2"]
# pop.msw$contact_max[pop.msw$`Age.Group` == "35-44"]   <- par$base_max[par$params == "msw.contact3"]
# pop.msw$contact_max[pop.msw$`Age.Group` == "45-54"]   <- par$base_max[par$params == "msw.contact4"]
# pop.msw$contact_max[pop.msw$`Age.Group` == "55+"]     <- par$base_max[par$params == "msw.contact5"]

