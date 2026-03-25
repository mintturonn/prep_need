

pop.m0 %>%
  mutate(pop = `18-24`+ `25-34`+ `35-44`+ `45-54`+ `55+` ) %>%
  select(state, FIPS, pop) %>%
  left_join(msmpr, by=c("state")) %>%
  mutate(msm_pop = pop*prev_extract,
         msm = msm_pop *0.247) -> pop.msm.cdc


read.csv("~/prep_need/data/AtlasPlusTableData_hivdiag_state_bothsexes.csv", skip=8, header = TRUE)  %>%
  filter(Year=="2022") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(mode = ifelse(Transmission.Category == "Heterosexual contact", "di_het_all",
                       ifelse(Transmission.Category == "Injection drug use", "di_pwid_all", "di_msm_all") )) %>%
  group_by(Geography, FIPS, mode) %>%
  summarize(di_num = sum(cases)) %>%
  ungroup() %>%
  pivot_wider(names_from = "mode", values_from = di_num) %>%
  mutate(di_state = di_het_all + di_msm_all + di_pwid_all) -> hivd_all # %>%
  # mutate(dipr_msm = di_msm/di_state) %>%
  # mutate(dipr_pwid = di_pwid/di_state) %>%
  # mutate(dipr_het = di_het/di_state) %>%
  # mutate(check_pr = dipr_msm + dipr_pwid + dipr_het) -> hivd

read.csv("~/prep_need/data/AtlasPlusTableData_hivdiag_state_women.csv", skip=8, header = TRUE)  %>%
  filter(Year=="2022") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(mode = ifelse(Transmission.Category == "Heterosexual contact", "di_het_f",
                       ifelse(Transmission.Category == "Injection drug use", "di_pwid_f", NA) )) %>%
  group_by(Geography, FIPS, mode) %>%
  summarize(di_num = sum(cases)) %>%
  ungroup() %>%
  pivot_wider(names_from = "mode", values_from = di_num) -> hivd_f #%>%
  # mutate(di_state = di_het +  di_pwid) %>%
  # mutate(dipr_pwid = di_pwid/di_state) %>%
  # mutate(dipr_het = di_het/di_state) %>%
  # mutate(check_pr =  dipr_pwid + dipr_het) -> hivd_f

read.csv("~/prep_need/data/AtlasPlusTableData_hivdiag_state_men.csv", skip=8, header = TRUE)  %>%
  filter(Year=="2022") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(mode = ifelse(Transmission.Category == "Heterosexual contact", "di_het_m",
                       ifelse(Transmission.Category == "Injection drug use", "di_pwid_m", "di_msm_m") )) %>%
  group_by(Geography, FIPS, mode) %>%
  summarize(di_num = sum(cases)) %>%
  ungroup() %>%
  pivot_wider(names_from = "mode", values_from = di_num) -> hivd_m # %>%
  # mutate(di_state = di_het +  di_pwid) %>%
  # mutate(dipr_pwid = di_pwid/di_state) %>%
  # mutate(dipr_het = di_het/di_state) %>%
  # mutate(check_pr =  dipr_pwid + dipr_het) -> hivd_m

hivd_all %>%
  left_join(hivd_f, by = c("Geography", "FIPS")) %>%
  left_join(hivd_m, by = c("Geography", "FIPS")) %>%
  mutate(di_het_sum = di_het_f+di_het_m,
         di_pwid_sum = di_pwid_f+di_pwid_m,
         di_msm_diff = di_msm_all-di_msm_m) %>%
  filter(FIPS < 60) %>%
  mutate(di_het_f = case_when(
         Geography == "New Hampshire" ~ 
            di_het_all[Geography == "New Hampshire"] * 
            di_het_f[Geography == "Massachusetts" ] / 
            di_het_all[Geography == "Massachusetts"],
         TRUE ~ di_het_f ),
         di_het_m = case_when(
           Geography == "New Hampshire" ~ 
            di_het_all[Geography == "New Hampshire"] * 
            di_het_m[Geography == "Massachusetts" ] / 
           di_het_all[Geography == "Massachusetts"],
           TRUE ~ di_het_m ))  %>%
  rename(state=Geography) %>%
  # select(state, FIPS, pwid_all, msm_all, di_het_f, di_het_m) %>%
  mutate(di_state_all = di_pwid_all+ di_msm_all+ di_het_f+ di_het_m,
         dipr_msm = di_msm_all/di_state_all,
         dipr_pwid = di_pwid_all/di_state_all,
         dipr_het_f = di_het_f/di_state_all, 
         dipr_het_m = di_het_m/di_state_all,
         check =  dipr_msm+dipr_pwid+dipr_het_f+dipr_het_m,
         dipr_msm_st = di_msm_all/sum(di_msm_all),
         dipr_pwid_st = di_pwid_all/sum(di_pwid_all),
         dipr_het_f_st = di_het_f/sum(di_het_f),
         dipr_het_m_st = di_het_m/sum(di_het_m)) %>%
  select(state, FIPS, dipr_msm, dipr_pwid, dipr_het_f, dipr_het_m, check, di_state_all, dipr_msm_st, dipr_pwid_st, dipr_het_f_st, dipr_het_m_st) -> hivd_df


hivd_df %>%
  left_join(pop.msm.cdc, by=c("state", "FIPS")) %>%
  mutate(pwid = msm * dipr_pwid/ dipr_msm ,
         wsm = msm * dipr_het_f/ dipr_msm ,
         msw = msm * dipr_het_m/ dipr_msm ) -> cdc_prep_need
  

  
  



