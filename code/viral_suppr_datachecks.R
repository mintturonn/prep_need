

# HIV viral suppression - phet with age
read_csv(here("data/AtlasPlusTableData_vs_state_sex.csv"), skip=9) %>%
  filter(Year == "2022") %>%
  mutate(vsupp_cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(vsupp_perc = as.numeric(Percent)/100) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  select(state, vsupp_cases, vsupp_perc, Indicator, Sex, `Age Group`, FIPS) %>%
  filter(Indicator == "HIV viral suppression")  %>%
  mutate( agecat = ifelse(`Age Group` == "13-24" | `Age Group` =="25-34" | `Age Group` == "35-44", "younger", "older")) %>%
  rename(Age.Group = `Age Group`) %>%
  mutate(vsupp_perc = ifelse(is.na(vsupp_perc), median(vsupp_perc, na.rm=T), vsupp_perc))-> hiv_het_vsuppr

# this is awkward
hiv_het_vsuppr_13_17 <- hiv_het_vsuppr_18_24 <- hiv_het_vsuppr[hiv_het_vsuppr$Age.Group=="13-24",] 
hiv_het_vsuppr_13_17$Age.Group <- "13-17"
hiv_het_vsuppr_18_24$Age.Group <- "18-24"
hiv_het_vsuppr <- rbind(hiv_het_vsuppr,hiv_het_vsuppr_13_17, hiv_het_vsuppr_18_24)

hiv_het_vsuppr %>%
  filter(Age.Group!= "13-17" & Age.Group!="18-24" & FIPS<60) %>%
  ggplot(aes(x=Age.Group, y=vsupp_perc, color=Sex)) + geom_point() + facet_geo(~ state)

# HIV viral suppression -pwid
read_csv(here("data/AtlasPlusTableData_vs_state_pwid.csv"), skip=9) %>%
  filter(Year=="2022") %>%
  mutate(vsupp_cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(vsupp_perc = as.numeric(Percent)/100) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  select(state, vsupp_cases, vsupp_perc, Indicator, Sex, FIPS) %>%
  mutate(vsupp_perc = ifelse(is.na(vsupp_perc), median(vsupp_perc, na.rm=T), vsupp_perc)) -> hiv_pwid_vsuppr

hiv_pwid_vsuppr %>%
  filter( FIPS<60) %>%
  ggplot(aes(x=Age.Group, y=vsupp_perc, color=Sex)) + geom_point() + facet_geo(~ state)

# hiv viral ssuppression trnsm and sex
read_csv(here("data/AtlasPlusTableData_vs_state_female_trnsm.csv"), skip=9) %>%
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
  mutate( cases = case_when(
      vsupp_perc  == is.na(vsupp_perc) & trans_cat == "pwid" ~ median(vsupp_perc[trans_cat == "pwid"], na.rm=T),
      vsupp_perc  == is.na(vsupp_perc) & trans_cat == "wsm"  ~ median(vsupp_perc[trans_cat == "wsm"], na.rm=T) ,
    TRUE ~ vsupp_perc )) %>%
  select(state,  FIPS, Sex, trans_cat,vsupp_perc, cases)  -> hiv_f_trnsm_vsuppr 

  ggplot(aes(x=trans_cat, y=vsupp_perc, color=Sex)) + geom_point() + facet_geo(~ state)

read_csv(here("data/AtlasPlusTableData_vs_state_male_trnsm.csv"), skip=9) %>%
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
  mutate( cases = case_when(
    vsupp_perc  == is.na(vsupp_perc) & trans_cat == "pwid" ~ median(vsupp_perc[trans_cat == "pwid"], na.rm=T),
    vsupp_perc  == is.na(vsupp_perc) & trans_cat == "wsm" ~ median(vsupp_perc[trans_cat == "wsm"], na.rm=T),
    FALSE ~ vsupp_perc )) %>%
  select(state,  FIPS, Sex, trans_cat, vsupp_perc, cases)  -> hiv_m_trnsm_vsuppr # %>%
  
  hiv_m_trnsm_vsuppr %>%
  filter( FIPS<60) %>%
  ggplot(aes(x=state, y=vsupp_perc, color=state)) + geom_point() + facet_wrap(~trans_cat)


