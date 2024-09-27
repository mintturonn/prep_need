
# incidence data from CDC Atlas Plus

# cdc hiv data state level by mode 
read_csv("~/prep_need/incidence_data/AtlasPlusTableData_incid_state.csv", skip=10) %>%
  filter(Year=="2022" & FIPS < 60) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases)),
         cases_ll = as.numeric(gsub(",", "", `Cases LCI`)),
         cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(population = as.numeric(gsub(",", "", Population))) %>%
  mutate(rate = as.numeric(gsub(",", "", `Rate per 100000`))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  filter(!is.na(rate)) %>%
  dplyr::select(state, cases, cases_ll, cases_ul, population) -> df_incid_state

# df_incid_state0 %>%
#   filter(state != "District of Columbia") -> df_incid_state
# 
# df_incid_state0 %>%
#   filter(state == "District of Columbia") -> df_incid_dc_state


read_csv("~/prep_need/incidence_data/AtlasPlusTableData_incid_national.csv", skip=10) %>%
  filter(Year=="2022") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases)),
         cases_ll = as.numeric(gsub(",", "", `Cases LCI`)),
         cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(population = as.numeric(gsub(",", "", Population))) %>%
  dplyr::select(Year, cases, cases_ll, cases_ul, population) %>%
  mutate(cases_subs    = cases - sum(df_incid_state$cases),
         cases_subs_ll = ifelse((cases_ll - sum(df_incid_state$cases <0)), 0, (cases_ll - sum(df_incid_state$cases <0))),
         cases_subs_ul = cases_ul - sum(df_incid_state$cases)) -> df_incid_nat

# cdc hiv data natioanl race/eth 
read_csv("~/prep_need/incidence_data/AtlasPlusTableData_incid_raceth.csv", skip=10) %>%
  filter(Year=="2022") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases)),
         cases_ll = as.numeric(gsub(",", "", `Cases LCI`)),
         cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(population = as.numeric(gsub(",", "", Population))) %>%
  mutate(rate = as.numeric(gsub(",", "", `Rate per 100000`))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  filter(!is.na(cases)) %>%
  mutate(race = case_when(
    `Race/Ethnicity` == "American Indian/Alaska Native" ~ "AIAN",
    `Race/Ethnicity` == "Asian" ~ "Asian",
    `Race/Ethnicity` == "Black/African American" ~ "Black",
    `Race/Ethnicity` == "Hispanic/Latino" ~ "Hispanic",
    `Race/Ethnicity` == "White" ~ "White",
    TRUE ~ `Race/Ethnicity` )) %>%
  dplyr::select(race, cases, cases_ll, cases_ul, population) -> df_incid_raceth

# cdc hiv data national age
read_csv("~/prep_need/incidence_data/AtlasPlusTableData_incid_age.csv", skip=10) %>%
  filter(Year=="2022") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases)),
         cases_ll = as.numeric(gsub(",", "", `Cases LCI`)),
         cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(population = as.numeric(gsub(",", "", Population))) %>%
  mutate(rate = as.numeric(gsub(",", "", `Rate per 100000`))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  rename(age = `Age Group`) %>%
  filter(!is.na(cases)) %>%
  mutate(age = case_when(
    `age` == "65+" ~ "55+",
    `age` == "55-64" ~ "55+",
    TRUE ~ `age` )) %>%
  group_by(age) %>%
  summarize(cases = sum(cases),
            cases_ll = sum(cases_ll),
            cases_ul = sum(cases_ul),
            population = sum(population)) %>%
  ungroup()  -> df_incid_age

# cdc hiv data national age
read_csv("~/prep_need/incidence_data/AtlasPlusTableData_incid_transmcat.csv", skip=10) %>%
  filter(Year=="2022") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases)),
         cases_ll = as.numeric(gsub(",", "", `Cases LCI`)),
         cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(population = as.numeric(gsub(",", "", Population))) %>%
  mutate(rate = as.numeric(gsub(",", "", `Rate per 100000`))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  rename(trnsm2 = `Transmission Category`) %>%
  filter(!is.na(cases)) %>%
  mutate(trnsm = case_when(
    `trnsm2` == "Male-to-male sexual contact and injection drug use" ~ "msm",
    `trnsm2` == "Male-to-male sexual contact" ~ "msm",
    `trnsm2` == "Heterosexual contact" ~ "phet",
    `trnsm2` == "Injection drug use" ~ "pwid",
    FALSE ~ `trnsm2` )) %>%
  group_by(trnsm) %>%
  summarize(cases = sum(cases),
            cases_ll = sum(cases_ll),
            cases_ul = sum(cases_ul)) %>%
  ungroup()  %>%
  filter(trnsm!= "phet") -> df_incid_transm1

read_csv("~/prep_need/incidence_data/AtlasPlusTableData_incid_transmcat_phet.csv", skip=10) %>%
  filter(Year=="2022") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases)),
         cases_ll = as.numeric(gsub(",", "", `Cases LCI`)),
         cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(population = as.numeric(gsub(",", "", Population))) %>%
  mutate(rate = as.numeric(gsub(",", "", `Rate per 100000`))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  filter(!is.na(cases)) %>%
  mutate(trnsm = case_when(
    `Sex` == "Male" ~ "msw",
    `Sex` == "Female" ~ "wsm",
    FALSE ~ `Sex` )) %>%
  group_by(trnsm) %>%
  summarize(cases = sum(cases),
            cases_ll = sum(cases_ll),
            cases_ul = sum(cases_ul)) %>%
  ungroup() %>%
  rbind(df_incid_transm1) -> df_incid_trnsm

# cdc hiv data national msm X age [does not include msm & pwid]
read_csv("~/prep_need/incidence_data/AtlasPlusTableData_incid_msm_age.csv", skip=10) %>%
  filter(Year=="2022") %>%
  filter(`Transmission Category` == "Male-to-male sexual contact") %>% # |  `Transmission Category` == "Male-to-male sexual contact and injection drug use") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases)),
         cases_ll = as.numeric(gsub(",", "", `Cases LCI`)),
         cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(population = as.numeric(gsub(",", "", Population))) %>%
  mutate(rate = as.numeric(gsub(",", "", `Rate per 100000`))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  rename(age = `Age Group`) %>%
  filter(!is.na(cases)) %>%
  mutate(age = case_when(
    `age` == "65+" ~ "55+",
    `age` == "55-64" ~ "55+",
    TRUE ~ `age` )) %>%
  group_by(age) %>%
  summarize(cases = sum(cases),
            cases_ll = sum(cases_ll),
            cases_ul = sum(cases_ul)) %>%
  ungroup() -> df_incid_msm_age

# cdc hiv in PWID by sex
read_csv("~/prep_need/incidence_data/AtlasPlusTableData_incid_pwid.csv", skip=10) %>%
  filter(Year=="2022") %>%
  rename(sex = Sex) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases)),
         cases_ll = as.numeric(gsub(",", "", `Cases LCI`)),
         cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(population = as.numeric(gsub(",", "", Population))) %>%
  dplyr::select(Year, cases, cases_ll, cases_ul, sex) -> df_incid_pwid

