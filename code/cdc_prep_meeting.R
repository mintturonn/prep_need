
#################
# R/E
results2 %>%
  mutate(race2 = recode(race,
                        "White" = "White",
                        "Hispanic" = "Hispanic",
                        "Black" = "Black",
                        "Asian" = "Other" ,
                        "Multiracial" = "Other",
                        "NHOPI" = "Other",
                        "AIAN" = "Other",
                        "All" = "pwid"))  %>%
  group_by(race2,  simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop)) %>%  
  ungroup() %>%
 # filter(race != "All") %>% # PWID do not have R/E strata
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  filter(Allocation == "sum_prep25.2") %>%
  group_by(race2,  Allocation) %>%
  summarize(
    mean_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    pop = mean(sum_pop)) %>%
  ungroup() %>%
  mutate(
    mean_prep_fmt = format_sigfig(mean_prep),
    lower_95_fmt = format_sigfig(lower_95),
    upper_95_fmt = format_sigfig(upper_95) ) %>%
  mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) -> test

test %>% filter(race2!="pwid") %>% summarize(sum=sum(mean_prep))

test %>% filter(Allocation == "100% to top 75%, 5% to bottom 25%") # %>% summarize(sum=sum(mean_prep))

 
df_trnsm1 %>% filter(Allocation == "100% to top 75%, 5% to bottom 25%" & trnsm!="pwid") %>% summarize(sum=sum(mean_prep))

df_race %>% filter(Allocation == "100% to top 75%, 5% to bottom 25%") %>% summarize(sum=sum(mean_prep))


#########################
# Age

## AGE

results2 %>%
  group_by(age, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop)) %>%
  ungroup() %>%
  filter(age=="13-24") -> ageres0

ageres0 %>%
  mutate( prep.young = pnat[as.numeric(simulation), "prep.young"],
          prep.1 = case_when(
            trnsm == "msm" ~ pnat[as.numeric(simulation), "prep.msm1"],
            trnsm == "msw" ~ pnat[as.numeric(simulation), "prep.msw1"],
            trnsm == "wsm" ~ pnat[as.numeric(simulation), "prep.wsm1"],  # Corrected column name
            TRUE ~ NA_real_)) %>%
  mutate( sum_prep100_adj = sum_prep100 - (7/12) * prep.1 * sum_pop, # prep need in 13-17 yos
          sum_prep10    = sum_prep10 * sum_prep100_adj/sum_prep100,
          sum_prep10.2  = sum_prep10.2 * sum_prep100_adj/sum_prep100,
          sum_prep25    = sum_prep25 * sum_prep100_adj/sum_prep100,
          sum_prep25.2  = sum_prep25.2 * sum_prep100_adj/sum_prep100,
          sum_pop     = (7/12) * sum_pop,
          age = "13-17",
          sum_prep100=sum_prep100_adj) %>%
  select(-sum_prep100_adj, -prep.young, -prep.1) -> age_13_17

ageres0 %>%
  filter(age=="13-24") %>%
  mutate( prep.young = pnat[as.numeric(simulation), "prep.young"],
          prep.1 = case_when(
            trnsm == "msm" ~ pnat[as.numeric(simulation), "prep.msm1"],
            trnsm == "msw" ~ pnat[as.numeric(simulation), "prep.msw1"],
            trnsm == "wsm" ~ pnat[as.numeric(simulation), "prep.wsm1"],  # Corrected column name
            TRUE ~ NA_real_)) %>%
  mutate( sum_prep100_adj = sum_prep100 - (5/12) * prep.young * sum_pop,
          sum_prep10      = sum_prep10  * sum_prep100_adj/sum_prep100,
          sum_prep10.2    = sum_prep10.2 * sum_prep100_adj/sum_prep100,
          sum_prep25      = sum_prep25  * sum_prep100_adj/sum_prep100,
          sum_prep25.2    = sum_prep25.2 * sum_prep100_adj/sum_prep100,
          age = "18-24",
          sum_prep100=sum_prep100_adj ) %>%
  select(-sum_prep100_adj, -prep.young, -prep.1) -> age_18_24

## Check
# bind_rows(age_13_17, age_18_24) %>% 
#   group_by( simulation, trnsm) %>%
#   summarize(
#     sum_prep100 = mean(sum_prep100),
#     sum_prep100_adj = sum(sum_prep100_adj),
#     sum_prep10 = sum(sum_prep10),
#     sum_prep25 = sum(sum_prep25),
#     sum_prep40 = sum(sum_prep40),
#     sum_prep001 = sum(sum_prep001),
#     sum_prep01 = sum(sum_prep01)) -> check
#  
#  sum(ageres0$sum_prep100-check$sum_prep100)
#  sum(ageres0$sum_prep10-check$sum_prep10)
#  sum(ageres0$sum_prep25-check$sum_prep25)
#  sum(ageres0$sum_prep40-check$sum_prep40)
#  sum(ageres0$sum_prep001-check$sum_prep001)
#  sum(ageres0$sum_prep01-check$sum_prep01)

results2 %>%
  group_by(age, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop)) %>%
  ungroup() %>%
  filter(age!="13-24") %>%
  bind_rows(age_13_17, age_18_24) %>% 
  group_by(age, simulation) %>%
  summarize(sum_prep100= sum(sum_prep100, na.rm=T),
            sum_prep10 = sum(sum_prep10, na.rm=T),
            sum_prep10.2 = sum(sum_prep10.2, na.rm=T),
            sum_prep25 = sum(sum_prep25, na.rm=T),
            sum_prep25.2 = sum(sum_prep25.2, na.rm=T),
            sum_pop = sum(sum_pop)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(age, Allocation) %>%
  summarize(
    mean_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    pop = mean(sum_pop),
    .groups = 'drop' ) %>%
  filter(age != "All") %>% # PWID do not have age strata
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indications)",
                             "sum_prep10" = "100% to top 90% HIV infections",
                             "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                             "sum_prep25" = "100% to top 75% HIV infections",
                             "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(age, Allocation, mean_prep, lower_95, upper_95, pop) %>%
  mutate(
    mean_prep_fmt = format_sigfig(mean_prep),
    lower_95_fmt = format_sigfig(lower_95),
    upper_95_fmt = format_sigfig(upper_95) ) %>%
  mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) -> df_age_new

df_age_new %>% filter(Allocation == "100% to top 75%, 5% to bottom 25%") # %>% summarize(sum=sum(mean_prep))

#####
# State

results2 %>%
  group_by(state,  simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop)) %>%  
  ungroup() %>%
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(state, Allocation) %>%
  summarize(
    mean_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    # pop = mean(sum_pop),
  ) %>%
  ungroup() %>%
  bind_rows(cdc_st_sum) %>%
  bind_rows(cb_st_sum) %>%
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indications)",
                             "sum_prep10" = "100% to top 90% HIV infections",
                             "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                             "sum_prep25" = "100% to top 75% HIV infections",
                             "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(state, Allocation, mean_prep, lower_95, upper_95) -> df_state  # trnsm, 

df_state  %>%
  mutate(
    mean_prep_fmt = format_sigfig(mean_prep),
    lower_95_fmt = format_sigfig(lower_95),
    upper_95_fmt = format_sigfig(upper_95) ) %>%
 # mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) %>%
  filter(Allocation == "100% to top 75%, 5% to bottom 25%") %>%
  arrange(state) %>% # trnsm, `Transmission Risk` = trnsm, 
  select(State=state, Allocation, mean_prep, mean_prep_fmt)   -> df_state_tab_new

