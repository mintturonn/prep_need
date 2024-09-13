
library(geofacet)
library(readr)
library(tidyverse)

read_csv(here("data/SVI_2020_US_COUNTY.csv")) %>%
  select(ST, STATE, ST_ABBR, COUNTY, FIPS, LOCATION, AREA_SQMI, E_TOTPOP, M_TOTPOP, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES) %>%
  group_by(STATE) %>%
  mutate(svi_sum = sum(RPL_THEMES),
         svi_prop = RPL_THEMES/sum(RPL_THEMES),
         cnty_pop_prop = E_TOTPOP/sum(E_TOTPOP),
         svi_check = sum(svi_prop)) %>%
  ungroup() %>%
  select(STATE, COUNTY, FIPS, E_TOTPOP, svi_prop, cnty_pop_prop)-> svi

# msm,pwid, het-f, het-m


# old
# read_csv(here("data/AtlasPlusTableData_HIVincidence_state.csv"), skip=10) %>%
#   filter(FIPS < 59) %>%
#   rename(STATE = Geography,
#          FIPS_ST = FIPS) %>%
#   mutate(rate = as.numeric(ifelse(`Rate per 100000`=="Data suppressed", NA, `Rate per 100000`))) %>%
#   mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, Year))) %>%
#   mutate(pop = as.numeric(ifelse(Population=="Data suppressed", NA, gsub(",", "", Population)))) %>%
#   filter(year == 2021) %>% 
#   mutate(rate2 = ifelse(is.na(rate), min(rate, na.rm=T)/2, rate)) %>%
#   mutate(prop_rate = rate2 / sum(rate2) ) %>%
#   mutate(pop_rate = pop / sum(pop) ) %>%
#   mutate(prep_nat = 3210000) %>%
#   mutate(alloc1 = 0.3) %>%
#   mutate(prep_st_pc = alloc1*prep_nat*pop_rate) %>%
#   mutate(prep_st_need = alloc1*prep_nat*prop_rate) %>%
#   mutate(alloc2 = 0.7) %>%
#   mutate(prep_st_pc2 = alloc2*prep_nat*pop_rate) %>%
#   mutate(prep_st_need2 = (1-alloc2)*prep_nat*prop_rate) %>%
#   select(STATE, FIPS_ST , prep_st_pc, prep_st_need,  prep_st_pc2, prep_st_need2) -> hiv_i_st
# 
# 
# 
# hiv_i_st %>%
#   left_join(svi, by="STATE") %>%
#   mutate(prep_ct_pc = prep_st_pc*cnty_pop_prop,
#          prep_ct_need = prep_st_need*svi_prop,
#          prep_ct_pc2 = prep_st_pc2*cnty_pop_prop,
#          prep_ct_need2 = prep_st_need2*svi_prop,
#          prp_need_tot_ct = prep_ct_pc+prep_ct_need,
#          prp_need_tot_ct2 = prep_ct_pc2+prep_ct_need2 ) -> prep_need
# 
# prep_need %>%
#   group_by(STATE) %>%
#   summarize(prp_need_tot_st = sum(prp_need_tot_ct),
#             prp_need_tot_st2 = sum(prp_need_tot_ct2)) -> prep_st

read_csv(here("data/AtlasPlusTableData_state_hivdiag_female.csv"), skip=8) %>%
  filter(FIPS < 59) %>%
  rename(STATE = Geography,
         FIPS_ST = FIPS) %>%
  #mutate(rate = as.numeric(ifelse(`Rate per 100000`=="Data suppressed", NA, `Rate per 100000`))) %>%
  mutate(cases = as.numeric(ifelse(Cases=="Data not available", NA, Cases))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, Year))) %>%
  rename(trans_cat = `Transmission Category`) %>%
  mutate(trans_cat = ifelse(trans_cat=="Heterosexual contact", "WSM-Contact", trans_cat)) %>%
  group_by(STATE, trans_cat, year, Sex) %>%
  summarize(cases = sum(cases, na.rm=T)) %>%
  ungroup() -> state_diag_w

read_csv(here("data/AtlasPlusTableData_state_hivdiag_male.csv"), skip=8) %>%
  filter(FIPS < 59) %>%
  rename(STATE = Geography,
         FIPS_ST = FIPS) %>%
  #mutate(rate = as.numeric(ifelse(`Rate per 100000`=="Data suppressed", NA, `Rate per 100000`))) %>%
  mutate(cases = as.numeric(ifelse(Cases=="Data not available", NA, Cases))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, Year))) %>%
  # here group MSM/PWID under MSM risk group
  mutate(trans_cat = ifelse(`Transmission Category`=="Male-to-male sexual contact and injection drug use", "Male-to-male sexual contact", `Transmission Category`)) %>%
  mutate(trans_cat = ifelse(trans_cat=="Heterosexual contact", "MSW-Contact", trans_cat)) %>%
  #filter(year == 2021) %>% 
  group_by(STATE, trans_cat, year, Sex) %>%
  summarize(cases = sum(cases, na.rm=T))  %>%
  ungroup() -> state_diag_m

rbind(state_diag_w, state_diag_m)  %>%
  group_by(trans_cat, year) %>%
  summarize(cases = sum(cases, na.rm=T)) -> nat_diag_trans_cat


# this shows the proporttional distribution of cases by state and transmission risk group
state_diag_w %>%
  full_join(state_diag_m, by=c("year", "STATE","trans_cat")) %>%
  left_join(nat_diag_trans_cat, by=c("year", "trans_cat")) %>%
  mutate(cases_f = ifelse(is.na(cases.x), 0, cases.x),
         cases_m = ifelse(is.na(cases.y), 0, cases.y)) %>%
  mutate(diag_prop_nat = (cases_f+cases_m)/cases)  %>%
  filter(year==2021)  -> diag_distr

# distribution to stae level
diag_distr %>%
  ggplot(aes(x=trans_cat, y=diag_prop_nat, fill=trans_cat)) +
  geom_col() +
  facet_geo(~STATE) +   
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) 



diag_distr %>%
  mutate(prep_nat = ifelse(trans_cat=="Male-to-male sexual contact", 1654666, 
                           ifelse(trans_cat=="Injection drug use", 196050, 
                                  ifelse(trans_cat=="WSM-Contact", 399943,
                                         ifelse(trans_cat=="MSW-Contact", 156840, NA))))) %>%
  mutate(prep_st_need = prep_nat*diag_prop_nat) -> prep_need_st


prep_need_st %>%
  select(STATE, trans_cat, prep_st_need) %>%
  pivot_wider(names_from = "trans_cat", values_from = "prep_st_need") %>%
  select(-Other) %>%
  mutate(prep_need_tot =  `Injection drug use`+ `WSM-Contact`+ `MSW-Contact`+ `Male-to-male sexual contact`) -> prep_st2


read_csv(here("data/AtlasPlusTableData_PrEP_st.csv"), skip=8) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, Year))) %>%
  mutate(pop = as.numeric(ifelse(Population=="Data suppressed", NA, gsub(",", "", Population)))) %>%
  filter(year == 2021 & FIPS<59) %>%
  rename(STATE = Geography) %>%
  select(STATE, pop) %>%
  left_join(prep_st2, by="STATE") -> test

test %>%
  ggplot()+
  geom_point(aes(y=reorder(STATE, prep_need_tot), x=pop), colour="#0072B2", size = 3, alpha = 4/10) +
  geom_point(aes(y=reorder(STATE, prep_need_tot), x=prep_need_tot), colour="maroon", size = 3, alpha = 2/10) +
  ylab("") + xlab("number needing PrEP") +
  theme_bw()

test %>%
  ggplot()+
  geom_point(aes(y=reorder(STATE, prp_need_tot_st), x=pop), colour="#0072B2", size = 3, alpha = 4/10) +
  geom_point(aes(y=reorder(STATE, prp_need_tot_st), x=prp_need_tot_st), colour="maroon", size = 3, alpha = 2/10) +
  geom_point(aes(y=reorder(STATE, prp_need_tot_st), x=prp_need_tot_st2), colour="maroon", size = 3, alpha = 8/10) +
  ylab("") + xlab("number needing PrEP") +
  theme_bw()




