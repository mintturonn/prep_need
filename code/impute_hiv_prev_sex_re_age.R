##
# library(AICcmodavg)
library(ggplot2)
library(here)
library(lme4)
library(MASS)
library(pscl)
library(readxl)
library(reshape2)
library(tidycensus)
library(tidyverse)

source(here('code/acs_fun.R'))


#  rm(list = ls())
# .rs.restartR()

pop_re_2015  <- pop_re_fun(2015, "2008-2014") 

pivot_longer(pop_re_2015$f, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "Age Group", values_to = "acs_pop") %>% 
  bind_rows(., pivot_longer(pop_re_2015$m, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "Age Group", values_to = "acs_pop")) %>%
  filter(state == "Puerto Rico") %>%
  rename(`Race/Ethnicity` = `Race.Ethnicity`) %>%
  filter(`Race/Ethnicity` != "White_any") %>%
  group_by(Sex, `Race/Ethnicity`, state) %>%
  summarise(est_pop = sum(acs_pop)) -> pop_sr_long

pivot_longer(pop_re_2015$f, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "age", values_to = "acs_pop") %>% 
  bind_rows(., pivot_longer(pop_re_2015$m, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "age", values_to = "acs_pop")) %>%
  rename(`Race/Ethnicity` = `Race.Ethnicity`)  %>%
  filter(`Race/Ethnicity` != "White_any") %>%
  select(state, FIPS, Sex, `Race/Ethnicity`, age, acs_pop) -> pop_sar_long

pop_sar_long %>%
  group_by(Sex, state, FIPS) %>%
  summarise(acs_pop = sum(acs_pop)) -> pop_ss_long
# pop_re_long %>%
#   filter(Race.Ethnicity != "White_any") %>%
#   group_by(Sex, `Age Group`) %>%
#   summarise(est_pop = sum(acs_pop)) -> test

# state level prevalence by sex
read_excel(here("data/state_s_prev_2008_2020.xlsx"), skip = 10) %>%
  mutate(s_case = as.numeric(ifelse(Cases=="Data suppressed", NA, gsub(",", "", Cases)))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)" | is.na(Year), 2020, Year))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  dplyr::select( Sex, FIPS, state, year, s_case)   -> s_df

# state level diagnosis by sex
read_excel(here("data/state_s_diag_2008_2020.xlsx"), skip = 8) %>%
  mutate(s_d_case = as.numeric(ifelse(Cases=="Data suppressed", NA, gsub(",", "", Cases)))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)" | is.na(Year), 2020, Year))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  dplyr::select( Sex, FIPS, state, year, s_d_case)   -> s_d_df

# state level proportion of prevalence in MSM (of total prevalence)
read_excel(here("data/state_transcat_prev_2008_2020.xlsx"), skip = 10) %>%
  mutate(t_case = as.numeric(ifelse(Cases=="Data suppressed", NA, gsub(",", "", Cases)))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)" | is.na(Year), 2020, Year))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  rename(transcat = `Transmission Category`) %>%
  mutate(transm = ifelse(transcat=="Male-to-male sexual contact" | transcat=="Male-to-male sexual contact and injection drug use", "msm", 
                      ifelse(transcat== "Heterosexual contact", "het", 
                             ifelse(transcat =="Injection drug use", "idu", "oth")))) %>%
  group_by(year, FIPS, state, transm) %>%
  summarize(transm_case = sum(t_case)) %>%
  ungroup() %>%
  pivot_wider(names_from ="transm", values_from = "transm_case") %>%
  mutate(msm_prop = msm / (msm+het+oth+idu),
         het_prop = het / (msm+het+oth+idu)) %>%
  dplyr::select(FIPS, state, year, msm, het, idu, oth, msm_prop, het_prop)   -> t_df

t_df %>%
  pivot_longer(cols = c("msm", "het", "idu", "oth"), names_to = "transm", values_to = "trans_case") -> t_df2

# state level prevalence by sex, age, race/ethnicity and transmission group - women
read_excel(here("data/state_f_tr_re_age_prev_2008_2020.xlsx"), skip = 10) %>%
  mutate(tsar_case = as.numeric(ifelse(Cases=="Data suppressed", NA, gsub(",", "", Cases)))) %>%
  mutate(sar_pop0 = as.numeric(ifelse(Population=="Data suppressed", NA, gsub(",", "", Population)))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)" | is.na(Year), 2020, Year))) %>%
  mutate(age = ifelse(`Age Group`=="45-54" | `Age Group`=="35-44" | `Age Group`=="25-34" | `Age Group`=="13-24", `Age Group`, "55+")) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  rename(transcat = `Transmission Category`) %>%
  group_by(age, transcat, year, FIPS, state, Sex, `Race/Ethnicity`) %>%
  summarize(tsar_case = sum(tsar_case)) %>%
  ungroup() %>%
  dplyr::select( age, Sex, FIPS, state, year, tsar_case, `Race/Ethnicity`, transcat)   -> tsar_f_df

# state level prevalence by sex, age, race/ethnicity and transmission group - men
read_excel(here("data/state_m_tr_re_age_prev_2008_2020.xlsx"), skip = 10) %>%
  mutate(tsar_case = as.numeric(ifelse(Cases=="Data suppressed", NA, gsub(",", "", Cases)))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)" | is.na(Year), 2020, Year))) %>%
  mutate(age = ifelse(`Age Group`=="45-54" | `Age Group`=="35-44" | `Age Group`=="25-34" | `Age Group`=="13-24", `Age Group`, "55+")) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  rename(transcat = `Transmission Category`) %>%
  group_by(age, transcat, year, FIPS, state, Sex, `Race/Ethnicity`) %>%
  summarize(tsar_case = sum(tsar_case)) %>%
  ungroup() %>%
  dplyr::select( age, Sex, FIPS, state, year, tsar_case, `Race/Ethnicity`, transcat)   -> tsar_m_df

################

rbind(tsar_m_df,tsar_f_df)  %>%
  mutate(transm = ifelse(transcat=="Male-to-male sexual contact" | transcat=="Male-to-male sexual contact and injection drug use", "msm", 
                         ifelse(transcat== "Heterosexual contact", "het", 
                                ifelse(transcat =="Injection drug use", "idu", "oth")))) %>%
  left_join(s_df , by = c("Sex", "FIPS", "state", "year")) %>% 
  left_join(s_d_df, by = c("Sex", "FIPS", "state", "year")) %>%
  left_join(t_df2, by = c("FIPS", "state", "year", "transm")) %>%
  left_join(pop_sar_long, by= c("Sex", "age", "FIPS", "state", "Race/Ethnicity")) %>%
  filter(state !="American Samoa" & state != "Guam" & state!="Northern Mariana Islands" & state!= "US Virgin Islands" & state != "Palau")  %>%
  mutate(acs_pop2 = ifelse(acs_pop==0, 1, acs_pop) ) %>%
  mutate(acs_transm_pop = ifelse(transcat=="Male-to-male sexual contact" | transcat=="Male-to-male sexual contact and injection drug use", acs_pop2*0.047, 
                                 ifelse(transcat== "Heterosexual contact", acs_pop2, acs_pop2*0.02))) %>%
  filter(transcat!="Other") -> hiv_df

################

hiv_df$geography[hiv_df$state == "Maine" | hiv_df$state == "Massachusetts" | hiv_df$state == "Connecticut" | hiv_df$state == "New Hampshire" |
                    hiv_df$state == "Rhode Island" | hiv_df$state == "Vermont"] <- "Region 1"
hiv_df$geography[hiv_df$state == "New Jersey" | hiv_df$state == "New York" | hiv_df$state == "Puerto Rico"] <- "Region 2"
hiv_df$geography[hiv_df$state == "Delaware" | hiv_df$state == "District of Columbia" | hiv_df$state == "Maryland" | hiv_df$state == "Pennsylvania" |
                    hiv_df$state == "Virginia" | hiv_df$state == "West Virginia"] <- "Region 3"
hiv_df$geography[hiv_df$state == "Alabama" | hiv_df$state == "Florida" | hiv_df$state == "Georgia" | hiv_df$state == "Kentucky" |
                    hiv_df$state == "Mississippi" | hiv_df$state == "North Carolina" | hiv_df$state == "South Carolina" | hiv_df$state == "Tennessee"] <- "Region 4"
hiv_df$geography[hiv_df$state == "Illinois" | hiv_df$state == "Indiana" | hiv_df$state == "Michigan" | hiv_df$state == "Minnesota" |
                    hiv_df$state == "Ohio" | hiv_df$state == "Wisconsin"] <- "Region 5"
hiv_df$geography[hiv_df$state == "Arkansas" | hiv_df$state == "Louisiana" | hiv_df$state == "New Mexico" | hiv_df$state == "Oklahoma" |
                    hiv_df$state == "Texas"] <- "Region 6"
hiv_df$geography[hiv_df$state == "Iowa" | hiv_df$state == "Kansas" | hiv_df$state == "Missouri" | hiv_df$state == "Nebraska"] <- "Region 7"
hiv_df$geography[hiv_df$state == "Colorado" | hiv_df$state == "Montana" | hiv_df$state == "North Dakota" | hiv_df$state == "South Dakota" |
                    hiv_df$state == "Utah" | hiv_df$state == "Wyoming"] <- "Region 8"
hiv_df$geography[hiv_df$state == "Arizona" | hiv_df$state == "California" | hiv_df$state == "Hawaii" | hiv_df$state == "Nevada"] <- "Region 9"
hiv_df$geography[hiv_df$state == "Alaska" | hiv_df$state == "Idaho" | hiv_df$state == "Oregon" | hiv_df$state == "Washington"] <- "Region 10"

hiv_df$geography2[hiv_df$state == "Maine" | hiv_df$state == "Massachusetts" | hiv_df$state == "Connecticut" | hiv_df$state == "New Hampshire" |
                   hiv_df$state == "Rhode Island" | hiv_df$state == "Vermont"] <- "Region 1"
hiv_df$geography2[hiv_df$state == "New Jersey" | hiv_df$state == "New York" | hiv_df$state == "Puerto Rico"] <- "Region 2"
hiv_df$geography2[hiv_df$state == "Delaware" | hiv_df$state == "District of Columbia" | hiv_df$state == "Maryland" | hiv_df$state == "Pennsylvania" |
                   hiv_df$state == "Virginia" | hiv_df$state == "West Virginia"] <- "Region 3"
hiv_df$geography2[hiv_df$state == "Alabama" | hiv_df$state == "Georgia" | hiv_df$state == "Kentucky" |
                   hiv_df$state == "Mississippi" | hiv_df$state == "North Carolina" | hiv_df$state == "South Carolina" | hiv_df$state == "Tennessee"] <- "Region 4"
hiv_df$geography2[hiv_df$state == "Illinois" | hiv_df$state == "Indiana" | hiv_df$state == "Michigan" | hiv_df$state == "Minnesota" |
                   hiv_df$state == "Ohio" | hiv_df$state == "Wisconsin"] <- "Region 5"
hiv_df$geography2[hiv_df$state == "Arkansas" | hiv_df$state == "Louisiana" | hiv_df$state == "New Mexico" | hiv_df$state == "Oklahoma"] <- "Region 6"
hiv_df$geography2[hiv_df$state == "Iowa" | hiv_df$state == "Kansas" | hiv_df$state == "Missouri" | hiv_df$state == "Nebraska"] <- "Region 7"
hiv_df$geography2[hiv_df$state == "Colorado" | hiv_df$state == "Montana" | hiv_df$state == "North Dakota" | hiv_df$state == "South Dakota" |
                   hiv_df$state == "Utah" | hiv_df$state == "Wyoming"] <- "Region 8"
hiv_df$geography2[hiv_df$state == "Arizona" | hiv_df$state == "Hawaii" | hiv_df$state == "Nevada"] <- "Region 9"
hiv_df$geography2[hiv_df$state == "Alaska" | hiv_df$state == "Idaho" | hiv_df$state == "Oregon" | hiv_df$state == "Washington"] <- "Region 10"

hiv_df$geography2[hiv_df$state == "Florida"] = "Florida" 
hiv_df$geography2[hiv_df$state == "California"] = "California" 
hiv_df$geography2[hiv_df$state == "Texas"] = "Texas" 


hiv_df$state2 <- hiv_df$state
hiv_df$state2[hiv_df$state == "Delaware"] <- "Pennsylvania"
hiv_df$state2[hiv_df$state == "Idaho"] <- "Illinois"
hiv_df$state2[hiv_df$state == "North Dakota"] <- 
hiv_df$state2[hiv_df$state == "Massachusetts"] <- "Rhode Island"
hiv_df$state2[hiv_df$state == "Vermont"] <- "Rhode Island"
hiv_df$state2[hiv_df$state == "New Hampshire"] <- "Rhode Island"
hiv_df$state2[hiv_df$state == "Iowa"] <- "Kansas"
hiv_df$state2[hiv_df$state == "Wyoming"] <- "Wisconsin"

hiv_df$state2[hiv_df$state == "Alaska"] <- "Colorado"
hiv_df$state2[hiv_df$state == "Puerto Rico"] <- "New Mexico"
hiv_df$state2[hiv_df$state == "New Jersey"] <- "New York"
hiv_df$state2[hiv_df$state == "Mississippi"] <- "Tennessee"
hiv_df$state2[hiv_df$state == "Hawaii"] <- "Oregon"
hiv_df$state2[hiv_df$state == "Missouri"] <- "Kansas"
hiv_df$state2[hiv_df$state == "South Dakota"] <- "Oregon"
hiv_df$state2[hiv_df$state == "Iowa"]  <- "Indiana"
hiv_df$state2[hiv_df$state == "Montana"]  <- "Oklahoma"




##################
# Split data into training and test 30 times
n <- 20

train_mse_df <-  test_mse_df <- data.frame(matrix(NA, nrow =n, ncol =8))
colnames(train_mse_df) <- colnames(test_mse_df) <- c("id", paste0("m", 1:7))
train_mse_df$id <- "train"
test_mse_df$id <- "test"

for ( i in 1:n ) {
  trainset <- sample(rep(c(TRUE,FALSE),length.out=nrow(hiv_df)))
  
  m1 <- glm.nb(tsar_case ~ Sex + `Race/Ethnicity` + age + year + transcat + geography2 + trans_case  + s_case + s_d_case  + offset(log(`acs_transm_pop`)), data = hiv_df[trainset,])
  m1tr <- predict(m1, hiv_df[trainset,], type = "response")
  m1tst <- predict(m1, hiv_df[!trainset,], type = "response")
  
  m2 <-  glm(tsar_case ~ Sex + `Race/Ethnicity` + age + year + transcat  + s_case + s_d_case + offset(log(`acs_transm_pop`)), data = hiv_df[trainset,], family = poisson(link = "log"))
  m2tr <- predict(m2, hiv_df[trainset,], type = "response")
  m2tst <- predict(m2, hiv_df[!trainset,], type = "response")
  
  m3 <-  glmer(tsar_case ~ Sex + transcat + age + year +`Race/Ethnicity` + geography2 + trans_case  + s_case + s_d_case + offset(log(`acs_transm_pop`)) + (1 | `Race/Ethnicity`), data = hiv_df[trainset,], family = poisson(link = "log"))
  m3tr <- predict(m3, hiv_df[trainset,], type = "response")
  m3tst <- predict(m3, hiv_df[!trainset,], type = "response")
  
  m4 <- glmer(tsar_case ~ Sex + transcat + age + year +`Race/Ethnicity`  +  geography2+  trans_case  + s_case + s_d_case  + offset(log(`acs_transm_pop`)) + (1 | transcat), data = hiv_df[trainset,], family = poisson(link = "log"))
  m4tr <- predict(m4, hiv_df[trainset,], type = "response")
  m4tst <- predict(m4, hiv_df[!trainset,], type = "response")
  
  m5 <- glm(tsar_case ~ Sex * `Race/Ethnicity` *  transcat + age  +  trans_case +  geography2 + year + s_case + s_d_case + acs_transm_pop+ offset(log(trans_case)), data = hiv_df[trainset,], family = poisson(link = "log"))
  m5tr <- predict(m5, hiv_df[trainset,], type = "response")
  m5tst <- predict(m5, hiv_df[!trainset,], type = "response")
  
  m6 <- glm(tsar_case ~ Sex * `Race/Ethnicity` *  transcat + age  + trans_case + geography2 + year + s_case + s_d_case + acs_transm_pop+ offset(log(s_case)), data = hiv_df[trainset,], family = poisson(link = "log"))
  m6tr <- predict(m6, hiv_df[trainset,], type = "response")
  m6tst <- predict(m6, hiv_df[!trainset,], type = "response")
  
  m7 <- glm(tsar_case ~ Sex * `Race/Ethnicity` *  transcat * age  +  trans_case +  geography2 + year + s_case + s_d_case + acs_transm_pop+ offset(log(trans_case)) , data = hiv_df[trainset,], family = poisson(link = "log"))
  m7tr <- predict(m7, hiv_df[trainset,], type = "response")
  m7tst <- predict(m7, hiv_df[!trainset,], type = "response")
  
  train_mse_df[i,"m1"] <- sqrt( colMeans((m1tr-hiv_df[trainset,"tsar_case"])^2, na.rm=T) )
  train_mse_df[i,"m2"] <- sqrt( colMeans((m2tr-hiv_df[trainset,"tsar_case"])^2, na.rm=T) )
  train_mse_df[i,"m3"] <- sqrt( colMeans((m3tr-hiv_df[trainset,"tsar_case"])^2, na.rm=T) )
  train_mse_df[i,"m4"] <- sqrt( colMeans((m4tr-hiv_df[trainset,"tsar_case"])^2, na.rm=T) )
  train_mse_df[i,"m5"] <- sqrt( colMeans((m5tr-hiv_df[trainset,"tsar_case"])^2, na.rm=T) )
  train_mse_df[i,"m6"] <- sqrt( colMeans((m6tr-hiv_df[trainset,"tsar_case"])^2, na.rm=T) )
  train_mse_df[i,"m7"] <- sqrt( colMeans((m7tr-hiv_df[trainset,"tsar_case"])^2, na.rm=T) )
  
  test_mse_df[i,"m1"] <- sqrt( colMeans((m1tst-hiv_df[!trainset,"tsar_case"])^2, na.rm=T) )
  test_mse_df[i,"m2"] <- sqrt( colMeans((m2tst-hiv_df[!trainset,"tsar_case"])^2, na.rm=T) )
  test_mse_df[i,"m3"] <- sqrt( colMeans((m3tst-hiv_df[!trainset,"tsar_case"])^2, na.rm=T) )
  test_mse_df[i,"m4"] <- sqrt( colMeans((m4tst-hiv_df[!trainset,"tsar_case"])^2, na.rm=T) )
  test_mse_df[i,"m5"] <- sqrt( colMeans((m5tst-hiv_df[!trainset,"tsar_case"])^2, na.rm=T) )
  test_mse_df[i,"m6"] <- sqrt( colMeans((m6tst-hiv_df[!trainset,"tsar_case"])^2, na.rm=T) )
  test_mse_df[i,"m7"] <- sqrt( colMeans((m7tst-hiv_df[!trainset,"tsar_case"])^2, na.rm=T) )
}

mse_all <- rbind(test_mse_df, train_mse_df)

mse_all %>%
  melt(id.vars = "id") %>%
  ggplot(aes(x=variable,y=value,colour=id)) +
  geom_boxplot() +
  ggtitle("MSE") +
  ylab("mean squared error") + xlab("model") +
  theme_bw() + ylim(c(0, NA)) -> p00

ggsave(filename = here("output", "hiv_imput_predict_mse.png"),
       plot = p00, 
       bg = "white",
       width = 6,
       height = 6, 
       dpi = 300)

hiv_df$m1 <- predict(m1, hiv_df, type = "response")
hiv_df$m2 <- predict(m2, hiv_df, type = "response")
hiv_df$m3 <- predict(m3, hiv_df, type = "response")
hiv_df$m4 <- predict(m4, hiv_df, type = "response")
hiv_df$m5 <- predict(m5, hiv_df, type = "response")
hiv_df$m6 <- predict(m6, hiv_df, type = "response")
hiv_df$m7 <- predict(m7, hiv_df, type = "response")

hiv_df$m1_d <- (hiv_df$m1-hiv_df$tsar_case)
hiv_df$m2_d <- (hiv_df$m2-hiv_df$tsar_case)
hiv_df$m3_d <- (hiv_df$m3-hiv_df$tsar_case)
hiv_df$m4_d <- (hiv_df$m4-hiv_df$tsar_case) 
hiv_df$m5_d <- (hiv_df$m5-hiv_df$tsar_case) 
hiv_df$m6_d <- (hiv_df$m6-hiv_df$tsar_case) 
hiv_df$m7_d <- (hiv_df$m7-hiv_df$tsar_case) 

hiv_df %>% 
  select(age:transcat, transcat, m7) -> hiv_pred

write.csv(hiv_pred,here("data", "hiv_prevalence_prediction.csv"), row.names = FALSE)

hiv_df %>%
  group_by( year, FIPS, state, Sex) %>%
  reframe(m1_case = sum(m1),
          m2_case = sum(m2),
          m3_case = sum(m3),
          m4_case = sum(m4),
          m5_case = sum(m5),
          m6_case = sum(m6),
          m7_case = sum(m7)) %>%
  ungroup() %>%
  left_join(s_df, by = c("Sex", "FIPS", "state", "year")) %>%
  mutate(m1diff = s_case-m1_case,
         m2diff = s_case-m2_case,
         m3diff = s_case-m3_case,
         m4diff = s_case-m4_case,
         m5diff = s_case-m5_case,
         m6diff = s_case-m6_case,
         m7diff = s_case-m7_case) -> all_res_by_sex

sum(abs(all_res_by_sex$m1diff))
sum(abs(all_res_by_sex$m2diff))
sum(abs(all_res_by_sex$m3diff))
sum(abs(all_res_by_sex$m4diff))
sum(abs(all_res_by_sex$m5diff))
sum(abs(all_res_by_sex$m6diff))
sum(abs(all_res_by_sex$m7diff))

hiv_df %>%
  group_by( year, FIPS, state, transm) %>%
  reframe(m1_case = sum(m1),
          m2_case = sum(m2),
          m3_case = sum(m3),
          m4_case = sum(m4),
          m5_case = sum(m5),
          m6_case = sum(m6),
          m7_case = sum(m7)) %>%
  ungroup() %>%
  left_join(t_df2, by = c("FIPS", "state", "year", "transm")) %>%
  mutate(m1diff = trans_case-m1_case,
         m2diff = trans_case-m2_case,
         m3diff = trans_case-m3_case,
         m4diff = trans_case-m4_case,
         m5diff = trans_case-m5_case,
         m6diff = trans_case-m6_case,
         m7diff = trans_case-m7_case) -> all_res_by_transcat
  
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "black" )


all_res_by_sex %>%
  filter(year == 2022) %>%
  pivot_longer(cols =  m1_case:s_case , names_to = "estimate", values_to = "value") %>%
  ggplot(aes(y=state, x=value, color=estimate)) +
  geom_point(alpha=0.8) + theme_bw() +
  scale_color_manual(values = custom_colors)+
  facet_wrap(~Sex)

all_res_by_sex %>%
  filter(year == 2022) %>%
  pivot_longer(cols =  c("m7_case", "s_case") , names_to = "estimate", values_to = "value") %>%
  ggplot(aes(y=state, x=value, color=estimate)) +
  geom_point(alpha=0.5, size=0.8) + theme_minimal() +
  scale_color_manual(values = custom_colors, labels = c("m7", "HIV surveillance data"))+
  facet_wrap(~Sex, scales = "free_x" ) + theme(legend.position = "bottom") -> p0

ggsave(filename = here("output", "hiv_imput_predict_sex_group.png"),
       plot = p0, 
       bg = "white",
       width = 6,
       height = 7, 
       dpi = 300)
# 
# all_res_by_transcat  %>%
#   filter(year == 2022) %>%
#   pivot_longer(cols =  m1_case:trans_case, names_to = "estimate", values_to = "value") %>%
#   filter(estimate != "msm_prop" &  estimate !="het_prop") %>%
#   ggplot(aes(y=state, x=value, color=estimate)) +
#   geom_point(alpha=0.8) + 
#   scale_color_manual(values = custom_colors) + theme_minimal() +
#   facet_wrap(~transm)

all_res_by_transcat %>%
  filter(year == 2022) %>%
  pivot_longer(cols =  c("m7_case", "trans_case"), names_to = "estimate", values_to = "value") %>%
  filter(estimate != "msm_prop" &  estimate !="het_prop") %>%
  ggplot(aes(y=state, x=value, color=estimate)) +
  geom_point(alpha=0.5, size=0.8) + 
  scale_color_manual(values = custom_colors, labels = c("m7", "HIV surveillance data")) + theme_minimal() + theme(legend.position = "bottom") +
  facet_wrap(~transm, labeller =  as_labeller(c(`idu` = "PWID", `het` = "PHET", `msm` = "MSM")), scales = "free_x" ) -> p1

ggsave(filename = here("output", "hiv_imput_predict_transm_group.png"),
       plot = p1, 
       bg = "white",
       width = 14,
       height = 7, 
       dpi = 300)



anova( m1, m2, m3, m4, m5, m6, m7)

hivd_df$tsar_m1 <- predict(m1, hivd_df, type = "response")

## adding 0.001, otherise dividing by zero
hivd_df$diff_m1 <- abs(hivd_df$tsar_case+0.001-(hivd_df$tsar_m1+0.001)) / (hivd_df$tsar_case+0.001)


sums(hivd_df$diff_m1)


sums <- function(dat){
  
  t <- matrix(NA, 7)   
  t[1] <-   sum(dat, na.rm=T)
  t[2] <-   mean(dat, na.rm=T)
  t[3] <-   median(dat, na.rm=T)
  t[4] <-   quantile(dat, na.rm=T, probs = 0.25)
  t[5] <-   quantile(dat, na.rm=T, probs = 0.75)
  t[6] <-   min(dat, na.rm=T)
  t[7] <-   max(dat, na.rm=T)
  
  rownames(t) <- c("sum", "mean", "median", "p25", "p75", "min", "max")
  
  return(round(t, 4))
}




plot(hivd_df$sar_cumcase_di, hivd_df$sar_m8, col=factor(hivd_df$`Race/Ethnicity`))
legend("topleft", pch=19,
       legend = levels(factor(hivd_df$`Race/Ethnicity`)),
       col = factor(levels(factor(hivd_df$`Race/Ethnicity`))))


hivd_df %>% 
  mutate(hiv_size = ifelse(sar_cumcase_di<1, "cases <1", ifelse(sar_cumcase_di > 0 & sar_cumcase_di< 101, "cases 1-100", 
                                                                ifelse(sar_cumcase_di > 100 & sar_cumcase_di < 1001, "cases 101-1000", 
                                                                       ifelse(sar_cumcase_di > 1000, "cases >1000", NA) )))) %>%
  #mutate(sar_m7_rate = sar_m7/sar_meanpop_di) %>%
  filter(!is.na(hiv_size)) %>%
  # ggplot(aes(x = sar_cumrate_di, y = sar_m7_rate, color = `Race/Ethnicity`)) +
  ggplot(aes(x = sar_cumcase_di, y = sar_m8, color = Sex)) +
  geom_point() +
  facet_wrap(~hiv_size+`Race/Ethnicity`, scales = "free")




hivd_df %>%
  #   filter(state == "Massachusetts") %>%
  filter(state == "Washington")-> test

hivd_df$sar_case_m3 <- round(predict(m3, hivd_df, type = "response")) 
hivd_df$sar_case_m4 <- round(predict(m4, hivd_df, type = "response")) 

hivd_df$reldiff_m1 <-  (hivd_df$sar_case_m1-hivd_df$sar_case) 
hivd_df$reldiff_m2 <-  (hivd_df$sar_case_m2-hivd_df$sar_case) 
hivd_df$reldiff_m3 <-  (hivd_df$sar_case_m3-hivd_df$sar_case) 
hivd_df$reldiff_m4 <-  (hivd_df$sar_case_m4-hivd_df$sar_case) 

# all sex age
m1 <- glm.nb(sar_case ~ Sex + `Race/Ethnicity` * `Age Group` + year + nsa_case + geography+ offset(log(sar_pop)), data = hivd_df)  

summary(m2 <- glm.nb(sar_case ~ Sex + `Race/Ethnicity` * `Age Group` + year + geography+ offset(log(sar_pop*nsa_case)), data = hivd_df))  

anova(m1, m2)

hivd_df$sar_case_m1 <- predict(m1, hivd_df, type = "response") 

hivd_df$case_diff07 <- predict(m1, hivd_df, type = "response") 

hivd_df$case_diff08 <-  hivd_df$sar_case-hivd_df$sar_case_nb07

plot(hivd_df$nsa_case, hivd_df$test)


##
# library(AICcmodavg)
library(ggplot2)
library(here)
library(lme4)
library(MASS)
library(pscl)
library(readxl)
library(tidycensus)
library(tidyverse)

source(here('code/acs_fun.R'))


#  rm(list = ls())
# .rs.restartR()

pop_re_2015  <- pop_re_fun(2015, "2008-2014") 

pivot_longer(pop_re_2015$f, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "`Age Group`", values_to = "acs_pop") %>% 
  bind_rows(., pivot_longer(pop_re_2015$m, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "`Age Group`", values_to = "acs_pop")) %>%
  filter(state == "Puerto Rico") %>%
  filter(Race.Ethnicity != "White_any") %>%
  group_by(Sex, Race.Ethnicity, state) %>%
  summarise(est_pop = sum(acs_pop)) -> pop_sr_long

pivot_longer(pop_re_2015$f, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "`Age Group`", values_to = "acs_pop") %>% 
  bind_rows(., pivot_longer(pop_re_2015$m, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "`Age Group`", values_to = "acs_pop")) %>%
  filter(state == "Puerto Rico") %>%
  filter(Race.Ethnicity != "White_any") -> pop_sar_long

# pop_re_long %>%
#   filter(Race.Ethnicity != "White_any") %>%
#   group_by(Sex, `Age Group`) %>%
#   summarise(est_pop = sum(acs_pop)) -> test

read.csv(file = here("data/AtlasPlusTableData_hivdiag_state_sex_age_re_year.csv"), skip = 10) %>%
  mutate(sar_rate0 = as.numeric(ifelse(Rate.per.100000=="Data suppressed", NA, Rate.per.100000))) %>%
  mutate(sar_case = as.numeric(ifelse(Cases=="Data suppressed", NA, gsub(",", "", Cases)))) %>%
  mutate(sar_pop0 = as.numeric(ifelse(Population=="Data suppressed", NA, gsub(",", "", Population)))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, Year))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  filter(year < 2015) %>%
  left_join(pop_sar_long, by = c("Sex", "Age Group", "Race.Ethnicity", "state", "FIPS"))  %>%
  mutate(sar_pop = ifelse(state == "Puerto Rico", acs_pop, sar_pop0)) %>%
  mutate(sar_rate = ifelse(state == "Puerto Rico", 10^5 * sar_case/sar_pop , sar_rate0)) %>%
  dplyr::select(`Age Group`, Sex, FIPS, state, year, sar_rate, sar_case,  sar_pop, Race.Ethnicity, Indicator) %>%
  # filter(state == "Massachusetts") %>%
  group_by(Indicator, state, FIPS, `Age Group`, Race.Ethnicity, Sex) %>%
  summarise(
    sar_cumcase = sum(sar_case),
    sar_meancase = mean(sar_case, na.rm = TRUE),
    sar_meanrate = mean(sar_rate, na.rm=TRUE),
    sar_meanpop = mean(sar_pop, na.rm=TRUE))  %>%
  ungroup() %>%
  group_by(Sex, state, FIPS, Indicator) %>%
  summarise(
    across(), 
    nsar_sum_case = sum(sar_cumcase, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(p_agerace = ifelse(nsar_sum_case == 0, 0, sar_cumcase/nsar_sum_case)) %>%
  mutate(sar_cumrate =  sar_cumcase/sar_meanpop)  -> sar_df

# sex X age
read.csv(file = here("data/AtlasPlusTableData_hivdiag_state_sex_age_year.csv"), skip = 10) %>%
  mutate(sa_rate = as.numeric(ifelse(Rate.per.100000=="Data suppressed", NA, Rate.per.100000))) %>%
  mutate(sa_case = as.numeric(ifelse(Cases=="Data suppressed", NA, gsub(",", "", Cases)))) %>%
  mutate(sa_pop = as.numeric(ifelse(Population=="Data suppressed", NA, gsub(",", "", Population)))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, Year))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  filter(year < 2015) %>%
  dplyr::select(`Age Group`, Sex, FIPS, state, sa_rate, sa_case,  sa_pop, Race.Ethnicity, Indicator) %>%
  group_by(Indicator, state, FIPS, `Age Group`, Sex) %>%
  summarise(
    sa_cumcase = sum(sa_case),
    sa_meancase = mean(sa_case, na.rm = TRUE),
    sa_meanrate = mean(sa_rate, na.rm=TRUE),
    sa_meanpop = mean(sa_pop, na.rm=TRUE))  %>%
  ungroup() %>%
  group_by(Sex, state, FIPS, Indicator) %>%
  summarise(
    across(), 
    nsa_sum_case = sum(sa_cumcase)) %>%
  ungroup() %>%
  mutate(p_age = ifelse(nsa_sum_case == 0, 0, sa_cumcase/nsa_sum_case)) %>%
  mutate(sa_cumrate =  sa_cumcase/sa_meanpop)  ->  sa_df

# sex X race
read.csv(file = here("data/AtlasPlusTableData_hivdiag_state_sex_re_year.csv"), skip = 10) %>%
  mutate(sr_rate0 = as.numeric(ifelse(Rate.per.100000=="Data suppressed", NA, Rate.per.100000))) %>%
  mutate(sr_case = as.numeric(ifelse(Cases=="Data suppressed", NA, gsub(",", "", Cases)))) %>%
  mutate(sr_pop0 = as.numeric(ifelse(Population=="Data suppressed", NA, gsub(",", "", Population)))) %>%
  mutate(year = as.numeric(ifelse(Year=="2020 (COVID-19 Pandemic)", 2020, Year))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  filter(year < 2015)  %>%
  # filter(state == "Puerto Rico") -> test
  left_join(pop_sr_long, by = c("Sex", "Race.Ethnicity", "state"))  %>%
  mutate(sr_pop = ifelse(state == "Puerto Rico", est_pop, sr_pop0)) %>%
  mutate(sr_rate = ifelse(state == "Puerto Rico", 10^5 * sr_case/sr_pop , sr_rate0)) %>%
  dplyr::select(`Age Group`, Sex, FIPS, state, sr_rate, sr_case,  sr_pop, Race.Ethnicity, Indicator)  %>% 
  group_by(Indicator, state, FIPS, Race.Ethnicity, Sex) %>%
  summarise(
    sr_cumcase = sum(sr_case, na.rm = TRUE),
    sr_meancase = mean(sr_case, na.rm = TRUE),
    sr_meanrate = mean(sr_rate, na.rm=TRUE),
    sr_meanpop = mean(sr_pop, na.rm=TRUE))  %>%
  ungroup()   %>%
  group_by(Sex, state, FIPS, Indicator) %>%
  summarise(
    across(), 
    nsr_sum_case = sum(sr_cumcase)) %>%
  ungroup() %>%
  mutate(p_race = ifelse(nsr_sum_case == 0, 0, sr_cumcase/nsr_sum_case)) %>%
  mutate(sr_cumrate =  sr_cumcase/sr_meanpop) %>%
  mutate(sr_cumrate_rm0 = ifelse(sr_cumrate==0, 10^-30, sr_cumrate)) %>%
  mutate(sr_cumcase_rm0 = ifelse(sr_cumcase==0, 0.001, sr_cumcase)) -> sr_df


sar_df %>%
  left_join(sa_df, by = c("Age Group", "Sex", "FIPS", "state",  "Indicator")) %>% 
  left_join(sr_df, by = c("Sex", "Race.Ethnicity", "FIPS", "state", "Indicator")) %>%
  mutate(indic = ifelse(Indicator == "HIV diagnoses", "di", "prev")) %>%
  select(Sex, state, FIPS, indic, `Age Group`, Race.Ethnicity, sa_meanrate,  sr_meanrate, sr_meanpop, sa_meanpop, sar_meanpop,  
         p_age, p_race, p_agerace, sr_cumrate,  sr_cumrate_rm0, sr_cumcase_rm0, sa_cumrate,  sar_cumrate, sr_cumcase, sa_cumcase, sar_cumcase) %>%
  mutate(sar_cumrate_rm0 = ifelse(sar_cumrate==0, 10^-30, sar_cumrate)) %>%
  # mutate(sar_cumrate_rm0 = sar_cumrate+1) %>%
  # mutate(logit_sar_rate = log(sar_cumrate_rm0/(1-sar_cumrate_rm0))) %>%
  # filter() %>% ## has suppressed data 
  pivot_wider(names_from = indic, values_from = c(sa_meanrate,  sr_meanrate, sr_meanpop, sa_meanpop, sar_meanpop,
                                                  p_age, p_race, p_agerace, 
                                                  sr_cumrate, sr_cumrate_rm0, sr_cumcase_rm0, sa_cumrate, sar_cumrate,sar_cumrate_rm0,
                                                  sr_cumcase, sa_cumcase, sar_cumcase )) %>%
  filter(state !="American Samoa" & state != "Guam" & state!="Northern Mariana Islands" & state!= "US Virgin Islands" & state != "New Hampshire") %>%
  relocate(c(sr_cumcase_di, sr_cumcase_rm0_di, sa_cumcase_di, sar_cumcase_di), .after = sar_cumcase_prev) -> hivd_df
# filter(is.na(sar_cumrate_di)) -> hivd_df

# hivd_df$logit_sar_cumrate_di <- log(hivd_df$sar_cumrate_rm0_di/(1-hivd_df$sar_cumrate_rm0_di))

################

hivd_df$geography[hivd_df$state == "Maine" | hivd_df$state == "Massachusetts" | hivd_df$state == "Connecticut" | hivd_df$state == "New Hampshire" |
                    hivd_df$state == "Rhode Island" | hivd_df$state == "Vermont"] <- "Region 1"
hivd_df$geography[hivd_df$state == "New Jersey" | hivd_df$state == "New York" | hivd_df$state == "Puerto Rico"] <- "Region 2"
hivd_df$geography[hivd_df$state == "Delaware" | hivd_df$state == "District of Columbia" | hivd_df$state == "Maryland" | hivd_df$state == "Pennsylvania" |
                    hivd_df$state == "Virginia" | hivd_df$state == "West Virginia"] <- "Region 3"
hivd_df$geography[hivd_df$state == "Alabama" | hivd_df$state == "Florida" | hivd_df$state == "Georgia" | hivd_df$state == "Kentucky" |
                    hivd_df$state == "Mississippi" | hivd_df$state == "North Carolina" | hivd_df$state == "South Carolina" | hivd_df$state == "Tennessee"] <- "Region 4"
hivd_df$geography[hivd_df$state == "Illinois" | hivd_df$state == "Indiana" | hivd_df$state == "Michigan" | hivd_df$state == "Minnesota" |
                    hivd_df$state == "Ohio" | hivd_df$state == "Wisconsin"] <- "Region 5"
hivd_df$geography[hivd_df$state == "Arkansas" | hivd_df$state == "Louisiana" | hivd_df$state == "New Mexico" | hivd_df$state == "Oklahoma" |
                    hivd_df$state == "Texas"] <- "Region 6"
hivd_df$geography[hivd_df$state == "Iowa" | hivd_df$state == "Kansas" | hivd_df$state == "Missouri" | hivd_df$state == "Nebraska"] <- "Region 7"
hivd_df$geography[hivd_df$state == "Colorado" | hivd_df$state == "Montana" | hivd_df$state == "North Dakota" | hivd_df$state == "South Dakota" |
                    hivd_df$state == "Utah" | hivd_df$state == "Wyoming"] <- "Region 8"
hivd_df$geography[hivd_df$state == "Arizona" | hivd_df$state == "California" | hivd_df$state == "Hawaii" | hivd_df$state == "Nevada"] <- "Region 9"
hivd_df$geography[hivd_df$state == "Alaska" | hivd_df$state == "Idaho" | hivd_df$state == "Oregon" | hivd_df$state == "Washington"] <- "Region 10"

hivd_df$state2 <- hivd_df$state
hivd_df$state2[hivd_df$state == "Delaware"] <- "Pennsylvania"
hivd_df$state2[hivd_df$state == "Idaho"] <- "Illinois"
hivd_df$state2[hivd_df$state == "North Dakota"] <- "South Dakota"
hivd_df$state2[hivd_df$state == "Massachusetts"] <- "Rhode Island"
hivd_df$state2[hivd_df$state == "Vermont"] <- "Rhode Island"
hivd_df$state2[hivd_df$state == "Montana"] <- "Kansas"
hivd_df$state2[hivd_df$state == "Missouri"] <- "Kansas"



##################

hivd_df %>%
  filter(!is.na(sar_cumcase_di)) %>%
  group_by(FIPS) %>%
  summarise(stnames = unique(state )) -> stnames

tab <- state_predict_fun(hivd_df, stnames$stnames[1])

for (i in 2:length(stnames$FIPS)){  
  tab <- rbind(tab, state_predict_fun(hivd_df, stnames$stnames[i] ))
}



##################

# negative binomial 

m1 <- glm.nb(sar_cumcase_di ~ Sex + Race.Ethnicity + Age.Group  + geography + offset(log(`sa_cumcase_di`)), data = hivd_df)

m2 <- glm.nb(sar_cumcase_di ~ Sex + Race.Ethnicity + Age.Group  + geography + offset(log(`sr_cumcase_rm0_di`)), data = hivd_df)

m2.1 <- update(m1, . ~ . -offset(log(`sa_cumcase_di`)) + offset(log(`sr_cumcase_rm0_di`)))

m3 <- glm.nb(sar_cumcase_di ~ Sex + Race.Ethnicity * Age.Group  + geography + offset(log(`sr_cumcase_rm0_di`)), data = hivd_df)

m3.1 <- update(m2.1, . ~ . + Race.Ethnicity:Age.Group)

m4 <- glm.nb(sar_cumcase_di ~ Sex + Race.Ethnicity * Age.Group  + geography + offset(log(`sa_cumcase_di`)), data = hivd_df)

m4.1 <- update(m1, . ~ . + Race.Ethnicity:Age.Group)

m5 <- glm.nb(sar_cumcase_di ~ Sex + Race.Ethnicity * Age.Group  + geography + 
               sr_meanrate_prev +  sa_meanrate_prev + offset(log(`sr_cumcase_rm0_di`)), data = hivd_df)

m5.1 <- update(m3.1, . ~ . + sr_meanrate_prev +  sa_meanrate_prev )

m6 <- glm.nb(sar_cumcase_di ~ Sex * Race.Ethnicity * Age.Group  + state2  + 
               sr_meanrate_prev +  sa_meanrate_prev + offset(log(`sr_cumcase_rm0_di`)), data = hivd_df)

m6.1 <- update(m5.1,  . ~ . -geography + state2 + Race.Ethnicity:Sex + Sex:Age.Group)


m7 <- glm.nb(sar_cumcase_di ~ Sex * Race.Ethnicity * Age.Group  + geography  + 
               p_age_di +   p_race_di + sr_meanrate_prev +  sa_meanrate_prev + offset(log(`sr_cumcase_rm0_di`)), data = hivd_df)

m7.1 <- update(m6.1, . ~ . +geography - state2 + p_age_di +   p_race_di )

m8 <- glm.nb(sar_cumcase_di ~ Sex * Race.Ethnicity * Age.Group  + state2  +
               p_age_di + p_race_di + sr_meanrate_prev +  sa_meanrate_prev + offset(log(`sr_cumcase_rm0_di`)), data = hivd_df)

m8.1 <- update(m7.1, . ~ . -geography + state2)

m9 <- glm.nb(sar_cumcase_di ~ Sex * Race.Ethnicity * Age.Group  + state2  + geography +  
               p_age_di +  p_race_di +p_agerace_di + sr_meanrate_prev +  sa_meanrate_prev + offset(log(`sr_cumcase_rm0_di`)), data = hivd_df)


m9.1 <- update(m8.1, . ~ . +geography +p_agerace_di)


anova( m1, m2.1, m3.1, m4.1, m5.1, m6.1, m7.1, m8.1, m9.1)
anova( m1, m2, m3, m4, m5, m6, m7, m8, m9)

hivd_df$sar_m1 <- predict(m1, hivd_df, type = "response")
hivd_df$sar_m2 <- predict(m2, hivd_df, type = "response")  # allow.new.levels = TRUE
hivd_df$sar_m3 <- predict(m3, hivd_df, type = "response") 
hivd_df$sar_m4 <- predict(m4, hivd_df, type = "response") 
hivd_df$sar_m5 <- predict(m5, hivd_df, type = "response") 
hivd_df$sar_m6 <- predict(m6, hivd_df, type = "response") 
hivd_df$sar_m7 <- predict(m7, hivd_df, type = "response") 
hivd_df$sar_m8 <- predict(m8, hivd_df, type = "response") 
hivd_df$sar_m9 <- predict(m9, hivd_df, type = "response")
# hivd_df$sar_m10 <- predict(m10, hivd_df, type = "response")
# hivd_df$sar_m11 <- predict(m11, hivd_df, type = "response")

## adding 0.001, otherise dividing by zero
hivd_df$diff_m1 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m1+0.001)) / (hivd_df$sar_cumcase_di+0.001)
hivd_df$diff_m2 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m2+0.001)) / (hivd_df$sar_cumcase_di+0.001)
hivd_df$diff_m3 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m3+0.001)) / (hivd_df$sar_cumcase_di+0.001)
hivd_df$diff_m4 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m4+0.001)) / (hivd_df$sar_cumcase_di+0.0001)
hivd_df$diff_m5 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m5+0.001)) / (hivd_df$sar_cumcase_di+0.001)
hivd_df$diff_m6 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m6+0.001)) / (hivd_df$sar_cumcase_di+0.001)
hivd_df$diff_m7 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m7+0.001)) / (hivd_df$sar_cumcase_di+0.001)
hivd_df$diff_m8 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m8+0.001)) / (hivd_df$sar_cumcase_di+0.001)
hivd_df$diff_m9 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m9+0.001)) / (hivd_df$sar_cumcase_di+0.001)
# hivd_df$diff_m10 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m10+0.001)) / (hivd_df$sar_cumcase_di+0.001)
# hivd_df$diff_m11 <- abs(hivd_df$sar_cumcase_di+0.001-(hivd_df$sar_m11+0.001)) / (hivd_df$sar_cumcase_di+0.001)



sums(hivd_df$diff_m1)
sums(hivd_df$diff_m2)
sums(hivd_df$diff_m3)
sums(hivd_df$diff_m4)
sums(hivd_df$diff_m5)
sums(hivd_df$diff_m6)
sums(hivd_df$diff_m7)
sums(hivd_df$diff_m8)
sums(hivd_df$diff_m9)


sums <- function(dat){
  
  t <- matrix(NA, 7)   
  t[1] <-   sum(dat, na.rm=T)
  t[2] <-   mean(dat, na.rm=T)
  t[3] <-   median(dat, na.rm=T)
  t[4] <-   quantile(dat, na.rm=T, probs = 0.25)
  t[5] <-   quantile(dat, na.rm=T, probs = 0.75)
  t[6] <-   min(dat, na.rm=T)
  t[7] <-   max(dat, na.rm=T)
  
  rownames(t) <- c("sum", "mean", "median", "p25", "p75", "min", "max")
  
  return(round(t, 4))
}




plot(hivd_df$sar_cumcase_di, hivd_df$sar_m8, col=factor(hivd_df$Race.Ethnicity))
legend("topleft", pch=19,
       legend = levels(factor(hivd_df$Race.Ethnicity)),
       col = factor(levels(factor(hivd_df$Race.Ethnicity))))


hivd_df %>% 
  mutate(hiv_size = ifelse(sar_cumcase_di<1, "cases <1", ifelse(sar_cumcase_di > 0 & sar_cumcase_di< 101, "cases 1-100", 
                                                                ifelse(sar_cumcase_di > 100 & sar_cumcase_di < 1001, "cases 101-1000", 
                                                                       ifelse(sar_cumcase_di > 1000, "cases >1000", NA) )))) %>%
  #mutate(sar_m7_rate = sar_m7/sar_meanpop_di) %>%
  filter(!is.na(hiv_size)) %>%
  # ggplot(aes(x = sar_cumrate_di, y = sar_m7_rate, color = Race.Ethnicity)) +
  ggplot(aes(x = sar_cumcase_di, y = sar_m8, color = Sex)) +
  geom_point() +
  facet_wrap(~hiv_size+Race.Ethnicity, scales = "free")




hivd_df %>%
  #   filter(state == "Massachusetts") %>%
  filter(state == "Washington")-> test

hivd_df$sar_case_m3 <- round(predict(m3, hivd_df, type = "response")) 
hivd_df$sar_case_m4 <- round(predict(m4, hivd_df, type = "response")) 

hivd_df$reldiff_m1 <-  (hivd_df$sar_case_m1-hivd_df$sar_case) 
hivd_df$reldiff_m2 <-  (hivd_df$sar_case_m2-hivd_df$sar_case) 
hivd_df$reldiff_m3 <-  (hivd_df$sar_case_m3-hivd_df$sar_case) 
hivd_df$reldiff_m4 <-  (hivd_df$sar_case_m4-hivd_df$sar_case) 

# all sex age
m1 <- glm.nb(sar_case ~ Sex + Race.Ethnicity * Age.Group + year + nsa_case + geography+ offset(log(sar_pop)), data = hivd_df)  

summary(m2 <- glm.nb(sar_case ~ Sex + Race.Ethnicity * Age.Group + year + geography+ offset(log(sar_pop*nsa_case)), data = hivd_df))  

anova(m1, m2)

hivd_df$sar_case_m1 <- predict(m1, hivd_df, type = "response") 

hivd_df$case_diff07 <- predict(m1, hivd_df, type = "response") 

hivd_df$case_diff08 <-  hivd_df$sar_case-hivd_df$sar_case_nb07

plot(hivd_df$nsa_case, hivd_df$test)


