
# devtools::install_github("warnes/SASxport")

library(SASxport)
library(survey)
library(nhanesA)
library(labelled)
library(ggpubr)
library(geofacet)
library(here)
library(readxl)
library(scales)
library(tidycensus)
library(tidyverse)

#        rm(list = ls())
#        .rs.restartR()

source(here('code/acs_fun.R'))

source(here('code/figure_specs.R'))

########################################
# nhanes
source("~/prep_need/code/nhanes_import_funs.R")
# 
source("~/prep_need/code/nhanes_data_import_hiv.R")

# nhns_d <- svydesign(id        = ~SDMVPSU,
#                       strata  = ~SDMVSTRA,
#                       weights = ~WTMEC2YR, #~WTINT2YR, # all info from interviews
#                       nest    = TRUE,
#                       data    = nhns)

nhns_t <- svydesign(id        = ~SDMVPSU,
                    strata  = ~SDMVSTRA,
                    weights = ~mec8yr, #~WTINT2YR, # all info from interviews
                    nest    = TRUE,
                    data    = nhns)

nhns_t_hiv0 <- subset(nhns_t, hiv == 0)

###################################################################################
## 

nhns %>%
  #  filter(!is.na(prep_msm2)) %>%
  group_by(prep_msm, year) %>%
  summarise(n=n()) %>%
  spread(prep_msm, n) %>%
  knitr::kable()

# svytotal(~I(RIAGENDR == 2 & RIDAGEYR >= 18 & RIDAGEYR <= 54), nhns_t_hiv0)

###################################################################################
## Sexually active in the past 12 months

### WSM
svyby(~year_fem, nhns_t_hiv0, by = ~age1, svyciprop,  vartype="ci", method="logit" ) %>%
  arrange(., age1) %>%
  as_tibble() %>%
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3) -> tab_year_wsm

tab_year_wsm[tab_year_wsm$Age.Group=="13-17",2] <- 0.2
tab_year_wsm[tab_year_wsm$Age.Group=="13-17",3] <- 0.1
tab_year_wsm[tab_year_wsm$Age.Group=="13-17",4] <- 0.34
tab_year_wsm[tab_year_wsm$Age.Group=="13-24", c("year_fem","ci_l","ci_u")] <- 5/11*tab_year_wsm[tab_year_wsm$Age.Group=="13-17",c("year_fem","ci_l","ci_u")] +  
  6/11*tab_year_wsm[tab_year_wsm$Age.Group=="18-24",c("year_fem","ci_l","ci_u")]

svyciprop(~year_fem, nhns_t_hiv0,  vartype="ci", method="logit") 

#MSW
svyby(~prep_msw, nhns_t_hiv0, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3)  -> tab_msw

tab_msw[tab_msw$Age.Group=="13-17",2:4] <- tab_msw[tab_msw$Age.Group=="18-24",2:4]
tab_msw[tab_msw$Age.Group=="13-24",2:4] <- tab_msw[tab_msw$Age.Group=="18-24",2:4]

svyby(~year_msw, nhns_t_hiv0, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>%
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3) -> tab_year_msw

tab_year_msw[tab_year_msw$Age.Group=="13-17",2] <- 0.24
tab_year_msw[tab_year_msw$Age.Group=="13-17",3] <- 0.13
tab_year_msw[tab_year_msw$Age.Group=="13-17",4] <- 0.35
tab_year_msw[tab_year_msw$Age.Group=="13-24", c("year_msw","ci_l","ci_u")] <- 5/11*tab_year_msw[tab_year_msw$Age.Group=="13-17",c("year_msw","ci_l","ci_u")] +    
                                                                              6/11*tab_year_msw[tab_year_msw$Age.Group=="18-24",c("year_msw","ci_l","ci_u")]
  
###################################################################################
## PrEP - base definition

# WSM
svyby(~prep_fem, nhns_t_hiv0, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3)  -> tab_fem

svyciprop(~prep_fem, nhns_t_hiv0,  vartype="ci", method="logit") 

tab_fem[tab_fem$Age.Group=="13-17",2:4] <- tab_fem[tab_fem$Age.Group=="18-24",2:4]
tab_fem[tab_fem$Age.Group=="13-24",2:4] <- tab_fem[tab_fem$Age.Group=="18-24",2:4]

# MSW

svyby(~prep_msw, nhns_t_hiv0, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3)  -> tab_msw

svyciprop(~prep_msw, nhns_t_hiv0,  vartype="ci", method="logit") 

tab_msw[tab_msw$Age.Group=="13-17",2:4] <- tab_msw[tab_msw$Age.Group=="18-24",2:4]
tab_msw[tab_msw$Age.Group=="13-24",2:4] <- tab_msw[tab_msw$Age.Group=="18-24",2:4]

## MSM [prep_msm include AI only, not VI]

svyby(~prep_msm, nhns_t_hiv0, by = ~age_msm, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age_msm) %>%
  as_tibble() %>% 
  rename(Age.Group = age_msm) %>%
  add_row(Age.Group= "45-54") %>%
  add_row(Age.Group= "55+") %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3)  -> tab_msm

tab_msm %>%
  rename(year_msm = prep_msm) %>%
  mutate(year_msm = 1,
         ci_l = 1, 
         ci_u = 1) -> tab_year_msm 

tab_year_msm[tab_year_msm$Age.Group=="13-17",2] <- 0.245
tab_year_msm[tab_year_msm$Age.Group=="13-17",3] <- 0.14
tab_year_msm[tab_year_msm$Age.Group=="13-17",4] <- 0.35
tab_year_msm[tab_year_msm$Age.Group=="13-24", c("year_msm","ci_l","ci_u")] <- 5/11*tab_year_msm[tab_year_msm$Age.Group=="13-17",c("year_msm","ci_l","ci_u")] +  
  6/11*tab_year_msm[tab_year_msm$Age.Group=="18-24",c("year_msm","ci_l","ci_u")]


tab_msm[tab_msm$Age.Group=="13-17",2:4] <- tab_msm[tab_msm$Age.Group=="18-24",2:4]
tab_msm[tab_msm$Age.Group=="13-24",2:4] <- tab_msm[tab_msm$Age.Group=="18-24",2:4]

# unstable older age group estimates -> aggregate at higher level
tab_msm[tab_msm$Age.Group=="45-54" | tab_msm$Age.Group=="55+", c("prep_msm","ci_l","ci_u")] <- tab_msm[tab_msm$Age.Group=="45+", c("prep_msm","ci_l","ci_u")]

svyciprop(~prep_msm, nhns_t_hiv0,  vartype="ci", method="logit") 


###################################################################################
## PrEP - looser definition


# WSM
svyby(~prep_fem2, nhns_t_hiv0, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3)  -> tab_fem2

svyciprop(~prep_fem2, nhns_t_hiv0,  vartype="ci", method="logit") 

# MSW
svyby(~prep_msw2, nhns_t_hiv0, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3)  -> tab_msw2

svyciprop(~prep_msw2, nhns_t_hiv0,  vartype="ci", method="logit") 


# MSM

svyby(~prep_msm2, nhns_t_hiv0, by = ~age_msm, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age_msm) %>%
  as_tibble() %>% 
  rename(Age.Group = age_msm) %>%
  add_row(Age.Group= "45-54") %>%
  add_row(Age.Group= "55+") %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3) -> tab_msm2

svyby(~prep_msw2, nhns_t_hiv0, by = ~age2, svyciprop,  vartype="ci", method="logit") 

tab_msm2 %>%
  rename(year_msm = prep_msm2) %>%
  mutate(year_msm = 1,
         ci_l = 1, 
         ci_u = 1) -> tab_year_msm 

tab_year_msm[tab_year_msm$Age.Group=="13-17",2] <- 0.245
tab_year_msm[tab_year_msm$Age.Group=="13-17",3] <- 0.14
tab_year_msm[tab_year_msm$Age.Group=="13-17",4] <- 0.35
tab_year_msm[tab_year_msm$Age.Group=="13-24", c("year_msm","ci_l","ci_u")] <- 5/11*tab_year_msm[tab_year_msm$Age.Group=="13-17",c("year_msm","ci_l","ci_u")] +  
  6/11*tab_year_msm[tab_year_msm$Age.Group=="18-24",c("year_msm","ci_l","ci_u")]


tab_msm2[tab_msm2$Age.Group=="13-17",2:4] <- tab_msm2[tab_msm2$Age.Group=="18-24",2:4]
tab_msm2[tab_msm2$Age.Group=="13-24",2:4] <- tab_msm2[tab_msm2$Age.Group=="18-24",2:4]

# unstable older age group estimates -> aggregate at higher level
tab_msm2[tab_msm2$Age.Group=="45-54" | tab_msm2$Age.Group=="55+", c("prep_msm2","ci_l","ci_u")] <- tab_msm2[tab_msm2$Age.Group=="45+", c("prep_msm2","ci_l","ci_u")]

svyciprop(~prep_msm2, nhns_t_hiv0,  vartype="ci", method="logit") 


###################################################################################
# ##
# # MSM
# # standard
# pop.msm <- cbind(pop.msm, statefun(pop.msm, rr_re_msm), statefun_min(pop.msm, rr_re_msm), statefun_max(pop.msm, rr_re_msm))
# # pop.msm <-  cbind(pop.msm, prepstatefun(pop.msm, rr_re_msm))
# 
# 
# # MSW
# pop.msw <- cbind(pop.msw, statefun(pop.msw, rr_re_msw), statefun_min(pop.msw, rr_re_msw), statefun_max(pop.msw, rr_re_msw))
# # pop.msw <-  cbind(pop.msw, prepstatefun(pop.msw, rr_re_msw))
# 
# 
# # WSM
# pop.wsm <- cbind(pop.wsm, statefun(pop.wsm, rr_re_wsm), statefun_min(pop.wsm, rr_re_wsm), statefun_max(pop.wsm, rr_re_wsm))
# # pop.wsm <-  cbind(pop.wsm, prepstatefun(pop.wsm, rr_re_wsm))


  
  
 
