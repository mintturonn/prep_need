
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

source(here('code/prep_model_funs.R'))
source(here('code/acs_fun.R'))
# will give warnings:
source(here('code/prep_model_pars.R'))
source(here('code/figure_specs.R'))

## check this is OK

prep.eff <- par$base_case[par$params == "prep_eff"]

########################################
# nhanes
source("~/prep_denominator/code/nhanes_import_funs.R")

# will download the nhanes cycles -- takes couple minutes
source("~/prep_denominator/code/nhanes_import_hiv.R")

# statefun(hivdat, beta0, nact, safe, safe_eff, contact)

nhns_d <- svydesign(id        = ~SDMVPSU,
                      strata  = ~SDMVSTRA,
                      weights = ~WTMEC2YR, #~WTINT2YR, # all info from interviews
                      nest    = TRUE,
                      data    = nhns)

nhns_all <- subset(nhns_d, year>2008)
# nhns_all_het <- subset(nhns_d, year>2010)
# # gc and gc-diagubin
# round(svyby(~gcd, nhns_all, by = ~year_msm, svyciprop,  vartype="ci", method="logit") %>% 
#             arrange(year_msm),3)

# svyby(~gc, nhns_all, by = ~year_msm, svyciprop,  vartype="ci", method="logit")
## SOMETHING WEIRD IN yr_msm_ai_pr variable for <2009: table(nhns$yr_msm_ai_pr, nhns$year)
## NHANES tabs
# prepneed (among all)
# women
svyby(~prep_fem, nhns_all, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3)  -> tab_fem
 # mutate(Age.Group = ifelse(Age.Group=="18-24", "13-24", Age.Group)) -> tab_fem

tab_fem[tab_fem$Age.Group=="13-17",2:4] <- tab_fem[tab_fem$Age.Group=="18-24",2:4]
tab_fem[tab_fem$Age.Group=="13-24",2:4] <- tab_fem[tab_fem$Age.Group=="18-24",2:4]

svyby(~year_fem, nhns_all, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>%
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3) -> tab_year_fem

tab_year_fem[tab_year_fem$Age.Group=="13-17",2] <- 0.2
tab_year_fem[tab_year_fem$Age.Group=="13-17",3] <- 0.1
tab_year_fem[tab_year_fem$Age.Group=="13-17",4] <- 0.34
tab_year_fem[tab_year_fem$Age.Group=="13-24", c("year_fem","ci_l","ci_u")] <- 5/11*tab_year_fem[tab_year_fem$Age.Group=="13-17",c("year_fem","ci_l","ci_u")] +  
                                                                              6/11*tab_year_fem[tab_year_fem$Age.Group=="18-24",c("year_fem","ci_l","ci_u")]
##############################################################
#MSM [prep_msm2 include AI only, not VI]
svyby(~prep_msm2, nhns_all, by = ~age_msm, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age_msm) %>%
  as_tibble() %>% 
  rename(Age.Group = age_msm) %>%
  add_row(Age.Group= "45-54") %>%
  add_row(Age.Group= "55+") %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3) -> tab_msm
#  mutate(Age.Group = ifelse(Age.Group=="18-24", "13-24", Age.Group)) -> tab_msm

tab_msm[tab_msm$Age.Group=="13-17",2:4] <- tab_msm[tab_msm$Age.Group=="18-24",2:4]
tab_msm[tab_msm$Age.Group=="13-24",2:4] <- tab_msm[tab_msm$Age.Group=="18-24",2:4]

# unstable older age group estimates -> aggregate at higher level
tab_msm[tab_msm$Age.Group=="45-54" | tab_msm$Age.Group=="55+", c("prep_msm2","ci_l","ci_u")] <- tab_msm[tab_msm$Age.Group=="45+", c("prep_msm2","ci_l","ci_u")]

# nhns %>%
#   #  filter(!is.na(prep_msm2)) %>%
#   group_by(prep_msm2, age_msm) %>%
#   summarise(n=n()) %>%
#   spread(prep_msm2, n) %>%
#   knitr::kable()

tab_msm %>%
  rename(year_msm = prep_msm2) %>%
  mutate(year_msm = 1,
         ci_l = 1, 
         ci_u = 1) -> tab_year_msm 


tab_year_msm[tab_year_msm$Age.Group=="13-17",2] <- 0.245
tab_year_msm[tab_year_msm$Age.Group=="13-17",3] <- 0.14
tab_year_msm[tab_year_msm$Age.Group=="13-17",4] <- 0.35
tab_year_msm[tab_year_msm$Age.Group=="13-24", c("year_msm","ci_l","ci_u")] <- 5/11*tab_year_msm[tab_year_msm$Age.Group=="13-17",c("year_msm","ci_l","ci_u")] +  
                                                                              6/11*tab_year_msm[tab_year_msm$Age.Group=="18-24",c("year_msm","ci_l","ci_u")]

#MSW
svyby(~prep_msw, nhns_all, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
  arrange(., age1) %>%
  as_tibble() %>% 
  rename(Age.Group = age1) %>%
  add_row(Age.Group= "13-17", .before = 1) %>%
  add_row(Age.Group= "13-24", .before = 3)  -> tab_msw
 # mutate(Age.Group = ifelse(Age.Group=="18-24", "13-24", Age.Group)) -> tab_msw

tab_msw[tab_msw$Age.Group=="13-17",2:4] <- tab_msw[tab_msw$Age.Group=="18-24",2:4]
tab_msw[tab_msw$Age.Group=="13-24",2:4] <- tab_msw[tab_msw$Age.Group=="18-24",2:4]


svyby(~year_msw, nhns_all, by = ~age1, svyciprop,  vartype="ci", method="logit") %>%
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


# MSM
# standard
pop.msm <- cbind(pop.msm, statefun(pop.msm, rr_re_msm), statefun_min(pop.msm, rr_re_msm), statefun_max(pop.msm, rr_re_msm))
# pop.msm <-  cbind(pop.msm, prepstatefun(pop.msm, rr_re_msm))


# MSW
pop.msw <- cbind(pop.msw, statefun(pop.msw, rr_re_msw), statefun_min(pop.msw, rr_re_msw), statefun_max(pop.msw, rr_re_msw))
# pop.msw <-  cbind(pop.msw, prepstatefun(pop.msw, rr_re_msw))


# WSM
pop.wsm <- cbind(pop.wsm, statefun(pop.wsm, rr_re_wsm), statefun_min(pop.wsm, rr_re_wsm), statefun_max(pop.wsm, rr_re_wsm))
# pop.wsm <-  cbind(pop.wsm, prepstatefun(pop.wsm, rr_re_wsm))


  
  
 
