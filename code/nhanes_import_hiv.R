

# install.packages("SASxport")
# require(SASxport)
# library(foreign)
# to read downloaded files:
# DEMO <- read.xport("SurveyData\\DEMO_I.XPT")


#install.packages("nhanesA")

# Within the CDC website, NHANES data are available in 5 categories
# Demographics (DEMO)
# Dietary (DIET)
# Examination (EXAM)
# Laboratory (LAB)
# Questionnaire (Q)

# nhanesTables(data_group='DEMO', year=1999)
# nhanesTables(data_group='DEMO', year=2015)
# 
# nhanesTables(data_group='DEMO', year=2018)
# nhanesTables(data_group='LAB', year=2016)
# nhanesTables(data_group='Q', year=2018)
# 

# library(survey)
# library(nhanesA)
# library(labelled)

# source("~/prep_denominator/code/nhanes_import_funs.R")

# names(demo)
# browseNHANES(year = 2016)

# cycle18_19 <- cycle_impmerge_fun("DEMO_K_R", "UCPREG_K_R", "ALQ_K_R", "RHQ_K_R",
#                                  "HIV_K_R", FALSE)

# cycle17_18 <- cycle_impmerge_fun("DEMO_J", "UCPREG_J", "SXQ_J", "RHQ_J",
#                                  "HIV_J", FALSE)

cycle15_16 <- cycle_impmerge_fun("DEMO_I", "UCPREG_I", "SXQ_I", "RHQ_I", "HSQ_I",
                                   "HIV_I", TRUE, "CHLMDA_I", "HSV_I", "TRICH_I")

cycle13_14 <- cycle_impmerge_fun("DEMO_H", "UCPREG_H", "SXQ_H", "RHQ_H", "HSQ_H",
                                 "HIV_H", TRUE, "CHLMDA_H", "HSV_H", "TRICH_H")
# 
cycle11_12 <- cycle_impmerge_fun("DEMO_G", "UCPREG_G", "SXQ_G", "RHQ_G", "HSQ_G",
                                 "HIV_G", TRUE, "CHLMDA_G", "HSV_G", "HEPB_S_G") # n.b. no trich, replaced with hep-B

cycle09_10 <- cycle_impmerge_fun("DEMO_F", "UCPREG_F", "SXQ_F", "RHQ_F", "HSQ_F",
                                 "HIV_F", TRUE, "CHLMDA_F", "HSV_F", "HEPB_S_F") # n.b. no trich, replaced with hep-B

cycle07_08 <- cycle_impmerge_fun("DEMO_E", "UCPREG_E", "SXQ_E", "RHQ_E", "HSQ_E",
                                 "HIV_E", TRUE, "CHLMDA_E", "HSV_E", "HEPB_S_E") # n.b. no trich, replaced with hep-B

cycle05_06 <- cycle_impmerge_fun("DEMO_D", "UCPREG_D", "SXQ_D", "RHQ_D", "HSQ_D",
                                 "HIV_D", TRUE, "CHLMDA_D", "HSV_D", "HEPC_D") # n.b. no trich, replaced with hep-C

cycle03_04 <- cycle_impmerge_fun("DEMO_C", "UC_C", "SXQ_C", "RHQ_C", "HSQ_C",
                                 "L03_C", TRUE, "L05_C", "l09_c", "L37SWA_C") # n.b. no trich, replaced with hep-C

cycle01_02 <- cycle_impmerge_fun("DEMO_B", "UC_B", "SXQ_B", "RHQ_B", "HSQ_B",
                                 "L03_B", TRUE, "L05_B", "l09_b", "l34_b") # n.b. no trich, replaced with hep-C

cycle99_00 <- cycle_impmerge_fun("DEMO", "UC", "SXQ", "RHQ", "HSQ",
                                 "LAB03", TRUE, "LAB05", "lab09", "Lab02") # n.b. no trich, replaced with hep-C

 cycle99_00[[1]] %>%
   bind_rows(., cycle01_02[[1]]) %>% 
   bind_rows(., cycle03_04[[1]]) %>% 
   bind_rows(., cycle05_06[[1]]) %>%
   bind_rows(., cycle07_08[[1]]) %>% 
   bind_rows(., cycle09_10[[1]]) %>%
   bind_rows(., cycle11_12[[1]]) %>% 
   bind_rows(., cycle13_14[[1]]) %>%
   bind_rows(., cycle15_16[[1]]) -> nhns 

# store the vars and labels from each cycle
nhns.labs <- list(cycle09_10[[2]], cycle11_12[[2]], cycle13_14[[2]], cycle15_16[[2]])

nhns$year[nhns$SDDSRVYR ==1] <- 2000
nhns$year[nhns$SDDSRVYR ==2] <- 2002
nhns$year[nhns$SDDSRVYR ==3] <- 2004
nhns$year[nhns$SDDSRVYR ==4] <- 2006
nhns$year[nhns$SDDSRVYR ==5] <- 2008
nhns$year[nhns$SDDSRVYR ==6] <- 2010
nhns$year[nhns$SDDSRVYR ==7] <- 2012
nhns$year[nhns$SDDSRVYR ==8] <- 2014
nhns$year[nhns$SDDSRVYR ==9] <- 2016

nhns$year2[nhns$SDDSRVYR ==1] <- 2002
nhns$year2[nhns$SDDSRVYR ==2] <- 2002
nhns$year2[nhns$SDDSRVYR ==3] <- 2002
nhns$year2[nhns$SDDSRVYR ==4] <- 2007
nhns$year2[nhns$SDDSRVYR ==5] <- 2007
nhns$year2[nhns$SDDSRVYR ==6] <- 2011
nhns$year2[nhns$SDDSRVYR ==7] <- 2011
nhns$year2[nhns$SDDSRVYR ==8] <- 2015
nhns$year2[nhns$SDDSRVYR ==9] <- 2015


nhns$hivtest[nhns$HSQ590 == 1] <- 1
nhns$hivtest[nhns$HSQ590 == 2] <- 0

nhns$hivtestunk[nhns$HSQ590 == 1] <- 0
nhns$hivtestunk[nhns$HSQ590 == 2] <- 0
nhns$hivtestunk[nhns$HSQ590 == 9] <- 1
nhns$hivtestunk[nhns$HSQ590 == 7] <- 1

nhns$hiv[nhns$LBDHI == 1] <- 1
nhns$hiv[nhns$LBDHI == 2] <- 0

nhns$hiv[nhns$LBXHIVC == 1] <- 1
nhns$hiv[nhns$LBXHIVC == 2] <- 0

nhns$gc[nhns$URXUGC==1] <- 1
nhns$gc[nhns$URXUGC==2] <- 0

nhns$ct[nhns$URXUCL==1] <- 1
nhns$ct[nhns$URXUCL==2] <- 0

# N.B. there are a few responses as 7 (refused) and 9 (dont know) - these are now NA
nhns$ctd[nhns$SXQ272==1] <- 1
nhns$ctd[nhns$SXQ272==2] <- 0

# N.B. there are a few responses as 7 (refused) and 9 (dont know) - these are now NA
nhns$gcd[nhns$SXQ270==1] <- 1
nhns$gcd[nhns$SXQ270==2] <- 0


# recode race/ethnicity
nhns$race[nhns$RIDRETH1 == 3] <- "white"
nhns$race[nhns$RIDRETH1 == 1 | nhns$RIDRETH1 == 2] <- "hispanic"
nhns$race[nhns$RIDRETH1 == 4] <- "black"
nhns$race[nhns$RIDRETH1 == 5] <- "other"

# recode gender
nhns$gender[nhns$RIAGENDR == 1] <- "male"
nhns$gender[nhns$RIAGENDR == 2] <- "female"

# recode age
nhns$age1[nhns$RIDAGEYR < 18] <- NA
nhns$age1[nhns$RIDAGEYR > 17 & nhns$RIDAGEYR < 25] <- "18-24"
nhns$age1[nhns$RIDAGEYR > 24 & nhns$RIDAGEYR < 35] <- "25-34"
nhns$age1[nhns$RIDAGEYR > 34 & nhns$RIDAGEYR < 45] <- "35-44"
nhns$age1[nhns$RIDAGEYR > 44 & nhns$RIDAGEYR < 55] <- "45-54"
nhns$age1[nhns$RIDAGEYR > 54 ] <- "55+" 

# recode age -- 
nhns$age3[nhns$RIDAGEYR < 18] <- NA
nhns$age3[nhns$RIDAGEYR > 17 & nhns$RIDAGEYR < 25] <- "18-24"
nhns$age3[nhns$RIDAGEYR > 24 & nhns$RIDAGEYR < 35] <- "25-34"
nhns$age3[nhns$RIDAGEYR > 34 & nhns$RIDAGEYR < 45] <- "35-44"
nhns$age3[nhns$RIDAGEYR > 44 & nhns$RIDAGEYR < 55] <- "45-54"
nhns$age3[nhns$RIDAGEYR > 54 & nhns$RIDAGEYR < 65 ] <- "55-64"
nhns$age3[nhns$RIDAGEYR > 64 & nhns$RIDAGEYR < 75 ] <- "65-74"
nhns$age3[ nhns$RIDAGEYR > 74 ] <- "75+"

# recode age for msm
nhns$age_msm[nhns$RIDAGEYR < 18] <- NA
nhns$age_msm[nhns$RIDAGEYR > 17 & nhns$RIDAGEYR < 25] <- "18-24"
nhns$age_msm[nhns$RIDAGEYR > 24 & nhns$RIDAGEYR < 35] <- "25-34"
nhns$age_msm[nhns$RIDAGEYR > 34 & nhns$RIDAGEYR < 45] <- "35-44"
nhns$age_msm[nhns$RIDAGEYR > 44 ] <- "45+"

nhns$age2[nhns$RIDAGEYR < 45] <- "18-44"
nhns$age2[nhns$RIDAGEYR > 44 ] <- "45-59"

# recode MSM/MSW self-identified orientation 18+ only
nhns$msm[nhns$SXQ296 == 2] <- "msw"
nhns$msm[nhns$SXQ296 == 1 | nhns$SXQ296 == 3] <- "msm"
nhns$msm[nhns$SXQ296 == 4 | nhns$SXQ296 == 6  | nhns$SXQ296 == 7  | nhns$SXQ296 == 9] <- "oth"


## Ever sex w/ man (any kind) 
nhns$ever_msm[nhns$SXQ809 == 1] <- 1
nhns$ever_msm[nhns$SXQ809 == 2] <- 0

## past year male partner (anal or oral) -- DEFIITION OF MSM sex with men in the past year
## DENOMINATOR FOR MSM
nhns$year_msm[nhns$SXQ450 > 0 &  nhns$SXQ450 < 7777 ] <- 1
nhns$year_msm[nhns$SXQ450 == 0] <- 0

nhns$year_msm[nhns$SXQ550 > 0 &  nhns$SXQ550 < 7777 ] <- 1
nhns$year_msm[nhns$SXQ550 == 0] <- 0

## for testing data -- 
# 2005-2006, 2007-2008In your lifetime, with how many men have you had anal or oral sex?
nhns$ever_msm[nhns$SXQ410 > 0 & nhns$SXQ410 < 7777] <- 1
nhns$ever_msm[nhns$SXQ410 == 0] <- 0

# 2009-2010 onwards 
# In your lifetime, with how many men have you had anal sex?
nhns$ever_msm[nhns$SXQ836 > 0 & nhns$SXQ836 < 7777] <- 1
nhns$ever_msm[nhns$SXQ836 == 0] <- 0

## sex and sexual orientation combined
nhns$sexorient[nhns$gender == "female"] <- "women"
nhns$sexorient[nhns$gender == "male"] <- "msw"
nhns$sexorient[nhns$year_msm==1] <- "msm"

## broader MSM definition
nhns$sexorient2[nhns$gender == "female"] <- "women"
nhns$sexorient2[nhns$gender == "male"] <- "msw"
nhns$sexorient2[nhns$ever_msm==1] <- "msm"

## DENOMINATOR FOR WOMEN
nhns$year_fem[nhns$SXD450 > 0 &  nhns$SXD450 < 7777 ] <- 1
nhns$year_fem[nhns$SXD450 == 0] <- 0

## DENOMINATOR FOR MSW, defined so no male partners in past 12 months
nhns$year_msw[nhns$SXD510 > 0 &  nhns$SXD510 < 7777 ] <- 1
nhns$year_msw[nhns$SXD510 == 0 & (nhns$SXQ550 == 0 | is.na(nhns$SXQ550)) ] <- 0
# exclude MSM from the MSW variable
nhns$year_msw[!is.na(nhns$year_msm)] <- NA

# number of partners for VI partner
# there is also lifetime, shoudl check this is < than past year
# criteria asks for 1 or more partners in the past 6 months, 
# here utilized >1 partner in 12 m
nhns$yr_f_vi[nhns$SXQ727 > 1 & nhns$SXQ727 < 7777 ] <- 1
nhns$yr_f_vi[nhns$SXQ727 < 2] <- 0

nhns$yr_f_2vi[nhns$SXQ727 > 2 & nhns$SXQ727 < 7777 ] <- 1
nhns$yr_f_2vi[nhns$SXQ727 < 3] <- 0

nhns$yr_m_vi[nhns$SXD510 > 1 & nhns$SXD510 < 7777] <- 1
nhns$yr_m_vi[nhns$SXD510 < 2] <- 0

nhns$yr_m_2vi[nhns$SXD510 > 2 & nhns$SXD510 < 7777] <- 1
nhns$yr_m_2vi[nhns$SXD510 < 3] <- 0

## Prop of WOM with >1  VI partners
nhns$yr_fem_vi_pr[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$yr_fem_vi_pr[nhns$year_fem ==1 & nhns$hiv ==0 & nhns$yr_f_vi==1] <- 1

# 2 partners
nhns$yr_fem_2vi_pr[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$yr_fem_2vi_pr[nhns$year_fem ==1 & nhns$hiv ==0 & nhns$yr_f_2vi==1] <- 1

# Prop of MSW with >1 VI partners and no male partners!
nhns$yr_msw_vi_pr[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$yr_msw_vi_pr[nhns$year_msw ==1 & nhns$hiv ==0 & nhns$yr_m_vi==1] <- 1

nhns$yr_msw_2vi_pr[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$yr_msw_2vi_pr[nhns$year_msw ==1 & nhns$hiv ==0 & nhns$yr_m_2vi==1] <- 1

## New sex partners in past year
nhns$yr_fem_newpart[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$yr_fem_newpart[nhns$year_fem ==1 & nhns$hiv ==0 & nhns$SXQ648==1] <- 1

nhns$yr_msw_newpart[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$yr_msw_newpart[nhns$year_msw ==1 & nhns$hiv ==0 & nhns$SXQ648==1] <- 1

## combined CT or GC
nhns$dgcct[nhns$ctd==1 | nhns$gcd==1] <- 1
nhns$dgcct[nhns$ctd==0 & nhns$gcd==0] <- 0

## Prop of HIV- MSM with GC or CT diagnosis
nhns$yr_msm_dgcct_pr[nhns$year_msm ==1 & nhns$hiv ==0] <- 0
nhns$yr_msm_dgcct_pr[nhns$year_msm ==1 & nhns$hiv ==0 & nhns$dgcct==1] <- 1

## Women GC only
nhns$yr_fem_dgc_pr[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$yr_fem_dgc_pr[nhns$year_fem ==1 & nhns$hiv ==0 & nhns$gcd==1] <- 1

## MSW GC only
nhns$yr_msw_dgc_pr[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$yr_msw_dgc_pr[nhns$year_msw ==1 & nhns$hiv ==0 & nhns$gcd==1] <- 1

# prev of MSM among all men of similar age
nhns$msm_pr[nhns$gender == "male"  & nhns$RIDAGEYR > 17 & nhns$RIDAGEYR < 60] <- 0
nhns$msm_pr[nhns$year_msm == 1] <- 1

# prev of WOM among all men of similar age
nhns$fem_pr[nhns$gender == "female"  & nhns$RIDAGEYR > 17 & nhns$RIDAGEYR < 60] <- 0
nhns$fem_pr[nhns$year_fem == 1] <- 1

# prev of MSW among all men of similar age
nhns$msw_pr[nhns$gender == "male"  & nhns$RIDAGEYR > 17 & nhns$RIDAGEYR < 60] <- 0
nhns$msw_pr[nhns$year_msw == 1] <- 1

########################
##  MSM
nhns$aioyr_msm[nhns$SXQ550 > 1 &  nhns$SXQ550 < 7777] <- 1
nhns$aioyr_msm[nhns$SXQ550 < 2 ] <- 0

nhns$yr_msm_ai[nhns$SXQ841 > 1 &  nhns$SXQ550 < 7777] <- 1
nhns$yr_msm_ai[nhns$SXQ841 < 2 ] <- 0

nhns$yr_msm_2ai[nhns$SXQ841 > 2 &  nhns$SXQ550 < 7777] <- 1
nhns$yr_msm_2ai[nhns$SXQ841 < 3 ] <- 0

## Prop of HIV- MSM with >1 AI partners
nhns$yr_msm_ai_pr[nhns$year_msm ==1 & nhns$hiv ==0] <- 0
nhns$yr_msm_ai_pr[nhns$year_msm ==1 & nhns$hiv ==0 & nhns$yr_msm_ai==1] <- 1

nhns$yr_msm_2ai_pr[nhns$year_msm ==1 & nhns$hiv ==0] <- 0
nhns$yr_msm_2ai_pr[nhns$year_msm ==1 & nhns$hiv ==0 & nhns$yr_msm_2ai==1] <- 1

# msm AI plus any male VI 
nhns$sum_ai_vi <- rowSums(nhns[,c("SXQ841", "SXD510")], na.rm=TRUE)

# select msm only
nhns$sum_msm_ai_vi <- ifelse(nhns$year_msm ==1, nhns$sum_ai_vi, NA)

nhns$yr_msm_ai_vi[nhns$sum_msm_ai_vi > 1 & nhns$sum_msm_ai_vi < 7777] <- 1
nhns$yr_msm_ai_vi[nhns$sum_msm_ai_vi < 2] <- 0

## Prop of HIV- MSM with >1 AI and VI partners
nhns$yr_msm_ai_vi_pr[nhns$year_msm ==1 & nhns$hiv ==0] <- 0
nhns$yr_msm_ai_vi_pr[nhns$year_msm ==1 & nhns$hiv ==0 & nhns$yr_msm_ai_vi==1] <- 1

## use condoms always (Q: had sex without condom) 
nhns$condom[nhns$SXQ251 == 1]  <- 1 #never
nhns$condom[nhns$SXQ251 == 2 | nhns$SXQ251 == 3 | nhns$SXQ251 == 4 | nhns$SXQ251 == 5]  <- 0 # always, less than half tt, about half tt, not always but more than half tt


## use condoms at least half the time (Q: had sex without condom) 
nhns$condom2[nhns$SXQ251 == 1 | nhns$SXQ251 == 2 ]  <- 1 #never, about half tt, 
nhns$condom2[ nhns$SXQ251 == 4 | nhns$SXQ251 == 5 | nhns$SXQ251 == 3]  <- 0 # always, less than half tt, not always but more than half tt


## Prop of HIV- who does not always use condom
nhns$condom_msm[nhns$year_msm ==1 & nhns$hiv ==0] <- 0
nhns$condom_msm[nhns$year_msm ==1 & nhns$hiv ==0 & nhns$condom==0] <- 1

nhns$condom_fem[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$condom_fem[nhns$year_fem ==1 & nhns$hiv ==0 & nhns$condom==0] <- 1

nhns$condom_msw[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$condom_msw[nhns$year_msw ==1 & nhns$hiv ==0 & nhns$condom==0] <- 1

nhns$condom_msm2[nhns$year_msm ==1 & nhns$hiv ==0] <- 0
nhns$condom_msm2[nhns$year_msm ==1 & nhns$hiv ==0 & nhns$condom2==0] <- 1

nhns$condom_fem2[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$condom_fem2[nhns$year_fem ==1 & nhns$hiv ==0 & nhns$condom2==0] <- 1

nhns$condom_msw2[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$condom_msw2[nhns$year_msw ==1 & nhns$hiv ==0 & nhns$condom2==0] <- 1


## SEXUALLY ACTIVE PEOPLE NEEDING PREP
#nhns$prep_msm[!is.na(nhns$nhns$aioyr_msm) & !is.na(nhns$condom) & !is.na(nhns$gcd) ] <- 0
nhns$prep_msm[nhns$year_msm ==1 & nhns$hiv ==0  & nhns$year>2008] <- 0
nhns$prep_msm[nhns$year_msm ==1 & nhns$hiv ==0  & nhns$yr_msm_ai_vi ==1 & (nhns$condom==0 | nhns$dgcct==1)  & nhns$year>2008] <- 1

nhns$prep_msm2[nhns$year_msm ==1 & nhns$hiv ==0  & nhns$year>2008] <- 0
nhns$prep_msm2[nhns$year_msm ==1 & nhns$hiv ==0  & nhns$yr_msm_ai ==1 & (nhns$condom==0 | nhns$dgcct==1)  & nhns$year>2008] <- 1

nhns$prep_msm3[nhns$year_msm ==1 & nhns$hiv ==0  & nhns$year>2008] <- 0
nhns$prep_msm3[nhns$year_msm ==1 & nhns$hiv ==0  & nhns$yr_msm_2ai ==1 & (nhns$condom2==0 | nhns$dgcct==1)  & nhns$year>2008] <- 1

nhns$prep_msm_nogcd[nhns$year_msm ==1 & nhns$hiv ==0  & nhns$year>2008] <- 0
nhns$prep_msm_nogcd[nhns$year_msm ==1 & nhns$hiv ==0  & nhns$yr_msm_ai_vi ==1 & (nhns$condom==0)  & nhns$year>2008] <- 1

# original
# nhns$prep_msm[nhns$year_msm ==1 & nhns$hiv ==0] <- 0
# nhns$prep_msm[nhns$year_msm ==1 & nhns$hiv ==0  & nhns$aioyr_msm ==1 & (nhns$condom==0 | nhns$dgcct==1)] <- 1
# nhns$prep_msm[nhns$hiv ==1 ] <- NA

# nhns$prep_msm[nhns$year_msm] <- 0
# nhns$prep_msm[ nhns$aioyr_msm==1 & (nhns$condom==0 | nhns$dgcct==1)] <- 1


# more than 2 partner FEM
nhns$prep_fem[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$prep_fem[nhns$year_fem ==1 & nhns$hiv ==0  & nhns$yr_fem_vi ==1 & (nhns$condom==0 | nhns$gcd==1)] <- 1

nhns$prep_fem2[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$prep_fem2[nhns$year_fem ==1 & nhns$hiv ==0  & nhns$yr_fem_vi ==1 & nhns$yr_fem_newpart ==1 & (nhns$condom==0 | nhns$gcd==1)] <- 1

nhns$prep_fem3[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$prep_fem3[nhns$year_fem ==1 & nhns$hiv ==0  & nhns$yr_fem_2vi ==1 & (nhns$condom2==0 | nhns$gcd==1)] <- 1

nhns$prep_fem_nogcd[nhns$year_fem ==1 & nhns$hiv ==0] <- 0
nhns$prep_fem_nogcd[nhns$year_fem ==1 & nhns$hiv ==0  & nhns$yr_fem_vi ==1 & (nhns$condom==0 )] <- 1

# more than 2 partner MSW
nhns$prep_msw[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$prep_msw[nhns$year_msw ==1 & nhns$hiv ==0  & nhns$yr_msw_vi ==1 & (nhns$condom==0 | nhns$gcd==1)] <- 1

nhns$prep_msw2[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$prep_msw2[nhns$year_msw ==1 & nhns$hiv ==0  & nhns$yr_msw_vi ==1 & nhns$yr_msw_newpart ==1 & (nhns$condom==0 | nhns$gcd==1)] <- 1

nhns$prep_msw3[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$prep_msw3[nhns$year_msw ==1 & nhns$hiv ==0  & nhns$yr_msw_2vi ==1 & (nhns$condom2==0 | nhns$gcd==1)] <- 1

nhns$prep_msw_nogcd[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$prep_msw_nogcd[nhns$year_msw ==1 & nhns$hiv ==0  & nhns$yr_msw_vi ==1 & (nhns$condom==0 )] <- 1

nhns$prep_msw[nhns$year_msw ==1 & nhns$hiv ==0] <- 0
nhns$prep_msw[nhns$year_msw ==1 & nhns$hiv ==0  & nhns$yr_msw_vi ==1 & (nhns$condom==0 | nhns$gcd==1)] <- 1

## combined
nhns$prep_all[nhns$prep_fem==1 | nhns$prep_msm2==1 | nhns$prep_msw==1] <- 1
nhns$prep_all[nhns$prep_fem==0 | nhns$prep_msm2==0 | nhns$prep_msw==0] <- 0

nhns$prep_all2[nhns$prep_fem2==1 | nhns$prep_msm2==1 | nhns$prep_msw2==1] <- 1
nhns$prep_all2[nhns$prep_fem2==0 | nhns$prep_msm2==0 | nhns$prep_msw2==0] <- 0

nhns$prep_all3[nhns$prep_fem3==1 | nhns$prep_msm3==1 | nhns$prep_msw3==1] <- 1
nhns$prep_all3[nhns$prep_fem3==0 | nhns$prep_msm3==0 | nhns$prep_msw3==0] <- 0

nhns$prep_all_nogcd[nhns$prep_fem_nogcd==1 | nhns$prep_msm_nogcd==1 | nhns$prep_msw_nogcd==1] <- 1
nhns$prep_all_nogcd[nhns$prep_fem_nogcd==0 | nhns$prep_msm_nogcd==0 | nhns$prep_msw_nogcd==0] <- 0


nhns$prep_all_nogcd[nhns$prep_fem_nogcd==1 | nhns$prep_msm_nogcd==1 | nhns$prep_msw_nogcd==1] <- 1
nhns$prep_all_nogcd[nhns$prep_fem_nogcd==0 | nhns$prep_msm_nogcd==0 | nhns$prep_msw_nogcd==0] <- 0
