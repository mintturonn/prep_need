

# nhanes manually done (used)
source(here('code/acs_fun.R'))

pop_re_2015  <- pop_re_fun(2015, "2008-2014") 
pop_re_2019  <- pop_re_fun(2019, "2015-2020") 

pivot_longer(pop_re_2019$f, cols = c("13-17", "18-24", "13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "Age.Group", values_to = "acs_pop") %>% 
  # bind_rows(., pivot_longer(pop_re_2015$m, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "Age.Group", values_to = "acs_pop")) %>%
  # bind_rows(., pivot_longer(pop_re_2019$f, cols = c("13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "Age.Group", values_to = "acs_pop")) %>%
  bind_rows(., pivot_longer(pop_re_2019$m, cols = c("13-17", "18-24", "13-24", "25-34", "35-44", "45-54", "55+" ), names_to = "Age.Group", values_to = "acs_pop")) %>%
  filter(Race.Ethnicity != "White_any")-> pop_re_long

##################################

pop.msm %>%
  dplyr::select(state, prev, Sex, Age.Group, `AI/AN`, Asian, Black, Hispanic, `NH/PI`, White) %>%
  rename("American Indian/Alaska Native"=`AI/AN`) %>%
  rename("Native Hawaiian/Other Pacific Islander"=`NH/PI`) %>%
  rename("Hispanic/Latino"=`Hispanic`) %>%
  rename("Black/African American"=`Black`) %>%
  pivot_longer(cols = c("American Indian/Alaska Native", "Asian", "Black/African American", 
                        "Hispanic/Latino", "Native Hawaiian/Other Pacific Islander", "White"), 
               names_to = "Race.Ethnicity", values_to= "incid") %>%
  left_join(msmpr, by = ("state") ) %>%
  left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity")) %>%
  mutate(prep_msm_cdc = 0.247 *prev_extract * acs_pop) %>%
  mutate(pop_msm = (1-prev)* prev_extract * acs_pop) %>% #hiv negative population * msm population * male population
  left_join(., tab_msm, by = "Age.Group") %>%
  left_join(., tab_year_msm, by = "Age.Group") %>%
  mutate(prep_100     = pop_msm * prep_msm2 *year_msm ,
         prep_100_min = pop_msm * ci_l.x * ci_l.y,
         prep_100_max = pop_msm * ci_u.x * ci_l.y)  -> pop.msm.prepnd

pop.msm %>%
  dplyr::select(state, prev, Sex, Age.Group, `AI/AN_min`, Asian_min, Black_min, Hispanic_min, `NH/PI_min`, White_min) %>%
  rename("American Indian/Alaska Native"=`AI/AN_min`) %>%
  rename("Native Hawaiian/Other Pacific Islander"=`NH/PI_min`) %>%
  rename("Hispanic/Latino"=`Hispanic_min`) %>%
  rename("Black/African American"=`Black_min`) %>%
  rename("Asian"= `Asian_min`) %>%
  rename("White"= `White_min`) %>%
  pivot_longer(cols = c("American Indian/Alaska Native", "Asian", "Black/African American", 
                        "Hispanic/Latino", "Native Hawaiian/Other Pacific Islander", "White"), 
               names_to = "Race.Ethnicity", values_to= "incid") %>%
  left_join(msmpr, by = ("state") ) %>%
  left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity")) %>%
  mutate(pop_msm = (1-prev)* prev_extract * acs_pop) %>% #hiv negative population * msm population * male population
  left_join(., tab_msm, by = "Age.Group") %>%
  mutate(prep_100 = pop_msm * prep_msm2)  -> pop.msm.prepnd_min

pop.msm %>%
  dplyr::select(state, prev, Sex, Age.Group, `AI/AN_max`, Asian_max, Black_max, Hispanic_max, `NH/PI_max`, White_max) %>%
  rename("American Indian/Alaska Native"=`AI/AN_max`) %>%
  rename("Native Hawaiian/Other Pacific Islander"=`NH/PI_max`) %>%
  rename("Hispanic/Latino"=`Hispanic_max`) %>%
  rename("Black/African American"=`Black_max`) %>%
  rename("Asian"= `Asian_max`) %>%
  rename("White"= `White_max`) %>%
  pivot_longer(cols = c("American Indian/Alaska Native", "Asian", "Black/African American", 
                        "Hispanic/Latino", "Native Hawaiian/Other Pacific Islander", "White"), 
               names_to = "Race.Ethnicity", values_to= "incid") %>%
  left_join(msmpr, by = ("state") ) %>%
  left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity")) %>%
  mutate(pop_msm = (1-prev)* prev_extract * acs_pop) %>% #hiv negative population * msm population * male population
  left_join(., tab_msm, by = "Age.Group") %>%
  mutate(prep_100 = pop_msm * prep_msm2)  -> pop.msm.prepnd_max

## proportions
pop.msm.prepnd$prep_grad_pr[pop.msm.prepnd$incid >= 1.09e-04 ] <- 1
pop.msm.prepnd$prep_grad_pr[pop.msm.prepnd$incid >= 5.8e-05 & pop.msm.prepnd$incid < 1.09e-04 ] <- 0.9
pop.msm.prepnd$prep_grad_pr[pop.msm.prepnd$incid >= 3.7e-05 & pop.msm.prepnd$incid < 5.8e-05 ] <- 0.8
pop.msm.prepnd$prep_grad_pr[pop.msm.prepnd$incid >= 2.3e-05 & pop.msm.prepnd$incid < 3.7e-05 ] <- 0.7
pop.msm.prepnd$prep_grad_pr[pop.msm.prepnd$incid >= 1.3e-05 & pop.msm.prepnd$incid < 2.3e-05 ] <- 0.6
pop.msm.prepnd$prep_grad_pr[pop.msm.prepnd$incid >= 7.6e-06 & pop.msm.prepnd$incid < 1.3e-05 ] <- 0.5
pop.msm.prepnd$prep_grad_pr[pop.msm.prepnd$incid >= 3.4e-06 & pop.msm.prepnd$incid < 7.6e-06 ] <- 0.4
pop.msm.prepnd$prep_grad_pr[pop.msm.prepnd$incid >= 1.7e-07 & pop.msm.prepnd$incid < 3.4e-06 ] <- 0.3
pop.msm.prepnd$prep_grad_pr[ pop.msm.prepnd$incid < 1.7e-07 ] <- 0.2

pop.msm.prepnd$prep_step1_pr[pop.msm.prepnd$incid >= 36e-05 ] <- 1
pop.msm.prepnd$prep_step1_pr[pop.msm.prepnd$incid < 36e-05 ] <- 0.1
pop.msm.prepnd$prep_step1_pr_min[pop.msm.prepnd_min$incid >= 36e-05 ] <- 1
pop.msm.prepnd$prep_step1_pr_min[pop.msm.prepnd_min$incid < 36e-05 ] <- 0.1
pop.msm.prepnd$prep_step1_pr_max[pop.msm.prepnd_max$incid >= 36e-05 ] <- 1
pop.msm.prepnd$prep_step1_pr_max[pop.msm.prepnd_max$incid < 36e-05 ] <- 0.1

# pop.msm.prepnd$prep_step2 <- pop.msm.prepnd$prep_100* pop.msm.prepnd$prep_step2_pr
# pop.msm.prepnd$prep_step3 <- pop.msm.prepnd$prep_100* pop.msm.prepnd$prep_step3_pr

pop.msm.prepnd$prep_step1 <- pop.msm.prepnd$prep_100* pop.msm.prepnd$prep_step1_pr
pop.msm.prepnd$prep_grad <- pop.msm.prepnd$prep_100* pop.msm.prepnd$prep_grad_pr

#### 

## incidence
# prep_incid is the number of people, incid is the proportion
pop.msm.prepnd$prep_incid <- pop.msm.prepnd$prep_100* pop.msm.prepnd$incid
pop.msm.prepnd$prep_incid_min <- pop.msm.prepnd_min$prep_100* pop.msm.prepnd_min$incid
pop.msm.prepnd$prep_incid_max <- pop.msm.prepnd_max$prep_100* pop.msm.prepnd_max$incid

pop.msm.prepnd$prep_step1     <- pop.msm.prepnd$prep_100* pop.msm.prepnd$prep_step1_pr
pop.msm.prepnd$prep_step1_min <- pop.msm.prepnd_min$prep_100* pop.msm.prepnd$prep_step1_pr_min
pop.msm.prepnd$prep_step1_max <- pop.msm.prepnd_max$prep_100* pop.msm.prepnd$prep_step1_pr_max

##################################
pop.msw %>%
  select(state, prev, Age.Group, `AI/AN`, Asian, Black, Hispanic, `NH/PI`, White) %>% # this is the opp sex prev! 
  mutate(Sex = "Male") %>%
  rename("American Indian/Alaska Native"=`AI/AN`) %>%
  rename("Native Hawaiian/Other Pacific Islander"=`NH/PI`) %>%
  rename("Hispanic/Latino"=`Hispanic`) %>%
  rename("Black/African American"=`Black`) %>%
  pivot_longer(cols = c("American Indian/Alaska Native", "Asian", "Black/African American", 
                        "Hispanic/Latino", "Native Hawaiian/Other Pacific Islander", "White"), 
               names_to = "Race.Ethnicity", values_to= "incid") %>%
  left_join(msmpr, by = ("state") ) %>%
  left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity")) %>%
  mutate(pop_msw = (1-prev)*(1- prev_extract) * acs_pop) %>%
  left_join(., tab_msw, by = "Age.Group") %>%
  left_join(., tab_year_msw, by = "Age.Group") %>%
  mutate(prep_100 = pop_msw * year_msw * prep_msw, 
         prep_100_min = pop_msw * year_msw * ci_l.x,
         prep_100_max = pop_msw * year_msw * ci_u.x) -> pop.msw.prepnd
 # left_join(., prep_indic_msw, by = "Age.Group")

pop.msw %>%
  select(state, prev, Age.Group, `AI/AN_min`, Asian_min, Black_min, Hispanic_min, `NH/PI_min`, White_min) %>% # this is the opp sex prev! 
  mutate(Sex = "Male") %>%
  rename("American Indian/Alaska Native"=`AI/AN_min`) %>%
  rename("Native Hawaiian/Other Pacific Islander"=`NH/PI_min`) %>%
  rename("Hispanic/Latino"=`Hispanic_min`) %>%
  rename("Black/African American"=`Black_min`) %>%
  rename("Asian"= `Asian_min`) %>%
  rename("White"= `White_min`) %>%
  pivot_longer(cols = c("American Indian/Alaska Native", "Asian", "Black/African American", 
                        "Hispanic/Latino", "Native Hawaiian/Other Pacific Islander", "White"), 
               names_to = "Race.Ethnicity", values_to= "incid") %>%
  left_join(msmpr, by = ("state") ) %>%
  left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity")) %>%
  mutate(pop_msw = (1-prev)*(1- prev_extract) * acs_pop) %>%
  left_join(., tab_msw, by = "Age.Group") %>%
  left_join(., tab_year_msw, by = "Age.Group") %>%
  mutate(prep_100 = pop_msw * year_msw * prep_msw) -> pop.msw.prepnd_min

pop.msw %>%
  select(state, prev, Age.Group, `AI/AN_max`, Asian_max, Black_max, Hispanic_max, `NH/PI_max`, White_max) %>% # this is the opp sex prev! 
  mutate(Sex = "Male") %>%
  rename("American Indian/Alaska Native"=`AI/AN_max`) %>%
  rename("Native Hawaiian/Other Pacific Islander"=`NH/PI_max`) %>%
  rename("Hispanic/Latino"=`Hispanic_max`) %>%
  rename("Black/African American"=`Black_max`) %>%
  rename("Asian"= `Asian_max`) %>%
  rename("White"= `White_max`) %>%
  pivot_longer(cols = c("American Indian/Alaska Native", "Asian", "Black/African American", 
                        "Hispanic/Latino", "Native Hawaiian/Other Pacific Islander", "White"), 
               names_to = "Race.Ethnicity", values_to= "incid") %>%
  left_join(msmpr, by = ("state") ) %>%
  left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity")) %>%
  mutate(pop_msw = (1-prev)*(1- prev_extract) * acs_pop) %>%
  left_join(., tab_msw, by = "Age.Group") %>%
  left_join(., tab_year_msw, by = "Age.Group") %>%
  mutate(prep_100 = pop_msw * year_msw * prep_msw) -> pop.msw.prepnd_max

# proportions
# pop.msw.prepnd$prep_grad_pr[pop.msw.prepnd$incid >= 1.09e-04 ] <- 1
# pop.msw.prepnd$prep_grad_pr[pop.msw.prepnd$incid >= 5.8e-05 & pop.msw.prepnd$incid < 1.09e-04 ] <- 0.9
# pop.msw.prepnd$prep_grad_pr[pop.msw.prepnd$incid >= 3.7e-05 & pop.msw.prepnd$incid < 5.8e-05 ] <- 0.8
# pop.msw.prepnd$prep_grad_pr[pop.msw.prepnd$incid >= 2.3e-05 & pop.msw.prepnd$incid < 3.7e-05 ] <- 0.7
# pop.msw.prepnd$prep_grad_pr[pop.msw.prepnd$incid >= 1.3e-05 & pop.msw.prepnd$incid < 2.3e-05 ] <- 0.6
# pop.msw.prepnd$prep_grad_pr[pop.msw.prepnd$incid >= 7.6e-06 & pop.msw.prepnd$incid < 1.3e-05 ] <- 0.5
# pop.msw.prepnd$prep_grad_pr[pop.msw.prepnd$incid >= 3.4e-06 & pop.msw.prepnd$incid < 7.6e-06 ] <- 0.4
# pop.msw.prepnd$prep_grad_pr[pop.msw.prepnd$incid >= 1.7e-07 & pop.msw.prepnd$incid < 3.4e-06 ] <- 0.3
# pop.msw.prepnd$prep_grad_pr[pop.msw.prepnd$incid < 1.7e-07 ] <- 0.2
# 
# pop.msw.prepnd$prep_step2_pr[pop.msw.prepnd$incid >= 8e-05 ] <- 1
# pop.msw.prepnd$prep_step2_pr[pop.msw.prepnd$incid >= 1.8e-05 & pop.msw.prepnd$incid <  8e-05 ] <- 0.6
# pop.msw.prepnd$prep_step2_pr[pop.msw.prepnd$incid < 1.8e-05 ] <- 0.3
# pop.msw.prepnd$prep_step3_pr[pop.msw.prepnd$incid >= 1e-03 ] <- 1
# pop.msw.prepnd$prep_step3_pr[pop.msw.prepnd$incid < 1e-03 ] <- 0
# 
# pop.msw.prepnd$prep_grad <- pop.msw.prepnd$prep_100* pop.msw.prepnd$prep_grad_pr
# pop.msw.prepnd$prep_step1 <- pop.msw.prepnd$prep_100* pop.msw.prepnd$prep_step1_pr
# pop.msw.prepnd$prep_step2 <- pop.msw.prepnd$prep_100* pop.msw.prepnd$prep_step2_pr
# 
# pop.msw.prepnd$prep_step3 <- pop.msw.prepnd$prep_100* pop.msw.prepnd$prep_step3_pr

pop.msw.prepnd$prep_step1_pr[pop.msw.prepnd$incid >= 36e-05 ] <- 1
pop.msw.prepnd$prep_step1_pr[pop.msw.prepnd$incid < 36e-05 ] <- 0.1
pop.msw.prepnd$prep_step1_pr_min[pop.msw.prepnd_min$incid >= 36e-05 ] <- 1
pop.msw.prepnd$prep_step1_pr_min[pop.msw.prepnd_min$incid < 36e-05 ] <- 0.1
pop.msw.prepnd$prep_step1_pr_max[pop.msw.prepnd_max$incid >= 36e-05 ] <- 1
pop.msw.prepnd$prep_step1_pr_max[pop.msw.prepnd_max$incid < 36e-05 ] <- 0.1

## incidence
# prep_incid is the number of people, incid is the proportion
pop.msw.prepnd$prep_incid <- pop.msw.prepnd$prep_100* pop.msw.prepnd$incid
pop.msw.prepnd$prep_incid_min <- pop.msw.prepnd_min$prep_100* pop.msw.prepnd_min$incid
pop.msw.prepnd$prep_incid_max <- pop.msw.prepnd_max$prep_100* pop.msw.prepnd_max$incid

pop.msw.prepnd$prep_step1     <- pop.msw.prepnd$prep_100* pop.msw.prepnd$prep_step1_pr
pop.msw.prepnd$prep_step1_min <- pop.msw.prepnd_min$prep_100* pop.msw.prepnd$prep_step1_pr_min
pop.msw.prepnd$prep_step1_max <- pop.msw.prepnd_max$prep_100* pop.msw.prepnd$prep_step1_pr_max

##################################

pop.wsm %>%
  select(state, prev, Age.Group, `AI/AN`, Asian, Black, Hispanic, `NH/PI`, White) %>%
  mutate(Sex = "Female") %>%
  rename("American Indian/Alaska Native"=`AI/AN`) %>%
  rename("Native Hawaiian/Other Pacific Islander"=`NH/PI`) %>%
  rename("Hispanic/Latino"=`Hispanic`) %>%
  rename("Black/African American"=`Black`) %>%
  pivot_longer(cols = c("American Indian/Alaska Native", "Asian", "Black/African American", 
                        "Hispanic/Latino", "Native Hawaiian/Other Pacific Islander", "White"), 
               names_to = "Race.Ethnicity", values_to= "incid") %>%
  left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity"))  %>%
  mutate(pop_wsm = (1-prev) * acs_pop) %>%
  left_join(., tab_fem, by = "Age.Group") %>%
  left_join(., tab_year_fem, by = "Age.Group") %>%
  mutate(prep_100     = pop_wsm * year_fem * prep_fem, # prep 100% min and max only applied here (not in prep_min)
         prep_100_min = pop_wsm * year_fem * ci_l.x,
         prep_100_max = pop_wsm * year_fem * ci_u.x) -> pop.wsm.prepnd
  # left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity")) %>%
  # left_join(., prep_indic_wsm, by = "Age.Group") 

pop.wsm %>%
  select(state, prev, Age.Group, `AI/AN_min`, Asian_min, Black_min, Hispanic_min, `NH/PI_min`, White_min) %>%
  mutate(Sex = "Female") %>%
  rename("American Indian/Alaska Native"=`AI/AN_min`) %>%
  rename("Native Hawaiian/Other Pacific Islander"=`NH/PI_min`) %>%
  rename("Hispanic/Latino"=`Hispanic_min`) %>%
  rename("Black/African American"=`Black_min`) %>%
  rename("Asian"= `Asian_min`) %>%
  rename("White"= `White_min`) %>%
  pivot_longer(cols = c("American Indian/Alaska Native", "Asian", "Black/African American", 
                        "Hispanic/Latino", "Native Hawaiian/Other Pacific Islander", "White"), 
               names_to = "Race.Ethnicity", values_to= "incid") %>%
  left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity"))  %>%
  mutate(pop_wsm = (1-prev) * acs_pop) %>%
  left_join(., tab_fem, by = "Age.Group") %>%
  left_join(., tab_year_fem, by = "Age.Group") %>%
  mutate(prep_100 = pop_wsm * year_fem * prep_fem) -> pop.wsm.prepnd_min

pop.wsm %>%
  select(state, prev, Age.Group, `AI/AN_max`, Asian_max, Black_max, Hispanic_max, `NH/PI_max`, White_max) %>%
  mutate(Sex = "Female") %>%
  rename("American Indian/Alaska Native"=`AI/AN_max`) %>%
  rename("Native Hawaiian/Other Pacific Islander"=`NH/PI_max`) %>%
  rename("Hispanic/Latino"=`Hispanic_max`) %>%
  rename("Black/African American"=`Black_max`) %>%
  rename("Asian"= `Asian_max`) %>%
  rename("White"= `White_max`) %>%
  pivot_longer(cols = c("American Indian/Alaska Native", "Asian", "Black/African American", 
                        "Hispanic/Latino", "Native Hawaiian/Other Pacific Islander", "White"), 
               names_to = "Race.Ethnicity", values_to= "incid") %>%
  left_join(., pop_re_long, by = c("state", "Age.Group", "Sex", "Race.Ethnicity"))  %>%
  mutate(pop_wsm = (1-prev) * acs_pop) %>%
  left_join(., tab_fem, by = "Age.Group") %>%
  left_join(., tab_year_fem, by = "Age.Group") %>%
  mutate(prep_100 = pop_wsm * year_fem * prep_fem) -> pop.wsm.prepnd_max

## incidence
pop.wsm.prepnd$prep_incid <- pop.wsm.prepnd$prep_100* pop.wsm.prepnd$incid
pop.wsm.prepnd$prep_incid_min <- pop.wsm.prepnd_min$prep_100* pop.wsm.prepnd_min$incid
pop.wsm.prepnd$prep_incid_max <- pop.wsm.prepnd_max$prep_100* pop.wsm.prepnd_max$incid

## proportions
# step 1
pop.wsm.prepnd$prep_step1_pr[pop.wsm.prepnd$incid >= 36e-05 ] <- 1
pop.wsm.prepnd$prep_step1_pr[pop.wsm.prepnd$incid < 36e-05 ] <- 0.1
pop.wsm.prepnd$prep_step1_pr_min[pop.wsm.prepnd_min$incid >= 36e-05 ] <- 1
pop.wsm.prepnd$prep_step1_pr_min[pop.wsm.prepnd_min$incid < 36e-05 ] <- 0.1
pop.wsm.prepnd$prep_step1_pr_max[pop.wsm.prepnd_max$incid >= 36e-05 ] <- 1
pop.wsm.prepnd$prep_step1_pr_max[pop.wsm.prepnd_max$incid < 36e-05 ] <- 0.1

pop.wsm.prepnd$prep_grad_pr[pop.wsm.prepnd$incid >= 1.09e-04 ] <- 1
pop.wsm.prepnd$prep_grad_pr[pop.wsm.prepnd$incid >= 5.8e-05 & pop.wsm.prepnd$incid < 1.09e-04 ] <- 0.9
pop.wsm.prepnd$prep_grad_pr[pop.wsm.prepnd$incid >= 3.7e-05 & pop.wsm.prepnd$incid < 5.8e-05 ] <- 0.8
pop.wsm.prepnd$prep_grad_pr[pop.wsm.prepnd$incid >= 2.3e-05 & pop.wsm.prepnd$incid < 3.7e-05 ] <- 0.7
pop.wsm.prepnd$prep_grad_pr[pop.wsm.prepnd$incid >= 1.3e-05 & pop.wsm.prepnd$incid < 2.3e-05 ] <- 0.6
pop.wsm.prepnd$prep_grad_pr[pop.wsm.prepnd$incid >= 7.6e-06 & pop.wsm.prepnd$incid < 1.3e-05 ] <- 0.5
pop.wsm.prepnd$prep_grad_pr[pop.wsm.prepnd$incid >= 3.4e-06 & pop.wsm.prepnd$incid < 7.6e-06 ] <- 0.4
pop.wsm.prepnd$prep_grad_pr[pop.wsm.prepnd$incid >= 1.7e-07 & pop.wsm.prepnd$incid < 3.4e-06 ] <- 0.3
pop.wsm.prepnd$prep_grad_pr[pop.wsm.prepnd$incid < 1.7e-07 ] <- 0.2

pop.wsm.prepnd$prep_step2_pr[pop.wsm.prepnd$incid >= 5.3e-04  ] <- 1
pop.wsm.prepnd$prep_step2_pr[pop.wsm.prepnd$incid >= 2.3e-05 & pop.wsm.prepnd$incid < 5.3e-04  ] <- 0.5
pop.wsm.prepnd$prep_step2_pr[pop.wsm.prepnd$incid < 2.3e-05 ] <- 0.1

pop.wsm.prepnd$prep_step3_pr[pop.wsm.prepnd$incid >= 1e-03 ] <- 1
pop.wsm.prepnd$prep_step3_pr[pop.wsm.prepnd$incid < 1e-03 ] <- 0

pop.wsm.prepnd$prep_grad <- pop.wsm.prepnd$prep_100* pop.wsm.prepnd$prep_grad_pr
pop.wsm.prepnd$prep_step1 <- pop.wsm.prepnd$prep_100* pop.wsm.prepnd$prep_step1_pr
pop.wsm.prepnd$prep_step2 <- pop.wsm.prepnd$prep_100* pop.wsm.prepnd$prep_step2_pr
pop.wsm.prepnd$prep_step3 <- pop.wsm.prepnd$prep_100* pop.wsm.prepnd$prep_step3_pr


###################
## incidence
# prep_incid is the number of people, incid is the proportion
pop.wsm.prepnd$prep_incid <- pop.wsm.prepnd$prep_100* pop.wsm.prepnd$incid
pop.wsm.prepnd$prep_incid_min <- pop.wsm.prepnd_min$prep_100* pop.wsm.prepnd_min$incid
pop.wsm.prepnd$prep_incid_max <- pop.wsm.prepnd_max$prep_100* pop.wsm.prepnd_max$incid

pop.wsm.prepnd$prep_step1     <- pop.wsm.prepnd$prep_100* pop.wsm.prepnd$prep_step1_pr
pop.wsm.prepnd$prep_step1_min <- pop.wsm.prepnd_min$prep_100* pop.wsm.prepnd$prep_step1_pr_min
pop.wsm.prepnd$prep_step1_max <- pop.wsm.prepnd_max$prep_100* pop.wsm.prepnd$prep_step1_pr_max

############################################################

# pop.wsm.prepnd %>%
#   select(state, Age.Group, Sex, Race.Ethnicity, prep_100, incid, prep_fem, acs_pop,  prep_grad, prep_step2, prep_step1, prep_step3, prep_incid) -> femsummary

pop.msm.prepnd %>%
  select(state, Age.Group, Sex, Race.Ethnicity, prep_100,  prep_100_min, prep_100_max, prep_msm_cdc,
                                                prep_incid, prep_incid_min, prep_incid_max,
                                                prep_step1, prep_step1_min, prep_step1_max) %>% # prep_grad, prep_step2, prep_step1, prep_step3
  group_by(state, Age.Group, Sex) %>%
  summarise(
            prep_incid = sum(prep_incid),
            prep_incid_min = sum(prep_incid_min),
            prep_incid_max = sum(prep_incid_max),
            prep_step1 = sum(prep_step1),
            prep_step1_min = sum(prep_step1_min),
            prep_step1_max = sum(prep_step1_max),
            prep_100 = sum(prep_100),
            prep_100_min = sum(prep_100_min),
            prep_100_max = sum(prep_100_max),
            prep_msm_cdc = sum(prep_msm_cdc)
            # prep_grad = sum(prep_grad),
            # prep_step1 = sum(prep_step1),
            # prep_step2 = sum(prep_step2)
            ) %>%
  ungroup() %>%
  filter(!is.na(prep_100)) -> msm_state_age

# pop.msm.prepnd %>%
#   select(state, Age.Group, Sex, Race.Ethnicity, prep_100,  prep_100_min, prep_100_max, prep_msm_cdc,
#          prep_incid, prep_incid_min, prep_incid_max,
#          prep_step1, prep_step1_min, prep_step1_max) %>% # prep_grad, prep_step2, prep_step1, prep_step3
#   group_by(state, Age.Group, Sex) %>%
#   summarise(
#     prep_incid = sum(prep_incid),
#     prep_incid_min = sum(prep_incid_min),
#     prep_incid_max = sum(prep_incid_max),
#     prep_step1 = sum(prep_step1),
#     prep_step1_min = sum(prep_step1_min),
#     prep_step1_max = sum(prep_step1_max),
#     prep_100 = sum(prep_100),
#     prep_100_min = sum(prep_100_min),
#     prep_100_max = sum(prep_100_max),
#     prep_msm_cdc = sum(prep_msm_cdc)
#     # prep_grad = sum(prep_grad),
#     # prep_step1 = sum(prep_step1),
#     # prep_step2 = sum(prep_step2)
#   ) %>%
#   ungroup() %>%
#   filter(!is.na(prep_100)) -> msm_state_age

pop.msw.prepnd %>%
  select(state, Age.Group, Sex, Race.Ethnicity,  prep_100,  prep_100_min, prep_100_max, 
                                                 prep_incid, prep_incid_min, prep_incid_max,
                                                 prep_step1, prep_step1_min, prep_step1_max) %>% # , prep_grad, prep_step2, prep_step1, prep_step3
  group_by(state, Age.Group, Sex) %>%
  summarise(
            prep_incid = sum(prep_incid),
            prep_incid_min = sum(prep_incid_min),
            prep_incid_max = sum(prep_incid_max),
            prep_step1 = sum(prep_step1),
            prep_step1_min = sum(prep_step1_min),
            prep_step1_max = sum(prep_step1_max),
            prep_100 = sum(prep_100),
            prep_100_min = sum(prep_100_min),
            prep_100_max = sum(prep_100_max),
            # prep_grad = sum(prep_grad),
            # prep_step1 = sum(prep_step1),
            # prep_step2 = sum(prep_step2)
            ) %>%
  ungroup() %>%
  filter(!is.na(prep_100)) %>%
  mutate( prep_msm_cdc = NA) -> msw_state_age

pop.wsm.prepnd %>%
  select(state, Age.Group, Sex, Race.Ethnicity,  prep_100,  prep_100_min, prep_100_max, 
                                                 prep_incid, prep_incid_min, prep_incid_max,
                                                 prep_step1, prep_step1_min, prep_step1_max) %>% # , prep_grad, prep_step2, prep_step1, prep_step3
  group_by(state, Age.Group, Sex) %>%
  summarise(
            prep_incid = sum(prep_incid),
            prep_incid_min = sum(prep_incid_min),
            prep_incid_max = sum(prep_incid_max),
            prep_step1 = sum(prep_step1),
            prep_step1_min = sum(prep_step1_min),
            prep_step1_max = sum(prep_step1_max),
            prep_100 = sum(prep_100),
            prep_100_min = sum(prep_100_min),
            prep_100_max = sum(prep_100_max),
            # prep_grad = sum(prep_grad),
            # prep_step1 = sum(prep_step1),
            # prep_step2 = sum(prep_step2)
            ) %>%
  ungroup() %>%
  filter(!is.na(prep_100)) -> wsm_state_age

wsm_state_age %>%
  left_join(msw_state_age, by = c("state", "Age.Group")) %>%
  left_join(msm_state_age, by = c("state", "Age.Group")) %>%
  mutate(risk_est = prep_incid.x + prep_incid.y + prep_incid,
         risk_est_min = prep_incid_min.x + prep_incid_min.y + prep_incid_min,
         risk_est_max = prep_incid_max.x + prep_incid_max.y + prep_incid_max) %>%
  group_by(state) %>%
  summarise(risk_est = sum(risk_est),
            risk_est_min = sum(risk_est_min),
            risk_est_max = sum(risk_est_max)) %>%
  ungroup() -> state_results

rbind(msw_state_age,  msm_state_age) %>%
  group_by(state, Sex, Age.Group) %>%
  summarize(
            prep_incid = sum(prep_incid),
            prep_incid_min = sum(prep_incid_min),
            prep_incid_max = sum(prep_incid_max),
            prep_step1 = sum(prep_step1),
            prep_step1_min = sum(prep_step1_min),
            prep_step1_max = sum(prep_step1_max),
            prep_100 = sum(prep_100),
            prep_100_min = sum(prep_100_min),
            prep_100_max = sum(prep_100_max)) %>%
  ungroup() -> men_results

## National level results by total, sex, R/E age

pop.msm.prepnd %>%
  select(state, Age.Group, Sex, Race.Ethnicity, prep_incid, prep_incid_min, prep_incid_max) %>%
  group_by(Age.Group, Sex, Race.Ethnicity) %>% 
  summarise( prep_incid = sum(prep_incid, na.rm=TRUE), 
             prep_incid_min = sum(prep_incid_min, na.rm=TRUE), 
             prep_incid_max = sum(prep_incid_max, na.rm=TRUE)) %>%
     ungroup() -> msm_results

pop.wsm.prepnd %>%
  filter(!is.na(prep_incid)) %>% # only leaves the populations with incidence data
  select(state, Age.Group, Sex, Race.Ethnicity, prep_incid, prep_incid_min, prep_incid_max, acs_pop) %>%
  group_by(Age.Group, Sex, Race.Ethnicity) %>% 
  summarise( prep_incid = sum(prep_incid, na.rm=TRUE), 
             prep_incid_min = sum(prep_incid_min, na.rm=TRUE), 
             prep_incid_max = sum(prep_incid_max, na.rm=TRUE),
             population = sum(acs_pop, na.rm=TRUE) ) %>%
  ungroup() -> wsm_results

pop.msw.prepnd %>%
  filter(!is.na(prep_incid)) %>% # only leaves the populations with incidence data
  select(state, Age.Group, Sex, Race.Ethnicity, prep_incid, prep_incid_min, prep_incid_max, acs_pop) %>%
  group_by(Age.Group, Sex, Race.Ethnicity) %>% 
  summarise( prep_incid = sum(prep_incid, na.rm=TRUE), 
             prep_incid_min = sum(prep_incid_min, na.rm=TRUE), 
             prep_incid_max = sum(prep_incid_max, na.rm=TRUE), 
             population = sum(acs_pop, na.rm=TRUE) ) %>%
  ungroup() -> msw_results

msw_results %>%
  left_join(msm_results, by = c("Age.Group", "Sex", "Race.Ethnicity")) %>%
  mutate(prep_incid     = prep_incid.x +  prep_incid.y,
         prep_incid_min = prep_incid_min.x +  prep_incid_min.y,
         prep_incid_max = prep_incid_max.x +  prep_incid_max.y) %>%
  bind_rows(wsm_results)  -> all_results #%>%
#  select(Age.Group, Sex, Race.Ethnicity, prep_incid, population) -> all_results

# nat_dat <- data.frame( data = c("prep_model", "prep_model_min","prep_model_max", "cdc_est", "cdc_ll", "cdc_ul"),
#                       total = c(sum(all_results$prep_incid), sum(all_results$prep_incid_min), sum(all_results$prep_incid_max), 32200,	29700, 34700),
#                        msm  = c(sum(msm_results$prep_incid), sum(all_results$prep_incid_min), sum(all_results$prep_incid_max), 24600,	22900, 26300),
#                        wsm  = c(sum(wsm_results$prep_incid), sum(all_results$prep_incid_min), sum(all_results$prep_incid_max), 5200, 4600, 5800),
#                        msw  = c(sum(msw_results$prep_incid), sum(all_results$prep_incid_min), sum(all_results$prep_incid_max), 2400, 1900, 3000) )

nat_dat <- data.frame( population     = c(".Total (non-IDU)", "MSM", "WSM", "MSW"),
                       prep_model     = c(sum(all_results$prep_incid), sum(msm_results$prep_incid), sum(wsm_results$prep_incid), sum(msw_results$prep_incid)),
                       prep_model_min = c(sum(all_results$prep_incid_min), sum(msm_results$prep_incid_min), sum(wsm_results$prep_incid_min), sum(msw_results$prep_incid_min)),
                       prep_model_max = c(sum(all_results$prep_incid_max), sum(msm_results$prep_incid_max), sum(wsm_results$prep_incid_max), sum(msw_results$prep_incid_max)),
                       cdc_est        = c(32200, 24600, 5200, 2400),
                       cdc_ll         = c(29700, 22900, 4600, 1900),
                       cdc_ul         = c(34700, 26300, 5800, 3000) )

nat_dat[1:4,2]/nat_dat[1:4,3]
           

nat_dat %>%
  ggplot()+
  geom_pointrange(aes(y=population, x=prep_model, xmin = prep_model_min, xmax = prep_model_max),color="blue", alpha = 4/10) +
  geom_pointrange(aes(y=population, x=cdc_est, xmin = cdc_ll, xmax = cdc_ul), alpha = 4/10) +
  theme_bw() +
  geom_text(aes(label=round(nat_dat[1:4,2]/nat_dat[1:4,3],2), y=population, x=50900)) +
  xlab("annual HIV incidence estimates") #+ xlim(c(0, 51000))

  
read_csv(here("cdc_raw_data_atlas/AtlasPlusTableData_national_hivincidence_2019.csv"), skip=10) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(cases_ll = as.numeric(gsub(",", "", `Cases LCI`))) %>%
  mutate(cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(rate = as.numeric(gsub(",", "", `Rate per 100000`))) %>%
  mutate(rate_ll = as.numeric(gsub(",", "", `Rate LCI`))) %>%
  mutate(rate_ul = as.numeric(gsub(",", "", `Rate UCI`))) %>%
  rename(state= Geography) -> cdc_national
  
# national_results <- data.frame(national = c()
#                                   )

# read_csv(here("cdc_raw_data_atlas/state-overall-all.csv"), skip=20) %>%
#   filter(Year == 2019) %>%
#   mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
#   mutate(cases_ll = as.numeric(gsub(",", "", `Cases LCI`))) %>%
#   mutate(cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
#   mutate(rate = as.numeric(gsub(",", "", `Rate per 100000`))) %>%
#   mutate(rate_ll = as.numeric(gsub(",", "", `Rate LCI`))) %>%
#   mutate(rate_ul = as.numeric(gsub(",", "", `Rate UCI`))) %>%
#   rename(state= Geography) %>%
#   filter(Indicator == "Estimated HIV prevalence" | )

read_csv(here("cdc_raw_data_atlas/hiv-incidence-estimated-all.csv"), skip=10) %>%
  filter(Year == 2019) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(cases_ll = as.numeric(gsub(",", "", `Cases LCI`))) %>%
  mutate(cases_ul = as.numeric(gsub(",", "", `Cases UCI`))) %>%
  mutate(rate = as.numeric(gsub(",", "", `Rate per 100000`))) %>%
  mutate(rate_ll = as.numeric(gsub(",", "", `Rate LCI`))) %>%
  mutate(rate_ul = as.numeric(gsub(",", "", `Rate UCI`))) %>%
  rename(state= Geography) %>%
  left_join(state_results, by = "state") %>%
  mutate(risk_rate = 100000 * risk_est / Population, 
         risk_rate_min = 100000 * risk_est_min / Population, 
         risk_rate_max = 100000 * risk_est_max / Population) %>%
  mutate(perc_diff = risk_rate/rate) -> incid_comp


read_csv(here("data/prep_need_sex_state_age_2019.csv"), skip=7) %>%
  mutate(cases = as.numeric(gsub(",", "", Population))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  rename(Age.Group = `Age Group`) %>%
  select(state, Age.Group, Sex, Population) %>%
  rename(cdc_estimate = Population) %>%
  left_join(rbind(wsm_state_age, men_results), by = c("state", "Age.Group", "Sex"))  ->  results_all

read_csv(here("data/hiv_burden_cdc_2019.csv"), skip=10) %>%  
  filter(Indicator == "Estimated HIV prevalence (undiagnosed and diagnosed)") %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>% 
  mutate(Age.Group = `Age Group`) %>%
  mutate(rate = cases/Population) %>%
  select(cases, state, Age.Group, Population, rate ) -> hivprev
  
results_all %>%
  filter(state !="Northern Mariana Islands") %>%
  filter(state !="American Samoa") %>%
  filter(state !="Guam") %>%
  filter(state !="US Virgin Islands") %>%
  filter(state !="Vermont") %>%
  filter(state !="North Dakota") %>%
  filter(state !="Utah") %>%
  filter(state !="Montana") %>%
  filter(state !="Missouri") %>%
  filter(state !="Idaho") %>%
  filter(state !="Delaware") %>%
  filter(state !="Massachusetts") %>%
  filter(state !="New Hampshire") %>%
  # filter(Sex == "Female") %>%
  # filter(Sex == "Male") %>%
  group_by(state) %>%
  summarize(cdc_estimate=sum(cdc_estimate),
            prep_100_min=sum(prep_100_min),
            prep_100_max=sum(prep_100_max),
            prep_step1_min=sum(prep_step1_min),
            prep_step1_max=sum(prep_step1_max))  %>% #-> table_res
  ungroup() %>%
  left_join(hivprev, by = c("state")) -> table_res

plot(table_res$rate, table_res$prep_step1_min, ylim=c(0, 20^4), xlim=c(0, 0.01), ylab="", xlab="")

            
            
            