

pop_re_fun <- function(acs_year, yearcat0){
  
  racel   <- c("A",     "H",      "B",     "C",    "D",     "E",    "F",     "G",     "I")
  racethn <- c("white_any", "white", "black", "aian", "asian", "nhpi", "other", "multi", "hispanic")
  Race.Ethnicity <-c("White_any", "White", "Black/African American", "American Indian/Alaska Native", "Asian", 
                     "Native Hawaiian/Other Pacific Islander", "Other", "Multiracial", "Hispanic/Latino") 
                    
  acs_yr <- load_variables(year = as.numeric(acs_year), dataset = "acs5", cache = TRUE)
  
  age_vars <- acs_yr$name[1:331]
  
  age <- get_acs(geography = "state",
                 variables = age_vars,
                 year = as.numeric(acs_year),
                 moe_level = 95)
  
  for (i in 1:length(racel)) {
    
    rl <- racel[i]
  age %>%
    group_by(GEOID, NAME) %>%
    summarize (`13-24` =2/5* estimate[variable==paste0("B01001", rl, "_005")] +
                             estimate[variable==paste0("B01001", rl, "_006")] +
                             estimate[variable==paste0("B01001", rl, "_007")] +
                             estimate[variable==paste0("B01001", rl, "_008")],
               `13-17` =2/5* estimate[variable==paste0("B01001", rl, "_005")] +
                             estimate[variable==paste0("B01001", rl, "_006")],
               `18-24` =     estimate[variable==paste0("B01001", rl, "_007")] +
                             estimate[variable==paste0("B01001", rl, "_008")],
               `25-34` =     estimate[variable==paste0("B01001", rl, "_009")] +
                             estimate[variable==paste0("B01001", rl, "_010")],
               `35-44` =     estimate[variable==paste0("B01001", rl, "_011")],
               `45-54` =     estimate[variable==paste0("B01001", rl, "_012")],
               `55+`   =     estimate[variable==paste0("B01001", rl, "_013")] +
                             estimate[variable==paste0("B01001", rl, "_014")] +
                             estimate[variable==paste0("B01001", rl, "_015")] +
                             estimate[variable==paste0("B01001", rl, "_016")] ) %>%
   ungroup() %>%
   mutate(FIPS = as.numeric(GEOID)) %>%
   mutate(Race.Ethnicity = Race.Ethnicity[i]) %>%
   mutate(Sex = "Male") %>%
   mutate(yearcat = yearcat0) %>%
   rename(state = NAME)  -> pop.st.m0
  
   if (i==1){ pop.st.m = pop.st.m0 } else {
     
     pop.st.m <- rbind(pop.st.m, pop.st.m0)
   }
  
  
  age %>%
    group_by(GEOID, NAME) %>%
    summarize (`13-24` =  2/5* estimate[variable==paste0("B01001", rl, "_020")] +
                               estimate[variable==paste0("B01001", rl, "_021")] +
                               estimate[variable==paste0("B01001", rl, "_022")] +
                               estimate[variable==paste0("B01001", rl, "_023")],
               `13-17` =  2/5* estimate[variable==paste0("B01001", rl, "_020")] +
                               estimate[variable==paste0("B01001", rl, "_021")],
               `18-24` =       estimate[variable==paste0("B01001", rl, "_022")] +
                               estimate[variable==paste0("B01001", rl, "_023")],
               `25-34` =       estimate[variable==paste0("B01001", rl, "_024")] +
                               estimate[variable==paste0("B01001", rl, "_025")] ,
               `35-44` =       estimate[variable==paste0("B01001", rl, "_026")] ,
               `45-54`=        estimate[variable==paste0("B01001", rl, "_027")] ,
               `55+` =         estimate[variable==paste0("B01001", rl, "_028")] +
                               estimate[variable==paste0("B01001", rl, "_029")] +
                               estimate[variable==paste0("B01001", rl, "_030")] +
                               estimate[variable==paste0("B01001", rl, "_031")] ) %>%
    ungroup() %>%
    mutate(FIPS = as.numeric(GEOID)) %>%
    mutate(Race.Ethnicity = Race.Ethnicity[i]) %>%
    mutate(Sex = "Female") %>%
    mutate(yearcat = yearcat0) %>%
    rename(state = NAME) -> pop.st.f0
 
  if (i==1){ pop.st.f = pop.st.f0 } else {
    
    pop.st.f <- rbind(pop.st.f, pop.st.f0)
   }
  }
  return(list(m = pop.st.m, f = pop.st.f))
}


pop_fun <- function(acs_year){
  
  acs_yr <- load_variables(year = as.numeric(acs_year), dataset = "acs5", cache = TRUE)
  
  age_vars <- acs_yr$name[1:79]
  
  age <- get_acs(geography = "state",
                 variables = age_vars,
                 year = as.numeric(acs_year),
                 moe_level = 95)
  
  age %>%
    group_by(GEOID, NAME) %>%
    summarize (`13-24` =(2/5)*estimate[variable=="B01001_005"] +
                             estimate[variable=="B01001_006"] +
                             estimate[variable=="B01001_007"] +
                             estimate[variable=="B01001_008"] +
                             estimate[variable=="B01001_009"] +
                             estimate[variable=="B01001_010"],
               `13-17` =(2/5)*estimate[variable=="B01001_005"] +
                             estimate[variable=="B01001_006"] ,
               `18-24` =      estimate[variable=="B01001_007"] +
                             estimate[variable=="B01001_008"] +
                             estimate[variable=="B01001_009"] +
                             estimate[variable=="B01001_010"],
               `25-34` =     estimate[variable=="B01001_011"] +
                             estimate[variable=="B01001_012"],
               `35-44` =     estimate[variable=="B01001_013"] +
                             estimate[variable=="B01001_014"],
               `45-54` =     estimate[variable=="B01001_015"] +
                             estimate[variable=="B01001_016"], 
               `55+` =       estimate[variable=="B01001_017"] +
                             estimate[variable=="B01001_018"] +
                             estimate[variable=="B01001_019"] +
                             estimate[variable=="B01001_020"] +
                             estimate[variable=="B01001_021"] +
                             estimate[variable=="B01001_022"] +
                             estimate[variable=="B01001_023"] +
                             estimate[variable=="B01001_024"] +
                             estimate[variable=="B01001_025"] ) %>%
    ungroup() %>%
    mutate(FIPS = as.numeric(GEOID)) %>%
    rename(state = NAME) -> pop.st.m
  
  age %>%
    group_by(GEOID, NAME) %>%
    summarize (`13-24` =  2/5*estimate[variable=="B01001_029"] +
                               estimate[variable=="B01001_030"] +
                               estimate[variable=="B01001_031"] +
                               estimate[variable=="B01001_032"] +
                               estimate[variable=="B01001_033"] +
                               estimate[variable=="B01001_034"],
               `13-17` =  2/5*estimate[variable=="B01001_029"] +
                               estimate[variable=="B01001_030"],
               `18-24` =      estimate[variable=="B01001_031"] +
                               estimate[variable=="B01001_032"] +
                               estimate[variable=="B01001_033"] +
                               estimate[variable=="B01001_034"],
               `25-34` =      estimate[variable=="B01001_035"] +
                              estimate[variable=="B01001_036"],
               `35-44` =      estimate[variable=="B01001_037"] +
                              estimate[variable=="B01001_038"],
               `45-54`=       estimate[variable=="B01001_039"] +
                               estimate[variable=="B01001_040"], 
               `55+` =         estimate[variable=="B01001_041"] +
                               estimate[variable=="B01001_042"] +
                               estimate[variable=="B01001_043"] +
                               estimate[variable=="B01001_044"] +
                               estimate[variable=="B01001_045"] +
                               estimate[variable=="B01001_046"] +
                               estimate[variable=="B01001_047"] +
                               estimate[variable=="B01001_048"] +
                               estimate[variable=="B01001_049"] ) %>%
    ungroup() %>%
    mutate(FIPS = as.numeric(GEOID)) %>%
    rename(state = NAME) -> pop.st.f
  
  return(list(m = pop.st.m, f = pop.st.f))
  
}

pop_fun_old <- function(acs_year){
  
  acs_yr <- load_variables(year = as.numeric(acs_year), dataset = "acs5", cache = TRUE)
  
  age_vars <- acs_yr$name[1:79]
  
  age <- get_acs(geography = "state",
                 variables = age_vars,
                 year = as.numeric(acs_year),
                 moe_level = 95)
  
  age %>%
    group_by(GEOID, NAME) %>%
    summarize (`13-24` =2/5*estimate[variable=="B01001_005"] +
                           estimate[variable=="B01001_006"] +
                           estimate[variable=="B01001_007"] +
                           estimate[variable=="B01001_008"] +
                           estimate[variable=="B01001_009"] +
                           estimate[variable=="B01001_010"],
             `25-34` =     estimate[variable=="B01001_011"] +
                           estimate[variable=="B01001_012"],
             `35-44` =     estimate[variable=="B01001_013"] +
                           estimate[variable=="B01001_014"],
             `45-54` =     estimate[variable=="B01001_015"] +
                           estimate[variable=="B01001_016"], 
             `55+` =       estimate[variable=="B01001_017"] +
                            estimate[variable=="B01001_018"] +
                            estimate[variable=="B01001_019"] +
                            estimate[variable=="B01001_020"] +
                            estimate[variable=="B01001_021"] +
                            estimate[variable=="B01001_022"] +
                            estimate[variable=="B01001_023"] +
                            estimate[variable=="B01001_024"] +
                            estimate[variable=="B01001_025"] ) %>%
              ungroup() %>%
              mutate(FIPS = as.numeric(GEOID)) %>%
              rename(state = NAME) -> pop.st.m
            
  age %>%
    group_by(GEOID, NAME) %>%
    summarize (`13-24` =  2/5*estimate[variable=="B01001_029"] +
                              estimate[variable=="B01001_030"] +
                              estimate[variable=="B01001_031"] +
                              estimate[variable=="B01001_032"] +
                              estimate[variable=="B01001_033"] +
                              estimate[variable=="B01001_034"],
            `25-34` =      estimate[variable=="B01001_035"] +
                           estimate[variable=="B01001_036"],
            `35-44` =      estimate[variable=="B01001_037"] +
                           estimate[variable=="B01001_038"],
            `45-54`=       estimate[variable=="B01001_039"] +
                           estimate[variable=="B01001_040"], 
            `55+` =         estimate[variable=="B01001_041"] +
                            estimate[variable=="B01001_042"] +
                            estimate[variable=="B01001_043"] +
                            estimate[variable=="B01001_044"] +
                            estimate[variable=="B01001_045"] +
                            estimate[variable=="B01001_046"] +
                            estimate[variable=="B01001_047"] +
                            estimate[variable=="B01001_048"] +
                            estimate[variable=="B01001_049"] ) %>%
              ungroup() %>%
              mutate(FIPS = as.numeric(GEOID)) %>%
              rename(state = NAME) -> pop.st.f
  
  return(list(m = pop.st.m, f = pop.st.f))
  
}

pop_yrs_nat_fun <- function(acs_year){
  
  acs_yr <- load_variables(year = as.numeric(acs_year), dataset = "acs5", cache = TRUE)
  
  age_vars <- acs_yr$name[280:328]
  
  age <- get_acs( variables = age_vars,
                 year = as.numeric(acs_year),
                 moe_level = 95,
                 geometry = FALSE,
                 survey = "acs1")
  
  age %>%
    group_by(GEOID, NAME) %>%
    summarize (`13-24` =2/5*estimate[variable=="B01001_005"] +
                 estimate[variable=="B01001_006"] +
                 estimate[variable=="B01001_007"] +
                 estimate[variable=="B01001_008"] +
                 estimate[variable=="B01001_009"] +
                 estimate[variable=="B01001_010"],
               `25-34` =     estimate[variable=="B01001_011"] +
                 estimate[variable=="B01001_012"],
               `35-44` =     estimate[variable=="B01001_013"] +
                 estimate[variable=="B01001_014"],
               `45-54` =     estimate[variable=="B01001_015"] +
                 estimate[variable=="B01001_016"], 
               `55-64` =       estimate[variable=="B01001_017"] +
                 estimate[variable=="B01001_018"], 
               `65-74` = estimate[variable=="B01001_019"] +
                 estimate[variable=="B01001_020"],
               `75-55` =  estimate[variable=="B01001_021"] +
                 estimate[variable=="B01001_022"] +
                 estimate[variable=="B01001_023"] +
                 estimate[variable=="B01001_024"] +
                 estimate[variable=="B01001_025"] ) %>%
    ungroup() %>%
    mutate(FIPS = as.numeric(GEOID)) %>%
    rename(state = NAME) -> pop.st.m
  
  age %>%
    group_by(GEOID, NAME) %>%
    summarize (`13-24` =  2/5*estimate[variable=="B01001_029"] +
                 estimate[variable=="B01001_030"] +
                 estimate[variable=="B01001_031"] +
                 estimate[variable=="B01001_032"] +
                 estimate[variable=="B01001_033"] +
                 estimate[variable=="B01001_034"],
               `25-34` =      estimate[variable=="B01001_035"] +
                 estimate[variable=="B01001_036"],
               `35-44` =      estimate[variable=="B01001_037"] +
                 estimate[variable=="B01001_038"],
               `45-54`=       estimate[variable=="B01001_039"] +
                 estimate[variable=="B01001_040"], 
               `55+` =         estimate[variable=="B01001_041"] +
                 estimate[variable=="B01001_042"] +
                 estimate[variable=="B01001_043"] +
                 estimate[variable=="B01001_044"] +
                 estimate[variable=="B01001_045"] +
                 estimate[variable=="B01001_046"] +
                 estimate[variable=="B01001_047"] +
                 estimate[variable=="B01001_048"] +
                 estimate[variable=="B01001_049"] ) %>%
    ungroup() %>%
    mutate(FIPS = as.numeric(GEOID)) %>%
    rename(state = NAME) -> pop.st.f
  
  return(list(m = pop.st.m, f = pop.st.f))
  
}
