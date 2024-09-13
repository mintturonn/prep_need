rm(list = ls())

pacman::p_load(tidyverse, tidycensus, rio, here, survey, srvyr, arrow)

# 1 download data -----------------------------------------------------------

# select variables of interest
vars <- c("SEX", "AGEP", "RAC1P", "HISP")

# download data
## NOTE: five variables are preloaded by default
## SERIALNO and SPORDER - uniquely identify observations
## WGTP and PWGTP - housing-unit weights and person weights
## ST - state code (Puerto Rico not included)

acs1 <- get_pums(
    variables = vars,
    variables_filter = list(AGEP = 13:99),
    state = "all",
    survey = "acs1",
    year = 2019,
    recode = TRUE,
    rep_weights = "person"
)

# split the dataset into 50 chunks
list_of_acs1 <- acs1 %>%
    mutate(chunk = ntile(row_number(), 50)) %>%
    split(.$chunk)

# write each chunk in the list to parquet files
walk2(list_of_acs1, 1:length(list_of_acs1), ~ {
    export(.x, here(
        "01_Data", "ACS", "microdata", "acs1_chunk",
        paste0("acs1_", .y, ".parquet")
    ))
})

# 2 in survey format -----------------------------------------------------------

# import each chunk and combine into one dataset
acs1 <- list.files(
    here("01_Data", "ACS", "microdata", "acs1_chunk"),
    full.names = TRUE
) %>%
    map_dfr(~ import(.x)) %>% 
    select(-chunk)

acs1_svy <- acs1 %>%
    to_survey(
        type = "person",
        design = "rep_weights"
    ) %>%
    mutate(
        # age is top half percent coded
        # where all values at or above the top half percent are coded as 99
        age = case_when(
            AGEP %in% 13:24 ~ "13-24",
            AGEP %in% 25:34 ~ "25-34",
            AGEP %in% 35:44 ~ "35-44",
            AGEP %in% 45:54 ~ "45-54",
            AGEP >= 55 ~ "55+",
            TRUE ~ NA_character_
        ),
        sex = ifelse(SEX == "1", "Male", "Female"),
        race = case_when(
            HISP != "01" ~ "Hispanic",
            HISP == "01" & RAC1P == "1" ~ "White",
            HISP == "01" & RAC1P == "2" ~ "Black",
            HISP == "01" & RAC1P == "3" ~ "AIAN",
            HISP == "01" & RAC1P == "4" ~ "AIAN",
            HISP == "01" & RAC1P == "5" ~ "AIAN",
            HISP == "01" & RAC1P == "6" ~ "Asian",
            HISP == "01" & RAC1P == "7" ~ "NHOPI",
            HISP == "01" & RAC1P == "8" ~ "Other",
            HISP == "01" & RAC1P == "9" ~ "Multiracial",
            TRUE ~ NA_character_
        ),
        FIPS = as.integer(ST),

        # drop ST_label "/" and after
        state = str_remove(ST_label, "/.*"),
        state = str_replace_all(state, " ", "_")
    )

# 3 Tables by state -----------------------------------------------------------

## 3.1 state -----------------------------------------------------------

acs1_state <- acs1_svy %>%
    survey_count(FIPS, state)
export(acs1_state, here(
    "01_Data", "ACS", "microdata",
    "pop", "acs1", "01_state.csv"
))

# 3.2 sex -----------------------------------------------------------
# 
# acs1_sex <- acs1_svy %>%
#     group_by(FIPS, state) %>%
#     survey_count(sex)
# export(acs1_sex, here(
#     "01_Data", "ACS", "microdata",
#     "pop", "acs1", "02_state_sex.csv"
# ))
# 
# ## 3.3 age -----------------------------------------------------------
# 
# acs1_age <- acs1_svy %>%
#     group_by(FIPS, state) %>%
#     survey_count(age)
# export(acs1_age, here(
#     "01_Data", "ACS", "microdata",
#     "pop", "acs1", "03_state_age.csv"
# ))
# 
# ## 3.4 race -----------------------------------------------------------
# 
# acs1_race <- acs1_svy %>%
#     group_by(FIPS, state) %>%
#     survey_count(race)
# export(acs1_race, here(
#     "01_Data", "ACS", "microdata",
#     "pop", "acs1", "04_state_race.csv"
# ))
# 
# ## 3.5 sex+age -----------------------------------------------------------
# 
# acs1_sex_age <- acs1_svy %>%
#     group_by(FIPS, state) %>%
#     survey_count(sex, age)
# export(acs1_sex_age, here(
#     "01_Data", "ACS", "microdata",
#     "pop", "acs1", "05_state_sex_age.csv"
# ))
# 
# ## 3.6 sex+race -----------------------------------------------------------
# 
# acs1_sex_race <- acs1_svy %>%
#     group_by(FIPS, state) %>%
#     survey_count(sex, race)
# export(acs1_sex_race, here(
#     "01_Data", "ACS", "microdata",
#     "pop", "acs1", "06_state_sex_race.csv"
# ))
# 
# ## 3.7 age+race -----------------------------------------------------------
# 
# acs1_age_race <- acs1_svy %>%
#     group_by(FIPS, state) %>%
#     survey_count(age, race)
# export(acs1_age_race, here(
#     "01_Data", "ACS", "microdata",
#     "pop", "acs1", "07_state_age_race.csv"
# ))

## 3.8 sex+age+race -----------------------------------------------------------

acs1_sex_age_race <- acs1_svy %>%
    group_by(FIPS, state) %>%
    survey_count(sex, age, race)
export(acs1_sex_age_race, here(
    "01_Data", "ACS", "microdata",
    "pop", "acs1", "08_state_sex_age_race.csv"
))
