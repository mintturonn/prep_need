
# incidence data from CDC Atlas Plus

# cdc hiv data state level by mode 
read_excel(here("data/AtlasPlusTableData_hivprev_2019_age_state.xlsx"), skip=10) %>%
  mutate(cases = as.numeric(gsub(",", "", Cases))) %>%
  mutate(state = gsub("[[:punct:]]", "", Geography)) %>%
  select(state, cases, `Transmission Category`, `Age Group`, FIPS) %>%
  filter(`Transmission Category` == "Male-to-male sexual contact" |
           `Transmission Category` ==   "Male-to-male sexual contact and injection drug use") %>%
  mutate( agecat = ifelse(`Age Group` == "13-24" | `Age Group` =="25-34" | `Age Group` == "35-44", "younger", "older")) %>%
  rename(Age.Group = `Age Group`) %>%