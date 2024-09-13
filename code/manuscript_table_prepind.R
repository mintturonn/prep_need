
library(ggpubr)
library(knitr)
library(kableExtra)
library(officer)
library(scales)
# state
# Create the summary data

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"),  -starts_with("popinc")) %>%
  group_by(state) %>%
  summarise(across(starts_with("tot.prep.ind"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    state = state,
    q025 = quantile(c_across(starts_with("tot.prep.ind")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("tot.prep.ind")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("tot.prep.ind")), 0.975, na.rm = TRUE)
  ) %>%  
  mutate(across(c(q025, q50, q975), ~ format(signif(.x, digits = 3), big.mark = ","))) %>%  # Format numbers to 3 significant figures with comma separators
  mutate(q_range = paste(q025, q975, sep = " - ")) %>%
  select(state,  q50, q_range) -> state_tab


write.csv(state_tab,    here("output_data/state_tab.csv"), row.names = FALSE)

# r/e

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"),  -starts_with("popinc")) %>%
  group_by(race) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    race = race,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%  
  mutate(across(c(q025, q50, q975), ~ format(signif(.x, digits = 3), big.mark = ","))) %>%  # Format numbers to 3 significant figures with comma separators
  mutate(q_range = paste(q025, q975, sep = " - ")) %>%
  select(race,  q50, q_range) -> re_tab

write.csv(re_tab,    here("output_data/re_tab.csv"), row.names = FALSE)


# transmission risk group
rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"),  -starts_with("popinc")) %>%
  group_by(trnsm) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    trnsm = trnsm,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%  
  mutate(across(c(q025, q50, q975), ~ format(signif(.x, digits = 3), big.mark = ","))) %>%  # Format numbers to 3 significant figures with comma separators
  mutate(q_range = paste(q025, q975, sep = " - ")) %>%
  select(trnsm,  q50, q_range) -> trns_tab

write.csv(trns_tab,    here("output_data/trns_tab.csv"), row.names = FALSE)


# age

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in"),  -starts_with("popinc")) %>%
  group_by(age) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    age = age,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  ) %>%
  mutate(across(c(q025, q50, q975), ~ format(signif(.x, digits = 3), big.mark = ","))) %>%  # Format numbers to 3 significant figures with comma separators
  mutate(q_range = paste(q025, q975, sep = " - ")) %>%
  select(age,  q50, q_range) -> age_tab

write.csv(age_tab,    here("output_data/age_tab.csv"), row.names = FALSE)



