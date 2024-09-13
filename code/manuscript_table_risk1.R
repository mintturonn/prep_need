
library(ggpubr)
library(knitr)
library(kableExtra)
library(officer)
library(scales)
# state
# Create the summary data

rbind(msm_out, wsm_out, msw_out) %>%
  est_r1() %>%
  est_r2() %>%
  group_by(state) %>%
  summarise(across(starts_with("tot.prep.ind"), \(x) sum(x, na.rm = TRUE)),
            across(starts_with("r1"), \(x) sum(x, na.rm = TRUE)),
            across(starts_with("r2"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    state = state,
    q025_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.025, na.rm = TRUE),
    q50_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.50, na.rm = TRUE),
    q975_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.975, na.rm = TRUE),
    q025_r1 = quantile(c_across(starts_with("r1")), 0.025, na.rm = TRUE),
    q50_r1 = quantile(c_across(starts_with("r1")), 0.50, na.rm = TRUE),
    q975_r1 = quantile(c_across(starts_with("r1")), 0.975, na.rm = TRUE),
    q025_r2 = quantile(c_across(starts_with("r2")), 0.025, na.rm = TRUE),
    q50_r2 = quantile(c_across(starts_with("r2")), 0.50, na.rm = TRUE),
    q975_r2 = quantile(c_across(starts_with("r2")), 0.975, na.rm = TRUE)
  ) %>%  
  mutate(across(c(q025_ind, q50_ind, q975_ind), ~ format(signif(.x, digits = 3), big.mark = ","))) %>%  # Format numbers to 3 significant figures with comma separators
  mutate(across(c(q025_r1, q50_r1, q975_r1), ~ format(signif(.x, digits = 3), big.mark = ","))) %>% 
  mutate(across(c(q025_r2, q50_r2, q975_r2), ~ format(signif(.x, digits = 3), big.mark = ","))) %>% 
  mutate(q_range_ind = paste(q025_ind, q975_ind, sep = " - ")) %>%
  mutate(q_range_r1  = paste(q025_r1, q975_r1, sep = " - ")) %>%
  mutate(q_range_r2  = paste(q025_r2, q975_r2, sep = " - ")) %>%
  select(state,  q50_ind, q_range_ind, q50_r1, q_range_r1,  q50_r2, q_range_r2) -> state_tab


write.csv(state_tab,    here("output_data/state_tab.csv"), row.names = FALSE)

# r/e

rbind(msm_out, wsm_out, msw_out) %>%
  est_r1() %>%
  est_r2() %>%
  group_by(race) %>%
  summarise(across(starts_with("tot.prep.ind"), \(x) sum(x, na.rm = TRUE)),
            across(starts_with("r1"), \(x) sum(x, na.rm = TRUE)),
            across(starts_with("r2"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    race = race,
    q025_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.025, na.rm = TRUE),
    q50_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.50, na.rm = TRUE),
    q975_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.975, na.rm = TRUE),
    q025_r1 = quantile(c_across(starts_with("r1")), 0.025, na.rm = TRUE),
    q50_r1 = quantile(c_across(starts_with("r1")), 0.50, na.rm = TRUE),
    q975_r1 = quantile(c_across(starts_with("r1")), 0.975, na.rm = TRUE),
    q025_r2 = quantile(c_across(starts_with("r2")), 0.025, na.rm = TRUE),
    q50_r2 = quantile(c_across(starts_with("r2")), 0.50, na.rm = TRUE),
    q975_r2 = quantile(c_across(starts_with("r2")), 0.975, na.rm = TRUE)
  ) %>%  
  mutate(across(c(q025_ind, q50_ind, q975_ind), ~ format(signif(.x, digits = 3), big.mark = ","))) %>%  # Format numbers to 3 significant figures with comma separators
  mutate(across(c(q025_r1, q50_r1, q975_r1), ~ format(signif(.x, digits = 3), big.mark = ","))) %>% 
  mutate(across(c(q025_r2, q50_r2, q975_r2), ~ format(signif(.x, digits = 3), big.mark = ","))) %>% 
  mutate(q_range_ind = paste(q025_ind, q975_ind, sep = " - ")) %>%
  mutate(q_range_r1  = paste(q025_r1, q975_r1, sep = " - ")) %>%
  mutate(q_range_r2  = paste(q025_r2, q975_r2, sep = " - ")) %>%
  select(race,  q50_ind, q_range_ind, q50_r1, q_range_r1,  q50_r2, q_range_r2) -> race_tab

write.csv(race_tab,    here("output_data/re_tab.csv"), row.names = FALSE)


# transmission risk group
rbind(msm_out, wsm_out, msw_out) %>%
  est_r1() %>%
  est_r2() %>%
  group_by(trnsm) %>%
  summarise(across(starts_with("tot.prep.ind"), \(x) sum(x, na.rm = TRUE)),
            across(starts_with("r1"), \(x) sum(x, na.rm = TRUE)),
            across(starts_with("r2"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    trnsm = trnsm,
    q025_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.025, na.rm = TRUE),
    q50_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.50, na.rm = TRUE),
    q975_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.975, na.rm = TRUE),
    q025_r1 = quantile(c_across(starts_with("r1")), 0.025, na.rm = TRUE),
    q50_r1 = quantile(c_across(starts_with("r1")), 0.50, na.rm = TRUE),
    q975_r1 = quantile(c_across(starts_with("r1")), 0.975, na.rm = TRUE),
    q025_r2 = quantile(c_across(starts_with("r2")), 0.025, na.rm = TRUE),
    q50_r2 = quantile(c_across(starts_with("r2")), 0.50, na.rm = TRUE),
    q975_r2 = quantile(c_across(starts_with("r2")), 0.975, na.rm = TRUE)
  ) %>%  
  mutate(across(c(q025_ind, q50_ind, q975_ind), ~ format(signif(.x, digits = 3), big.mark = ","))) %>%  # Format numbers to 3 significant figures with comma separators
  mutate(across(c(q025_r1, q50_r1, q975_r1), ~ format(signif(.x, digits = 3), big.mark = ","))) %>% 
  mutate(across(c(q025_r2, q50_r2, q975_r2), ~ format(signif(.x, digits = 3), big.mark = ","))) %>% 
  mutate(q_range_ind = paste(q025_ind, q975_ind, sep = " - ")) %>%
  mutate(q_range_r1  = paste(q025_r1, q975_r1, sep = " - ")) %>%
  mutate(q_range_r2  = paste(q025_r2, q975_r2, sep = " - ")) %>%
  select(trnsm,  q50_ind, q_range_ind, q50_r1, q_range_r1,  q50_r2, q_range_r2) -> trns_tab

write.csv(trns_tab,    here("output_data/trns_tab.csv"), row.names = FALSE)


# age

rbind(msm_out, wsm_out, msw_out) %>%
  est_r1() %>%
  est_r2() %>%
  group_by(age) %>%
  summarise(across(starts_with("tot.prep.ind"), \(x) sum(x, na.rm = TRUE)),
            across(starts_with("r1"), \(x) sum(x, na.rm = TRUE)),
            across(starts_with("r2"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    age = age,
    q025_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.025, na.rm = TRUE),
    q50_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.50, na.rm = TRUE),
    q975_ind = quantile(c_across(starts_with("tot.prep.ind")), 0.975, na.rm = TRUE),
    q025_r1 = quantile(c_across(starts_with("r1")), 0.025, na.rm = TRUE),
    q50_r1 = quantile(c_across(starts_with("r1")), 0.50, na.rm = TRUE),
    q975_r1 = quantile(c_across(starts_with("r1")), 0.975, na.rm = TRUE),
    q025_r2 = quantile(c_across(starts_with("r2")), 0.025, na.rm = TRUE),
    q50_r2 = quantile(c_across(starts_with("r2")), 0.50, na.rm = TRUE),
    q975_r2 = quantile(c_across(starts_with("r2")), 0.975, na.rm = TRUE)
  ) %>%  
  mutate(across(c(q025_ind, q50_ind, q975_ind), ~ format(signif(.x, digits = 3), big.mark = ","))) %>%  # Format numbers to 3 significant figures with comma separators
  mutate(across(c(q025_r1, q50_r1, q975_r1), ~ format(signif(.x, digits = 3), big.mark = ","))) %>% 
  mutate(across(c(q025_r2, q50_r2, q975_r2), ~ format(signif(.x, digits = 3), big.mark = ","))) %>% 
  mutate(q_range_ind = paste(q025_ind, q975_ind, sep = " - ")) %>%
  mutate(q_range_r1  = paste(q025_r1, q975_r1, sep = " - ")) %>%
  mutate(q_range_r2  = paste(q025_r2, q975_r2, sep = " - ")) %>%
  select(age,  q50_ind, q_range_ind, q50_r1, q_range_r1,  q50_r2, q_range_r2)-> age_tab

write.csv(age_tab,    here("output_data/age_tab.csv"), row.names = FALSE)



