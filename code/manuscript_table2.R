
# state

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in")) %>%
  group_by(state) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  rowwise() %>%
  summarise(
    state = state,
    q025 = quantile(c_across(starts_with("popinc")), 0.025, na.rm = TRUE),
    q50 = quantile(c_across(starts_with("popinc")), 0.50, na.rm = TRUE),
    q975 = quantile(c_across(starts_with("popinc")), 0.975, na.rm = TRUE)
  )

# r/e

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in")) %>%
  group_by(race) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup()

# transmission risk group
rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in")) %>%
  group_by(trnsm) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() 

# age

rbind(msm_out, wsm_out, msw_out) %>%
  select(-starts_with("inc"), -starts_with("prep.in")) %>%
  group_by(age) %>%
  summarise(across(starts_with("popinc"), \(x) sum(x, na.rm = TRUE)))



