

rbind(msm_out, wsm_out, msw_out) %>%
  group_by(trnsm, age) %>%
  summarise(
    q025_inc = 100*quantile(unlist(across(starts_with("inc"))), 0.025, na.rm = TRUE),
    q50_inc =  100*quantile(unlist(across(starts_with("inc"))), 0.50, na.rm = TRUE),
    q975_inc = 100*quantile(unlist(across(starts_with("inc"))), 0.975, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    across(c(q025_inc, q50_inc, q975_inc), ~ format(signif(.x, digits = 3), big.mark = ",")), # Format numbers to 3 significant figures with comma separators
    q_range_inc = paste(q025_inc, q975_inc, sep = " - ")
  ) %>%
  select(trnsm, age,  q50_inc, q_range_inc) ->  trns_risk_tab

write.csv(trns_risk_tab,    here("output_data/trns_risk_tab.csv"), row.names = FALSE)