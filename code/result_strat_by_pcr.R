

nsim <- nrow(pnat)

index_list <- list()

# Loop through each risk group
for (risk_group in c("msm", "msw", "wsm")) {
  # Select columns that start with the risk group
pnat %>%
    as_tibble() %>%
    select(starts_with(paste0(risk_group, ".contact"))) -> scols
  
  # Loop through each selected column
  for (col in colnames(scols)) {
    # Create logical vectors for <= 3 and > 3
    leq_3 <- which(scols[[col]] <= 3)
    gt_3 <- which(scols[[col]] > 3)
    
    # Store the indices in the list
    index_list[[paste0(col, "_leq_3")]] <- leq_3
    index_list[[paste0(col, "_gt_3")]] <- gt_3
  }
}


rbind(msm_out) %>%
  select(state,  race,  trnsm,  age, starts_with("inc")) %>%
  pivot_wider(names_from = age, values_from = starts_with("inc")) -> msm.wide.inc

rbind(wsm_out) %>%
  select(state,  race,  trnsm,  age, starts_with("inc")) %>%
  pivot_wider(names_from = age, values_from = starts_with("inc")) -> wsm.wide.inc

rbind(msw_out) %>%
  select(state,  race,  trnsm,  age, starts_with("inc")) %>%
  pivot_wider(names_from = age, values_from = starts_with("inc")) -> msw.wide.inc


# Apply the function to separate the data   // 

# Define the age map
age_map <- c("13-24", "25-34", "35-44", "45-54", "55+")

# Apply the function to separate the data
# Ensure your_incidence_df and index_list are correctly defined in this context
inc.div <- separate_incidence(rbind(msm.wide.inc, wsm.wide.inc, msw.wide.inc), index_list, age_map)


# Generate ROC curves for each separated data frame
for (name in names(separated_incidence)) {
  data_frame <- separated_incidence[[name]]
  
  # Create a binary incident_indicator for incidence (assuming you have a correct way to calculate it)
  # This is an example, replace incident_indicator creation logic as needed
  data_frame <- data_frame %>% mutate(incident_indicator = ifelse(inc >= threshold, 1, 0))
  
  # Create ROC curves
  create_roc_curves(data_frame, "inc")
}

