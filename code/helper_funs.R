
#####################################

est_r1 <- function(df) {
  df %>%
    mutate(across(starts_with("inc"), 
                  .fns = ~ case_when(
                    .x < 0.01 ~ get(sub("inc", "tot.prep.ind", cur_column())) * 0.1,
                    .x >= 0.01 ~ get(sub("inc", "tot.prep.ind", cur_column())) * 1,
                    TRUE ~ NA_real_
                  ),
                  .names = "r1_{col}"
    ))
}

est_r2 <- function(df) {
  df %>%
    mutate(across(starts_with("inc"), 
                  .fns = ~ case_when(
                    .x < 0.001 ~ get(sub("inc", "tot.prep.ind", cur_column())) * 0.05,
                    .x >= 0.001 & .x < 0.01 ~ get(sub("inc", "tot.prep.ind", cur_column())) * 0.1,
                    .x >= 0.01 ~ get(sub("inc", "tot.prep.ind", cur_column())) * 1,
                    TRUE ~ NA_real_
                  ),
                  .names = "r2_{col}"
    ))
}

#####################################

betapars <- function(mu, var) {
  # This function calculates the beta and alpha (shape) parameters for beta likelhood, taking the model output (mean) and variance from data (sd^2)
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  
  return(params = list(alpha = alpha, beta = beta))
}

#####################################
# Function to map index_list to wide data and separate incidence estimates into two dataframes
# Function to map index_list to combined data and separate incidence estimates into two dataframes
separate_incidence <- function(data_combined, index_list, age_map) {
  leq_3_df <- data.frame()
  gt_3_df <- data.frame()
  
  # Iterate over the index_list
  for (contact_key in names(index_list)) {
    indices <- index_list[[contact_key]]
    condition <- ifelse(grepl("_leq_3$", contact_key), "leq_3", "gt_3")
    transmission_group <- substr(contact_key, 1, 3)
    age_group <- gsub(".*\\.contact([0-9]+)_.*", "\\1", contact_key)
    age_label <- age_map[as.numeric(age_group)]
    
    # Filter data_combined for the current transmission group
    data_filtered_by_trnsm <- data_combined %>% filter(trnsm == transmission_group)
    
    # Iterate over each simulation
    for (sim in 1:5) {  # assuming 'inc', 'inc2', 'inc3', 'inc4', 'inc5'
      sim_label <- paste0("inc", ifelse(sim == 1, "", sim))
      inc_col <- paste0(sim_label, "_", age_label)
      
      # Create a temporary dataframe for filtered data
      filtered_data <- data_filtered_by_trnsm %>%
        filter(row_number() %in% indices) %>%
        select(state, race, trnsm, inc_value = all_of(inc_col)) %>%
        mutate(age = age_label, sim = sim_label)
      
      # Combine the filtered data into the respective result dataframe
      if (condition == "leq_3") {
        leq_3_df <- bind_rows(leq_3_df, filtered_data)
      } else {
        gt_3_df <- bind_rows(gt_3_df, filtered_data)
      }
    }
  }
  
  return(list(leq_3 = leq_3_df, gt_3 = gt_3_df))
}


#####################################
# create ROC curves
create_roc_curves <- function(data, incidence_var) {
  roc_obj <- roc(response = data$incident_indicator, predictor = data[[incidence_var]])
  plot(roc_obj, main = paste("ROC Curve for", incidence_var))
  print(paste("AUC for", incidence_var, ":", auc(roc_obj)))
}



