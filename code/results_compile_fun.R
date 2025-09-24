
# Function to format numbers to three significant figures with commas
format_sigfig <- function(x) {
  formatted_number <- round(signif(x, digits = 3))  # Round after getting 3 significant digits
  formatted <- format(formatted_number, big.mark = ",", scientific = FALSE, trim = TRUE)
  return(formatted)
}

# Function to format numbers to three significant figures with commas using formatC
format_sigfig2 <- function(x) {
  formatted <-formatC(x, format = "f", big.mark = ",", digits = 0)
  return(formatted)
}

# Function to calculate and append results
calculate_and_append <- function(popinc_column, inc_column, prepind_column, sim_label) {
  temp <- rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out) %>%
    select(trnsm, state, age, race, sex, pop, all_of(popinc_column), all_of(inc_column), all_of(prepind_column)) %>%
    rename(popinc = !!sym(popinc_column), inc = !!sym(inc_column), prepind = !!sym(prepind_column)) %>%
    mutate(
      total_popinc = sum(popinc, na.rm = TRUE),
      prop_popinc_total = popinc / total_popinc,
      total_prepind = sum(prepind, na.rm = TRUE),
      prop_prepind_total = prepind / total_prepind
    ) %>%
    group_by(trnsm) %>%
    mutate(
      total_popinc_trnsm = sum(popinc, na.rm = TRUE),
      prop_popinc_trnsm = popinc / total_popinc_trnsm,
      total_prepind_trnsm = sum(prepind, na.rm = TRUE),
      prop_prepind_trnsm = prepind / total_prepind_trnsm
    ) %>%
    arrange(trnsm, inc) %>%
    mutate(cumprop_popinc_trnsm = cumsum(prop_popinc_trnsm),
           cumprop_prepind_trnsm = cumsum(prop_prepind_trnsm)) %>%
    ungroup() %>%
    arrange(inc) %>%
    mutate(
      cumprop_popinc = cumsum(prop_popinc_total),
      cumprop_prepind = cumsum(prop_prepind_total)
    ) %>%
    ungroup() %>%
    mutate(simulation = sim_label)
  
  # Combining results (global assignment to the results data frame)
  results <<- bind_rows(results, temp)
}
