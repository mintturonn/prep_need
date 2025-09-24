

#        rm(list = ls())
#        .rs.restartR()

library(here)
library(tidyverse)
library(readxl)
library(ggstance)
library(scales)

source("~/prep_need/code/prep_model_sir_fun.R")
source("~/prep_need/code/results_compile_fun.R")

pnat <- as.matrix(read_csv(here("output_data/pnat.csv")))
pr.msm <- as.matrix(read_csv(here("output_data/pr_msm.csv")))
pr.wsm <- as.matrix(read_csv(here("output_data/pr_wsm.csv")))
pr.msw <- as.matrix(read_csv(here("output_data/pr_msw.csv")))
pr.pwid <- as.matrix(read_csv(here("output_data/pr_pwid.csv")))

vs.msm <- as.matrix(read_csv(here("output_data/vs_msm.csv")))
vs.wsm <- as.matrix(read_csv(here("output_data/vs_wsm.csv")))
vs.msw <- as.matrix(read_csv(here("output_data/vs_msw.csv")))
vs.pwid <- as.matrix(read_csv(here("output_data/vs_pwid.csv")))

source("~/prep_need/code/calibration_data.R")
source("~/prep_need/code/calibration_figures.R")

############################

## THIS IS FOR THE CUMULATIVE INCIDENCE CALCULATIONS

# Identify all popinc and inc columns
popinc_cols <- grep("^popinc", names(rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out)), value = TRUE)
inc_cols <- grep("^inc", names(rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out)), value = TRUE)
tot_prepind_cols <- grep("^tot.prep.ind", names(rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out)), value = TRUE)

# First simulation pair (no number)
main_popinc_col <- popinc_cols[!grepl("\\d", popinc_cols)]
main_inc_col <- inc_cols[!grepl("\\d", inc_cols)]
main_tot_prepind_col <- tot_prepind_cols[!grepl("\\d", tot_prepind_cols)]

# Subsequent simulation pairs (with numbers)
popinc_similarity_cols <- grep("popinc\\d+", names(rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out)), value = TRUE)
inc_similarity_cols <- grep("inc\\d+", names(rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out)), value = TRUE)
tot_prepind_similarity_cols <- grep("tot.prep.ind\\d+", names(rbind(msm_out, wsm_out, msw_out, pwid_f_out, pwid_m_out)), value = TRUE)


# Initialize an empty data frame to store results
results <- data.frame()

# Calculate for the first simulation pair
calculate_and_append(main_popinc_col, main_inc_col, main_tot_prepind_col, "1")

# Calculate for subsequent simulation pairs
for (sim in seq_along(popinc_similarity_cols)) {
  calculate_and_append(
    popinc_similarity_cols[sim], 
    inc_similarity_cols[sim], 
    tot_prepind_similarity_cols[sim], 
    as.character(sim + 1)
  )
}

####################
## Incident infections by incidence
# results %>%
#   ggplot(aes(x=100*inc, y=100*cumprop_popinc_trnsm, identity=simulation)) +
#   geom_line(size=0.2, color="gray40") +
#   facet_wrap(~trnsm, scales = "free", ncol=4) + theme_minimal() +
#   scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
#   theme(legend.position = "none") +
#   labs(
#     x = "Average HIV incidence per 100 persons",
#     y = "Cumulative percent of HIV infections",
#     color = "Simulation") +
#   theme(
#    # panel.grid.minor = element_blank(), 
#     axis.line = element_line(color = "black"),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white") 
#   ) -> p0
# 
# ggsave(here("output_figures/cumulative_hiv.png"), plot = p0, width = 16, height = 4, dpi = 300)

results %>%
  ggplot(aes(x=100*inc, y=100*cumprop_popinc, identity=simulation)) +
  geom_line(size=0.2, color="gray40") +
#  facet_wrap(~trnsm, scales = "free", ncol=4) + theme_minimal() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  theme(legend.position = "none") +
  labs(
    x = "Average HIV incidence per 100 persons",
    y = "Cumulative percent of HIV infections",
    color = "Simulation") +
  theme(
    # panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white") 
  ) -> p0.1

ggsave(here("output_figures/cumulative_hiv_total.png"), plot = p0.1, width = 5, height = 4, dpi = 300)

###################
#Population size by by incidence
# 
# results %>%
#   ggplot(aes(x=100*inc, y=100*cumprop_prepind_trnsm, identity=simulation)) +
#   geom_line(size=0.2, color="gray40") +
#   facet_wrap(~trnsm, scales = "free", ncol=4) + theme_minimal() +
#   scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
#   theme(legend.position = "none") +
#   labs(
#     x = "Average HIV incidence per 100 persons",
#     y = "Cumulative percent of people with PrEP indicators",
#     color = "Simulation") +
#   theme(
#     #panel.grid.minor = element_blank(), 
#     axis.line = element_line(color = "black"),
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white") 
#   ) -> p1
# 
# ggsave(here("output_figures/cumulative_population_size.png"), plot = p1, width = 16, height = 4, dpi = 300)


results %>%
  ggplot(aes(x=100*inc, y=100*cumprop_prepind, identity=simulation)) +
  geom_line(size=0.2, color="gray40") +
  #  facet_wrap(~trnsm, scales = "free", ncol=4) + theme_minimal() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  theme(legend.position = "none") +
  labs(
    x = "Average HIV incidence per 100 persons",
    y = "Cumulative percent of HIV infections",
    color = "Simulation") +
  theme(
    # panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white") 
  ) -> p1.1

ggsave(here("output_figures/cumulative_population_total.png"), plot = p1.1, width = 5, height = 4, dpi = 300)

#####################
# 
# results %>%
# ggplot( aes(x = 100*cumprop_prepind_trnsm, y = 100*cumprop_popinc_trnsm, identity=simulation)) +
#   geom_line(size=0.2, color="gray40") +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Line of equality
#   facet_wrap(~trnsm,  ncol=4) + theme_minimal() +
#   labs(
#     x = "Cumulative Population Percentage",
#     y = "Cumulative HIV Infections Percentage"
#   ) +
#   theme_minimal() +
#   theme(
#     #panel.grid.minor = element_blank(), 
#     axis.line = element_line(color = "black") ,
#     panel.background = element_rect(fill = "white"),
#     plot.background = element_rect(fill = "white")
#   ) -> p2
# 
# ggsave(here("output_figures/lorenz_curve.png"), plot = p2, width = 16, height = 4, dpi = 300)
# 


results %>%
  ggplot( aes(x = 100*cumprop_prepind, y = 100*cumprop_popinc, identity=simulation)) +
  geom_line(size=0.2, color="gray40") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Line of equality
 # facet_wrap(~trnsm,  ncol=4) + theme_minimal() +
  labs(
    x = "Cumulative Population Percentage",
    y = "Cumulative HIV Infections Percentage"
  ) +
  theme_minimal() +
  theme(
    #panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black") ,
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) -> p2.1

ggsave(here("output_figures/lorenz_curve_total.png"), plot = p2.1, width = 5, height = 4, dpi = 300)

#####################

# Use the results data frame to calculate incidence per capita values for specific cumulative proportion thresholds
results %>%
  group_by(trnsm, simulation) %>%
  summarize(
    inc_at_0.1 = approx(x = cumprop_prepind_trnsm, y = inc, xout = 0.1, method = "linear")$y,
    inc_at_0.4 = approx(x = cumprop_prepind_trnsm, y = inc, xout = 0.4, method = "linear")$y,
    inc_at_0.5 = approx(x = cumprop_prepind_trnsm, y = inc, xout = 0.5, method = "linear")$y,
    inc_at_0.9 = approx(x = cumprop_prepind_trnsm, y = inc, xout = 0.9, method = "linear")$y
  ) %>%
  pivot_longer(cols = c(inc_at_0.1, inc_at_0.4, inc_at_0.5, inc_at_0.9),
               names_to = "threshold", values_to = "incidence_per_capita") %>%
  arrange(trnsm, simulation, threshold) -> incidence_quantiles_pop

# Summarize the ranges of incidence per capita corresponding to each cumulative proportion threshold
incidence_quantiles_pop %>%
#incidence_quantiles_hiv %>%
  group_by(trnsm, threshold) %>%
  summarize(
    min_incidence = 100*min(incidence_per_capita, na.rm = TRUE),
    max_incidence = 100*max(incidence_per_capita, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(trnsm, threshold)

#####################

results %>%
  mutate(inc0.01 = ifelse(inc>0.01, 1, 1)) 
incidence_percentile_results %>%
  group_by(trnsm) %>%
  summarize(
    min_incidence_10 = 100*min(inc_at_10, na.rm = TRUE),
    max_incidence_25 = 100*max(inc_at_25, na.rm = TRUE),
    min_incidence_40 = 100*min(inc_at_40, na.rm = TRUE),
    max_incidence_40 = 100*max(inc_at_40, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(trnsm)

#####################

cumulative_prop_threshold0 <- 0.1
cumulative_prop_threshold1 <- 0.25
cumulative_prop_threshold2 <- 0.4

incidence_percentile_results <- results %>%
  group_by(trnsm, simulation) %>%
  summarize(
    inc_at_10 = approx(x = cumprop_popinc_trnsm, y = inc, xout = cumulative_prop_threshold0, method = "linear")$y,
    inc_at_25 = approx(x = cumprop_popinc_trnsm, y = inc, xout = cumulative_prop_threshold1, method = "linear")$y,
    inc_at_40 = approx(x = cumprop_popinc_trnsm, y = inc, xout = cumulative_prop_threshold2, method = "linear")$y,
    .groups = 'drop'
  )

incidence_percentile_results %>%
  group_by(trnsm) %>%
  summarize(
    min_incidence_25 = 100*min(inc_at_25, na.rm = TRUE),
    max_incidence_25 = 100*max(inc_at_25, na.rm = TRUE),
    min_incidence_40 = 100*min(inc_at_40, na.rm = TRUE),
    max_incidence_40 = 100*max(inc_at_40, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(trnsm)

#####################

incidence_percentile_results <- results %>%
  group_by(simulation) %>%
  summarize(
    inc_at_10 = approx(x = cumprop_popinc, y = inc, xout = cumulative_prop_threshold0, method = "linear")$y,
    inc_at_25 = approx(x = cumprop_popinc, y = inc, xout = cumulative_prop_threshold1, method = "linear")$y,
    inc_at_40 = approx(x = cumprop_popinc, y = inc, xout = cumulative_prop_threshold2, method = "linear")$y,
    .groups = 'drop'
  )

incidence_percentile_results %>%
  summarize(
    min_incidence_10 = 100*min(inc_at_10, na.rm = TRUE),
    max_incidence_10 = 100*max(inc_at_10, na.rm = TRUE),
    min_incidence_25 = 100*min(inc_at_25, na.rm = TRUE),
    max_incidence_25 = 100*max(inc_at_25, na.rm = TRUE),
    min_incidence_40 = 100*min(inc_at_40, na.rm = TRUE),
    max_incidence_40 = 100*max(inc_at_40, na.rm = TRUE),
    .groups = 'drop'
  )

# Join the results back to the original dataframe to calculate the proportion
results %>%
  inner_join(incidence_percentile_results, by = c("simulation")) %>%
  mutate(id0 = ifelse(inc >= inc_at_10, 1, 0),
         id1 = ifelse(inc >= inc_at_25, 1, 0),
         id2 = ifelse(inc >= inc_at_40, 1, 0),
         id3 = ifelse(inc >= 0.001, 1, 0),
         id4 = ifelse(inc >= 0.01, 1, 0)) %>%
  mutate(
    prep100 = prepind,
    prep10 = ifelse(id0 == 1 , prepind, 0),
    prep25 = ifelse(id1 == 1 , prepind, 0),
    prep40 = ifelse(id2 == 1 , prepind, 0),
    prep001 = ifelse(id3 == 1 , prepind, 0),
    prep01  = ifelse(id4 == 1 , prepind, 0)) %>%
  select(prep100, prep10, prep25, prep40, prep001, prep01, trnsm, simulation, state, age, race, sex, pop) -> test # %>%

test %>%
  group_by(trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep40 = sum(prep40, na.rm=T),
            sum_prep001= sum(prep001, na.rm=T),
            sum_prep01 = sum(prep01, na.rm=T),
            .groups = 'drop') %>%  
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(trnsm, Allocation) %>%
  summarize(
    median_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    .groups = 'drop' ) %>%
  mutate(Allocation = recode(Allocation,
                            "sum_prep100" = "100% coverage (PrEP indicators)",
                            "sum_prep10" = "Top 90% HIV infections",
                            "sum_prep25" = "Top 75% HIV infections",
                            "sum_prep40" = "Top 60% HIV infections",
                            "sum_prep001" = ">=0.1% incidence",
                            "sum_prep01" = ">=1% incidence")) %>%
 # filter(median_prep >0) %>%
  bind_rows(tibble(
    trnsm = c("pwid", "wsm", "msw", "msm"),
    Allocation = c("Cost-benefit estimate"),
    median_prep = c(187500, 382500, 150000, 1582500),
    lower_95 = c(187500, 382500, 150000, 1582500),
    upper_95 = c(187500, 382500, 150000, 1582500) )) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("100% coverage (PrEP indicators)",
                                        "Cost-benefit estimate",
                                        "Top 90% HIV infections", 
                                        "Top 75% HIV infections", 
                                        "Top 60% HIV infections",
                                        ">=0.1% incidence",
                                        ">=1% incidence" ))) %>%
  select(trnsm, Allocation, median_prep, lower_95, upper_95) %>%
  filter(Allocation != "100% coverage (PrEP indicators)") %>%
  ggplot(aes(x = trnsm, y = median_prep, fill = Allocation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), position = position_dodge(0.7),  width = 0.25) +
  labs(
    x = "Transmission risk group",
    y = "Number of people") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
  #  plot.background = element_rect(fill = "white") 
  ) +
  scale_y_continuous(labels = comma) + 
  scale_fill_brewer(palette = "BrBG") -> p3

ggsave(here("output_figures/epi-allocation-trnsm.png"), plot = p3, width = 16, height =6, dpi = 300)

#####################
## RACE/ETHNICITY

test %>%
  group_by(race, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep40 = sum(prep40, na.rm=T),
            sum_prep001= sum(prep001, na.rm=T),
            sum_prep01 = sum(prep01, na.rm=T),
            sum_pop = sum(pop),
            .groups = 'drop') %>%  
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(race, trnsm, Allocation) %>%
  summarize(
    median_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    pop = mean(sum_pop),
    .groups = 'drop' ) %>%
  filter(race != "All") %>% # PWID do not have R/E strata
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indicators)",
                             "sum_prep10" = "Top 90% HIV infections",
                             "sum_prep25" = "Top 75% HIV infections",
                             "sum_prep40" = "Top 60% HIV infections",
                             "sum_prep001" = ">=0.1% incidence",
                             "sum_prep01" = ">=1% incidence")) %>%
  # filter(median_prep >0) %>%
  # bind_rows(tibble(
  #   trnsm = c("pwid", "wsm", "msw", "msm"),
  #   Allocation = c("Cost-benefit estimate"),
  #   median_prep = c(187500, 382500, 150000, 1582500),
  #   lower_95 = c(187500, 382500, 150000, 1582500),
  #   upper_95 = c(187500, 382500, 150000, 1582500) )) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("100% coverage (PrEP indicators)",
                                        #"Cost-benefit estimate",
                                        "Top 90% HIV infections", 
                                        "Top 75% HIV infections", 
                                        "Top 60% HIV infections",
                                        ">=0.1% incidence",
                                        ">=1% incidence" ))) %>%
  select(race, trnsm, Allocation, median_prep, lower_95, upper_95, pop) %>%
  filter(Allocation != "100% coverage (PrEP indicators)") %>%
  ggplot(aes(x = race, y = 100*median_prep/pop, fill = Allocation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = 100*lower_95/pop, ymax = 100*upper_95/pop), position = position_dodge(0.7),  width = 0.25) +
  facet_wrap(~trnsm) + 
  labs(
    x = "Race/ethnicity",
    y = "Benefit from PrEP per 100 persons") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    #  plot.background = element_rect(fill = "white") 
  ) +
  scale_y_continuous(labels = comma) + 
  scale_fill_brewer(palette = "BrBG") -> p4

ggsave(here("output_figures/epi-allocation-race-rate.png"), plot = p4, width = 16, height =6, dpi = 300)

#####################
## STATE

test %>%
  group_by(state, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep40 = sum(prep40, na.rm=T),
            sum_prep001= sum(prep001, na.rm=T),
            sum_prep01 = sum(prep01, na.rm=T),
            sum_pop = sum(pop),
            .groups = 'drop') %>%  
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(state, trnsm, Allocation) %>%
  summarize(
    median_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    pop = mean(sum_pop),
    .groups = 'drop' ) %>%
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indicators)",
                             "sum_prep10" = "Top 90% HIV infections",
                             "sum_prep25" = "Top 75% HIV infections",
                             "sum_prep40" = "Top 60% HIV infections",
                             "sum_prep001" = ">=0.1% incidence",
                             "sum_prep01" = ">=1% incidence")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("100% coverage (PrEP indicators)",
                                        #"Cost-benefit estimate",
                                        "Top 90% HIV infections", 
                                        "Top 75% HIV infections", 
                                        "Top 60% HIV infections",
                                        ">=0.1% incidence",
                                        ">=1% incidence" ))) %>%
  select(state, trnsm, Allocation, median_prep, lower_95, upper_95, pop) %>%
  filter(Allocation != "100% coverage (PrEP indicators)") %>%
  ggplot() + # y=reorder(y=state, median_prep)
  geom_point(aes(y=reorder(state, median_prep),  x=100*median_prep/pop, color=Allocation), position=position_dodge2(width = 1)) +
  geom_linerange(aes(y=reorder(state, median_prep), xmin=100*lower_95/pop, xmax=100*upper_95/pop, color=Allocation), 
                    position=position_dodge2(width = 1), size = 1, alpha = 8/10) +
  # geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  # geom_errorbar(aes(ymin = lower_95, ymax = upper_95), position = position_dodge(0.7),  width = 0.25) +
  facet_wrap(~trnsm, ncol=4) + 
   labs(
    x = "Race/ethnicity",
    y = "Number of people") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white") 
  ) +
  scale_x_continuous(labels = comma) + 
  scale_fill_brewer(palette = "BrBG") -> p5

ggsave(here("output_figures/epi-allocation-state.png"), plot = p5, width = 18, height =12, dpi = 300)

#####################
## AGE

test %>%
  group_by(age, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep40 = sum(prep40, na.rm=T),
            sum_prep001= sum(prep001, na.rm=T),
            sum_prep01 = sum(prep01, na.rm=T),
            sum_pop = sum(pop),
            .groups = 'drop') %>%  
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(age, trnsm, Allocation) %>%
  summarize(
    median_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    pop = mean(sum_pop),
    .groups = 'drop' ) %>%
  filter(age != "All") %>% # PWID do not have age strata
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indicators)",
                             "sum_prep10" = "Top 90% HIV infections",
                             "sum_prep25" = "Top 75% HIV infections",
                             "sum_prep40" = "Top 60% HIV infections",
                             "sum_prep001" = ">=0.1% incidence",
                             "sum_prep01" = ">=1% incidence")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("100% coverage (PrEP indicators)",
                                        #"Cost-benefit estimate",
                                        "Top 90% HIV infections", 
                                        "Top 75% HIV infections", 
                                        "Top 60% HIV infections",
                                        ">=0.1% incidence",
                                        ">=1% incidence" ))) %>%
  select(age, trnsm, Allocation, median_prep, lower_95, upper_95, pop) %>%
  filter(Allocation != "100% coverage (PrEP indicators)") %>%
  ggplot(aes(x = age, y = median_prep, fill = Allocation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), position = position_dodge(0.7),  width = 0.25) +
  labs(
    x = "Age",
    y = "Number of people") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    #  plot.background = element_rect(fill = "white") 
  ) +
  facet_wrap(~trnsm, ncol=4) + 
  scale_y_continuous(labels = comma) + 
  scale_fill_brewer(palette = "BrBG") -> p4

ggsave(here("output_figures/epi-allocation-race.png"), plot = p4, width = 16, height =6, dpi = 300)

