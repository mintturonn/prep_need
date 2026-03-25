

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
source("~/prep_need/code/results_compile_fun.R")
source("~/prep_need/code/cdc_prep_need.R")

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

results %>%
  ggplot(aes(x=100*inc, y=100*cumprop_popinc_trnsm, identity=simulation)) +
  geom_line(size=0.2, color="gray40") +
  facet_wrap(~trnsm, scales = "free", ncol=4) + theme_minimal() +
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
  ) -> p0

ggsave(here("output_figures/cumulative_hiv_suppl.png"), plot = p0, width = 16, height = 4, dpi = 300)


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

results %>%
  ggplot(aes(x=100*inc, y=100*cumprop_prepind_trnsm, identity=simulation)) +
  geom_line(size=0.2, color="gray40") +
  facet_wrap(~trnsm, scales = "free", ncol=4) + theme_minimal() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  theme(legend.position = "none") +
  labs(
    x = "Average HIV incidence per 100 persons",
    y = "Cumulative percent of population",
    color = "Simulation") +
  theme(
    #panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) -> p1

ggsave(here("output_figures/cumulative_population_size_suppl.png"), plot = p1, width = 16, height = 4, dpi = 300)


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
results %>%
ggplot( aes(x = 100*cumprop_prepind_trnsm, y = 100*cumprop_popinc_trnsm, identity=simulation)) +
  geom_line(size=0.2, color="gray40") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Line of equality
  facet_wrap(~trnsm,  ncol=4) + theme_minimal() +
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
  ) -> p2

ggsave(here("output_figures/lorenz_curve_suppl.png"), plot = p2, width = 16, height = 4, dpi = 300)



results %>%
  ggplot( aes(x = 100*cumprop_prepind, y = 100*cumprop_popinc, identity=simulation)) +
  geom_line(size=0.2, color="gray40") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Line of equality
 # facet_wrap(~trnsm,  ncol=4) + theme_minimal() +
  labs(
    x = "Cumulative Population Percentage",
    y = "Cumulative HIV Infections Percentage"
  ) +
  theme(
    #panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black") ,
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) -> p2.1

ggsave(here("output_figures/lorenz_curve_total.png"), plot = p2.1, width = 5, height = 4, dpi = 300)

#####################
# 
# # Use the results data frame to calculate incidence per capita values for specific cumulative proportion thresholds
# results %>%
#   group_by(trnsm, simulation) %>%
#   summarize(
#     inc_at_0.1 = approx(x = cumprop_prepind_trnsm, y = inc, xout = 0.1, method = "linear")$y,
#     inc_at_0.4 = approx(x = cumprop_prepind_trnsm, y = inc, xout = 0.4, method = "linear")$y,
#     inc_at_0.5 = approx(x = cumprop_prepind_trnsm, y = inc, xout = 0.5, method = "linear")$y,
#     inc_at_0.9 = approx(x = cumprop_prepind_trnsm, y = inc, xout = 0.9, method = "linear")$y
#   ) %>%
#   pivot_longer(cols = c(inc_at_0.1, inc_at_0.4, inc_at_0.5, inc_at_0.9),
#                names_to = "threshold", values_to = "incidence_per_capita") %>%
#   arrange(trnsm, simulation, threshold) -> incidence_quantiles_pop
# 
# # Summarize the ranges of incidence per capita corresponding to each cumulative proportion threshold
# incidence_quantiles_pop %>%
# #incidence_quantiles_hiv %>%
#   group_by(trnsm, threshold) %>%
#   summarize(
#     min_incidence = 100*min(incidence_per_capita, na.rm = TRUE),
#     max_incidence = 100*max(incidence_per_capita, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   arrange(trnsm, threshold)

#####################
#  ?????????????
# results %>%
#   mutate(inc0.01 = ifelse(inc>0.01, 1, 1)) 
# incidence_percentile_results %>%
#   group_by(trnsm) %>%
#   summarize(
#     min_incidence_10 = 100*min(inc_at_10, na.rm = TRUE),
#     max_incidence_25 = 100*max(inc_at_25, na.rm = TRUE),
#     min_incidence_40 = 100*min(inc_at_40, na.rm = TRUE),
#     max_incidence_40 = 100*max(inc_at_40, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   arrange(trnsm)

#####################

## This is by transmission risk group! 

cumulative_prop_threshold0 <- 0.1
cumulative_prop_threshold1 <- 0.25
cumulative_prop_threshold2 <- 0.4

incidence_percentile_results0 <- results %>%
  group_by(trnsm, simulation) %>%
  summarize(
    inc_at_10 = approx(x = cumprop_popinc_trnsm, y = inc, xout = cumulative_prop_threshold0, method = "linear")$y,
    inc_at_25 = approx(x = cumprop_popinc_trnsm, y = inc, xout = cumulative_prop_threshold1, method = "linear")$y,
    inc_at_40 = approx(x = cumprop_popinc_trnsm, y = inc, xout = cumulative_prop_threshold2, method = "linear")$y,
    .groups = 'drop'
  )

incidence_percentile_results0 %>%
  group_by(trnsm) %>%
  summarize(
    min_incidence_10 = 100*min(inc_at_10, na.rm = TRUE),
    max_incidence_10 = 100*max(inc_at_10, na.rm = TRUE),
    min_incidence_25 = 100*min(inc_at_25, na.rm = TRUE),
    max_incidence_25 = 100*max(inc_at_25, na.rm = TRUE),
    min_incidence_40 = 100*min(inc_at_40, na.rm = TRUE),
    max_incidence_40 = 100*max(inc_at_40, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(trnsm)



#####################
## This is total

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
         id0.1 = ifelse(cumprop_popinc >= 0.1, 1, 0), # check the inc_at_10
         id1.1 = ifelse(cumprop_popinc >= 0.25, 1, 0), # check the inc_at_25
         id3 = ifelse(inc >= 0.01, 1, 0),
         id4 = ifelse(inc >= 0.02, 1, 0),
         id5 = ifelse(inc >= 0.04, 1, 0)) %>%
  mutate(
    prep100 = prepind,
    prep10 = ifelse(id0 == 1 , prepind,0),
    prep10t = ifelse(id0.1 == 1 , prepind,0),
    prep10.2 = ifelse(id0 == 1 , prepind, 0.05*prepind),
    prep25 = ifelse(id1 == 1 , prepind, 0),
    prep25t = ifelse(id1.1 == 1 , prepind, 0),
    prep25.2 = ifelse(id1 == 1 , prepind, 0.05*prepind),
    prep01 = ifelse(id3 == 1 , prepind, 0),
    prep02  = ifelse(id4 == 1 , prepind, 0),
    prep04  = ifelse(id5 == 1 , prepind, 0)) %>%
  select(prep100, prep10,prep10t, prep10.2, prep25, prep25t, prep25.2, prep01,  prep02,  prep04, 
         trnsm, simulation, state, age, race, sex, pop) -> results2 

####################################

results2 %>%
  group_by(trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10t = sum(prep10t, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25t = sum(prep25t, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_prep01= sum(prep01, na.rm=T),
            sum_prep02 = sum(prep02, na.rm=T),
            sum_prep04 = sum(prep04, na.rm=T)) %>%  
  ungroup() -> nums_tab

# Number by transmission risk group
results2 %>%
  group_by(trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T)) %>%  
  ungroup() %>%
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(trnsm, Allocation) %>%
  summarize(
    mean_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE) ) %>%
  ungroup() %>%
  mutate(Allocation = recode(Allocation,
                            "sum_prep100" = "100% coverage (PrEP indications)",
                            "sum_prep10" = "100% to top 90% HIV infections",
                            "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                            "sum_prep25" = "100% to top 75% HIV infections",
                            "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  ungroup() %>%
  bind_rows(tibble(
    trnsm = c("pwid", "wsm", "msw", "msm"),
    Allocation = c("Cost-benefit estimate"),
    mean_prep = c(187500, 382500, 150000, 1582500),
    lower_95 = c(NA, NA, NA, NA),
    upper_95 = c(NA, NA, NA, NA) )) %>%
  bind_rows(tibble(
    trnsm = c("pwid", "wsm", "msw", "msm"),
    Allocation = c("Former CDC estimate"),
    mean_prep = c(sum(cdc_prep_need$pwid),sum(cdc_prep_need$wsm),sum(cdc_prep_need$msw),sum(cdc_prep_need$msm)),
    lower_95 = c(NA, NA, NA, NA),
    upper_95 = c(NA, NA, NA, NA) )) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(trnsm, Allocation, mean_prep, lower_95, upper_95) -> df_trnsm1

df_trnsm1  %>%
  mutate(
    mean_prep_fmt = format_sigfig(mean_prep),
    lower_95_fmt = format_sigfig(lower_95),
    upper_95_fmt = format_sigfig(upper_95) ) %>%
  mutate(  mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) %>%
  arrange(trnsm, Allocation) %>%
  select(`Transmission Risk` = trnsm, Allocation, `mean (95% UI)` = mean_CI) %>%
  pivot_wider(id_cols = "Transmission Risk", names_from = "Allocation", values_from = "mean (95% UI)") -> df_trnsm1_tab
  
write_csv(df_trnsm1_tab, "~/prep_need/output_data/trnsm_num_tab.csv")

  
df_trnsm1  %>%
  filter(Allocation != "100% coverage (PrEP indications)") %>%
  mutate(trnsm = factor(trnsm, levels = c("msm", "pwid", "wsm", "msw"))) %>%
  ggplot(aes(x = trnsm, y = mean_prep, fill = Allocation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), position = position_dodge(0.7),  width = 0.25) +
  labs(
    x = "Transmission risk group",
    y = "Number of people") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    axis.text.y = element_text( hjust = 1, size=16),
    axis.title = element_text(size=16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16), 
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white") 
  )  +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#8C510A", "#DFC27D", "#BDD7E7","#3182BD", "#FDAE61", "#F46D43") ) -> p3 #"#D73027", 

ggsave(here("output_figures/epi-allocation-trnsm.png"), plot = p3, width = 16, height =6, dpi = 300)

#######################
# Rate
        
results2 %>%
  mutate(prep.1 = case_when(
                trnsm == "pwid" & sex=="Male" ~ pnat[as.numeric(simulation), "prop.pwid.m"], # this is to get PWID population right
                trnsm == "pwid" & sex=="Female" ~ pnat[as.numeric(simulation), "prop.pwid.f"],
                trnsm == "msm" ~ 1,
                trnsm == "msw" ~ 1,
                trnsm == "wsm" ~ 1,  
                TRUE ~ NA_real_),
         pop2 = prep.1*pop) %>%
  group_by(trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop2)) %>%  
  ungroup() %>%
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(trnsm, Allocation) %>%
  summarize(
    mean_prep = mean(prep_value/sum_pop, na.rm = TRUE),
    lower_95 = quantile(prep_value/sum_pop, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value/sum_pop, 0.975, na.rm = TRUE),
    pop = mean(sum_pop) ) %>%
  ungroup() %>%
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indications)",
                             "sum_prep10" = "100% to top 90% HIV infections",
                             "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                             "sum_prep25" = "100% to top 75% HIV infections",
                             "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  ungroup() %>%
  bind_rows(tibble(
    trnsm = c("pwid", "wsm", "msw", "msm"),
    Allocation = c("Cost-benefit estimate"),
    mean_prep = c(187500, 382500, 150000, 1582500),
    lower_95 = c(NA, NA, NA, NA),
    upper_95 = c(NA, NA, NA, NA) )) %>%
  bind_rows(tibble(
    trnsm = c("pwid", "wsm", "msw", "msm"),
    Allocation = c("Former CDC estimate"),
    mean_prep = c(sum(cdc_prep_need$pwid),sum(cdc_prep_need$wsm),sum(cdc_prep_need$msw),sum(cdc_prep_need$msm)),
    lower_95 = c(NA, NA, NA, NA),
    upper_95 = c(NA, NA, NA, NA) )) %>%
  group_by(trnsm) %>%
  fill(pop, .direction = "downup") %>%
  ungroup() %>%
  mutate(mean_prep = ifelse(Allocation == "Cost-benefit estimate" | Allocation == "Former CDC estimate", mean_prep/pop, mean_prep )) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(trnsm, Allocation, mean_prep, lower_95, upper_95, pop) -> df_trnsm2

df_trnsm2  %>%
  mutate(
    mean_prep_fmt = format_sigfig2(100*mean_prep),
    lower_95_fmt = format_sigfig2(100*lower_95),
    upper_95_fmt = format_sigfig2(100*upper_95) ) %>%
  mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) %>%
  arrange(trnsm, Allocation) %>%
  select(`Transmission Risk` = trnsm, Allocation, `mean (95% UI)` = mean_CI) %>%
  pivot_wider(id_cols = "Transmission Risk", names_from = "Allocation", values_from = "mean (95% UI)") -> df_trnsm2_tab

write_csv(df_trnsm2_tab, "~/prep_need/output_data/trnsm_rate_tab.csv")


df_trnsm2  %>%
  # filter(Allocation != "100% coverage (PrEP indications)") %>%
  mutate(trnsm = factor(trnsm, levels = c("msm", "pwid", "wsm", "msw"))) %>%
  ggplot(aes(x = trnsm, y = 100*mean_prep, fill = Allocation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = 100*lower_95, ymax = 100*upper_95), position = position_dodge(0.7),  width = 0.25) +
  labs(
    x = "Transmission risk group",
    y = "Rate per 100 people") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    axis.text.y = element_text( hjust = 1, size=16),
    axis.title = element_text(size=16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16), 
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white") 
  )  +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#8C510A", "#DFC27D", "purple", "#BDD7E7","#3182BD", "#FDAE61", "#F46D43" )) -> p3.2

ggsave(here("output_figures/epi-allocation-trnsm-rate.png"), plot = p3.2, width = 16, height =6, dpi = 300)

##########################
## RACE/ETHNICITY

results2 %>%
  group_by(race, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop),
            .groups = 'drop') %>%  
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(race, trnsm, Allocation) %>%
  summarize(
    mean_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    pop = mean(sum_pop)) %>%
  ungroup() %>%
  filter(race != "All") %>% # PWID do not have R/E strata
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indications)",
                             "sum_prep10" = "100% to top 90% HIV infections",
                             "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                             "sum_prep25" = "100% to top 75% HIV infections",
                             "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(race, trnsm, Allocation, mean_prep, lower_95, upper_95, pop) -> df_race

df_race  %>%
  mutate(
    mean_prep_fmt = format_sigfig(mean_prep),
    lower_95_fmt = format_sigfig(lower_95),
    upper_95_fmt = format_sigfig(upper_95) ) %>%
  mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) %>%
  arrange(trnsm, Allocation) %>%
  select(`Transmission Risk` = trnsm, `Race/Ethnicity`=race, Allocation, `mean (95% UI)` = mean_CI)  %>%
  pivot_wider(id_cols = c("Transmission Risk", "Race/Ethnicity"), names_from = "Allocation", values_from = "mean (95% UI)") -> df_race_tab

write_csv(df_race_tab, "~/prep_need/output_data/race_tab.csv")

df_race %>%
   filter(Allocation != "100% coverage (PrEP indications)") %>%
  mutate(trnsm = factor(trnsm, levels = c("msm", "pwid", "wsm", "msw"))) %>%
  ggplot(aes(x = race, y = mean_prep, fill = Allocation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), position = position_dodge(0.7),  width = 0.25) +
  facet_wrap(~trnsm) + 
  labs(
    x = "Race/ethnicity",
    y = "Number of people") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    axis.text.y = element_text( hjust = 1, size=16),
    axis.title = element_text(size=16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16), 
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white") 
  ) +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c( "#BDD7E7","#3182BD", "#FDAE61", "#F46D43" ) )  -> p4.2

ggsave(here("output_figures/epi-allocation-race.png"), plot = p4.2, width = 16, height =6, dpi = 300)

#####################
## RACE/ETHNICITY RATE

results2 %>%
  group_by(race, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop),
            .groups = 'drop') %>%  
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(race, trnsm, Allocation) %>%
  summarize(
    mean_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    pop = mean(sum_pop) ) %>%
  ungroup() %>%
  filter(race != "All") %>% # PWID do not have R/E strata
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indications)",
                             "sum_prep10" = "100% to top 90% HIV infections",
                             "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                             "sum_prep25" = "100% to top 75% HIV infections",
                             "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(race, trnsm, Allocation, mean_prep, lower_95, upper_95, pop) -> df_race2

df_race  %>%
  mutate(
    mean_prep_fmt = format_sigfig2(100*mean_prep/pop),
    lower_95_fmt = format_sigfig2(100*lower_95/pop),
    upper_95_fmt = format_sigfig2(100*upper_95/pop) ) %>%
  mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) %>%
  arrange(trnsm, Allocation) %>%
  select(`Transmission Risk` = trnsm,  `Race/Ethnicity`=race, Allocation, `mean (95% UI)` = mean_CI)  %>%
  pivot_wider(id_cols =  c("Transmission Risk", "Race/Ethnicity"), names_from = "Allocation", values_from = "mean (95% UI)") -> df_race2_tab

write_csv(df_race2_tab, "~/prep_need/output_data/race_rate_tab.csv")

df_race2 %>%
  #filter(Allocation != "100% coverage (PrEP indications)") %>%
  mutate(trnsm = factor(trnsm, levels = c("msm", "pwid", "wsm", "msw"))) %>%
  ggplot(aes(x = race, y = 100*mean_prep/pop, fill = Allocation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = 100*lower_95/pop, ymax = 100*upper_95/pop), position = position_dodge(0.7),  width = 0.25) +
  facet_wrap(~trnsm) + 
  labs(
    x = "Race/ethnicity",
    y = "Rate per 100 people") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    axis.text.y = element_text( hjust = 1, size=16),
    axis.title = element_text(size=16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16), 
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white") 
  ) +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("purple","#BDD7E7","#3182BD", "#FDAE61", "#F46D43" ) ) -> p4.1

ggsave(here("output_figures/epi-allocation-race-rate.png"), plot = p4.1, width = 16, height =6, dpi = 300)



#####################
## STATE

## Number
cdc_prep_need %>% 
  select(state, msm, pwid, wsm, msw) %>%
  pivot_longer(cols = c("msm", "pwid", "wsm", "msw"), names_to = "trnsm", values_to = "mean_prep") %>%
  mutate(lower_95 = NA,
         upper_95 = NA,
         Allocation = "Former CDC estimate") %>%
  group_by(state, Allocation) %>%
  summarize(mean_prep = sum(mean_prep),
            lower_95 = sum(lower_95),
            upper_95 = sum(upper_95)) %>%
  ungroup() -> cdc_st_sum

cdc_prep_need %>% 
  mutate(msm = dipr_msm_st * 1582500, # this overrides existing vars w/ same name
         wsm = dipr_het_f_st * 382500,
         msw = dipr_het_m_st * 150000,
         pwid = dipr_pwid_st * 187500 ) %>%
  select(state, msm, pwid, wsm, msw) %>%
  pivot_longer(cols = c("msm", "pwid", "wsm", "msw"), names_to = "trnsm", values_to = "mean_prep") %>%
  mutate(lower_95 = NA,
         upper_95 = NA,
         Allocation = "Cost-benefit estimate")  %>%
  group_by(state, Allocation) %>%
  summarize(mean_prep = sum(mean_prep),
            lower_95 = sum(lower_95),
            upper_95 = sum(upper_95)) %>%
  ungroup() -> cb_st_sum

results2 %>%
  group_by(state,  simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop)) %>%  
  ungroup() %>%
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(state, Allocation) %>%
  summarize(
    mean_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
   # pop = mean(sum_pop),
     ) %>%
  ungroup() %>%
  bind_rows(cdc_st_sum) %>%
  bind_rows(cb_st_sum) %>%
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indications)",
                             "sum_prep10" = "100% to top 90% HIV infections",
                             "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                             "sum_prep25" = "100% to top 75% HIV infections",
                             "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(state, Allocation, mean_prep, lower_95, upper_95) -> df_state  # trnsm, 

df_state  %>%
  mutate(
    mean_prep_fmt = format_sigfig2(mean_prep),
    lower_95_fmt = format_sigfig2(lower_95),
    upper_95_fmt = format_sigfig2(upper_95) ) %>%
  mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) %>%
  arrange( Allocation) %>% # trnsm, `Transmission Risk` = trnsm, 
  select(State=state, Allocation, `mean (95% UI)` = mean_CI)  %>%
  pivot_wider(id_cols = c( "State"), names_from = "Allocation", values_from = "mean (95% UI)") -> df_state_tab

write_csv(df_state_tab, "~/prep_need/output_data/state_tab.csv")



df_state %>%
  filter(Allocation != "100% coverage (PrEP indications)") %>%
  # filter(Allocation != "Top 60% HIV infections") %>%
  # filter(Allocation != ">=1% incidence") %>%
  # mutate(trnsm = factor(trnsm, levels = c("msm", "pwid", "wsm", "msw"))) %>%
  ggplot() + # y=reorder(y=state, mean_prep)
  geom_point(aes(y=reorder(state, mean_prep),  x=mean_prep, color=Allocation), size=3, position=position_dodge2(width = 0.3)) +
  geom_linerange(aes(y=reorder(state, mean_prep), xmin=lower_95, xmax=upper_95, color=Allocation), 
                    position=position_dodge2(width = 0.3), size = 1, alpha=0.7) +
  # facet_wrap(~trnsm, ncol=4, scales = "free_x") + 
   labs( x = "Number of people (log scale)", y = "") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    axis.text.y = element_text( hjust = 1, size=14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "black", size = 0.05),
    panel.grid.minor.x = element_line(color = "black", size = 0.05),
    panel.grid.major.y = element_line(color = "black", size = 0.05)) +
  scale_x_log10(labels = comma) +
#  scale_x_continuous(labels = comma, limits = c(0, NA)) + 
  scale_color_manual(values = c("#8C510A", "#DFC27D", "#BDD7E7","#3182BD", "#FDAE61", "#F46D43" ) )  -> p5

ggsave(here("output_figures/epi-allocation-state.png"), plot = p5, width = 13, height =20, dpi = 300)


## State rate

results2 %>%
  mutate(prep.1 = case_when(
    trnsm == "pwid" & sex=="Male" ~ pnat[as.numeric(simulation), "prop.pwid.m"],
    trnsm == "pwid" & sex=="Female" ~ pnat[as.numeric(simulation), "prop.pwid.f"],
    trnsm == "msm" ~ 1,
    trnsm == "msw" ~ 1,
    trnsm == "wsm" ~ 1,  
    TRUE ~ NA_real_),
    pop2 = prep.1*pop) %>%
  group_by(state,  simulation) %>% # collapse trnsm
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop2)) %>%  
  ungroup() %>% 
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(state, Allocation) %>% # collapse trnsm
  summarize(
    mean_prep = mean(prep_value/sum_pop, na.rm = TRUE),
    lower_95 = quantile(prep_value/sum_pop, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value/sum_pop, 0.975, na.rm = TRUE),
    pop = mean(sum_pop)) %>%
  ungroup() %>%
  bind_rows(cdc_st_sum) %>%
  bind_rows(cb_st_sum)  %>%
  group_by(state) %>% # trnsm
  fill(pop, .direction = "downup") %>%
  ungroup() %>%
  mutate(mean_prep = ifelse(Allocation == "Cost-benefit estimate" | Allocation == "Former CDC estimate", mean_prep/pop, mean_prep )) %>%
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indications)",
                             "sum_prep10" = "100% to top 90% HIV infections",
                             "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                             "sum_prep25" = "100% to top 75% HIV infections",
                             "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(state, Allocation, mean_prep, lower_95, upper_95, pop) -> df_state2

df_state2  %>%
  mutate(
    mean_prep_fmt = format_sigfig2(100*mean_prep),
    lower_95_fmt = format_sigfig2(100*lower_95),
    upper_95_fmt = format_sigfig2(100*upper_95) ) %>%
  mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) %>%
  arrange(Allocation) %>%
  select(State=state,Allocation, `mean (95% UI)` = mean_CI)  %>% # `Transmission Risk` = trnsm, 
  pivot_wider(id_cols = c("State"), names_from = "Allocation", values_from = "mean (95% UI)") -> df_state2_tab

write_csv(df_state2_tab, "~/prep_need/output_data/state_rate_tab.csv")


df_state2 %>% 
  left_join(df_state[,c("state","Allocation","mean_prep")], by=c("state", "Allocation")) %>%
  #filter(Allocation != "100% coverage (PrEP indications)") %>%
  # filter(Allocation != "Top 60% HIV infections") %>%
  # filter(Allocation != ">=1% incidence") %>%
  # mutate(trnsm = factor(trnsm, levels = c("msm", "pwid", "wsm", "msw"))) %>%
  ggplot() + 
  geom_point(aes(y=reorder(state, mean_prep.y),  x=mean_prep.x, color=Allocation), size=3, position=position_dodge2(width = 0.5)) +
  geom_linerange(aes(y=reorder(state, mean_prep.y), xmin=lower_95, xmax=upper_95, color=Allocation), 
                 position=position_dodge2(width = 0.5), size = 1, alpha=0.7) +
  # facet_wrap(~trnsm, ncol=4, scales = "free_x") + 
  labs(x = "Rate per 100 people", y = "") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    axis.text.y = element_text( hjust = 1, size=14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "black", size = 0.05),
    panel.grid.minor.x = element_line(color = "black", size = 0.05),
    panel.grid.major.y = element_line(color = "black", size = 0.05)) +
  scale_x_continuous(labels = comma) + 
  scale_color_manual(values = c( "#8C510A", "#DFC27D", "purple", "#BDD7E7","#3182BD", "#FDAE61", "#F46D43" ))  -> p5.2

ggsave(here("output_figures/epi-allocation-state-rate.png"), plot = p5.2, width = 13, height =20, dpi = 300)

#####################
## AGE

results2 %>%
  group_by(age, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop)) %>%
  ungroup() %>%
  filter(age=="13-24") -> ageres0

ageres0 %>%
  mutate( prep.young = pnat[as.numeric(simulation), "prep.young"],
          prep.1 = case_when(
                            trnsm == "msm" ~ pnat[as.numeric(simulation), "prep.msm1"],
                            trnsm == "msw" ~ pnat[as.numeric(simulation), "prep.msw1"],
                            trnsm == "wsm" ~ pnat[as.numeric(simulation), "prep.wsm1"],  # Corrected column name
                            TRUE ~ NA_real_)) %>%
  mutate( sum_prep100_adj = sum_prep100 - (7/12) * prep.1 * sum_pop, # prep need in 13-17 yos
          sum_prep10    = sum_prep10 * sum_prep100_adj/sum_prep100,
          sum_prep10.2  = sum_prep10.2 * sum_prep100_adj/sum_prep100,
          sum_prep25    = sum_prep25 * sum_prep100_adj/sum_prep100,
          sum_prep25.2  = sum_prep25.2 * sum_prep100_adj/sum_prep100,
          sum_pop     = (7/12) * sum_pop,
          age = "13-17",
          sum_prep100=sum_prep100_adj) %>%
  select(-sum_prep100_adj, -prep.young, -prep.1) -> age_13_17

ageres0 %>%
  filter(age=="13-24") %>%
  mutate( prep.young = pnat[as.numeric(simulation), "prep.young"],
          prep.1 = case_when(
            trnsm == "msm" ~ pnat[as.numeric(simulation), "prep.msm1"],
            trnsm == "msw" ~ pnat[as.numeric(simulation), "prep.msw1"],
            trnsm == "wsm" ~ pnat[as.numeric(simulation), "prep.wsm1"],  # Corrected column name
            TRUE ~ NA_real_)) %>%
  mutate( sum_prep100_adj = sum_prep100 - (5/12) * prep.young * sum_pop,
          sum_prep10      = sum_prep10  * sum_prep100_adj/sum_prep100,
          sum_prep10.2    = sum_prep10.2 * sum_prep100_adj/sum_prep100,
          sum_prep25      = sum_prep25  * sum_prep100_adj/sum_prep100,
          sum_prep25.2    = sum_prep25.2 * sum_prep100_adj/sum_prep100,
          age = "18-24",
          sum_prep100=sum_prep100_adj ) %>%
  select(-sum_prep100_adj, -prep.young, -prep.1) -> age_18_24

## Check
# bind_rows(age_13_17, age_18_24) %>% 
#   group_by( simulation, trnsm) %>%
#   summarize(
#     sum_prep100 = mean(sum_prep100),
#     sum_prep100_adj = sum(sum_prep100_adj),
#     sum_prep10 = sum(sum_prep10),
#     sum_prep25 = sum(sum_prep25),
#     sum_prep40 = sum(sum_prep40),
#     sum_prep001 = sum(sum_prep001),
#     sum_prep01 = sum(sum_prep01)) -> check
#  
#  sum(ageres0$sum_prep100-check$sum_prep100)
#  sum(ageres0$sum_prep10-check$sum_prep10)
#  sum(ageres0$sum_prep25-check$sum_prep25)
#  sum(ageres0$sum_prep40-check$sum_prep40)
#  sum(ageres0$sum_prep001-check$sum_prep001)
#  sum(ageres0$sum_prep01-check$sum_prep01)
 
results2 %>%
  group_by(age, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop)) %>%
  ungroup() %>%
  filter(age!="13-24") %>%
  bind_rows(age_13_17, age_18_24) %>% 
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(age, trnsm, Allocation) %>%
  summarize(
    mean_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    pop = mean(sum_pop),
    .groups = 'drop' ) %>%
  filter(age != "All") %>% # PWID do not have age strata
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indications)",
                             "sum_prep10" = "100% to top 90% HIV infections",
                             "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                             "sum_prep25" = "100% to top 75% HIV infections",
                             "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(age, trnsm, Allocation, mean_prep, lower_95, upper_95, pop) -> df_age

df_age  %>%
  mutate(
    mean_prep_fmt = format_sigfig(mean_prep),
    lower_95_fmt = format_sigfig(lower_95),
    upper_95_fmt = format_sigfig(upper_95) ) %>%
  mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) %>%
  arrange(trnsm, Allocation) %>%
  select(`Transmission Risk` = trnsm, Age = age, Allocation, `mean (95% UI)` = mean_CI)  %>%
  pivot_wider(id_cols = c("Transmission Risk", "Age"), names_from = "Allocation", values_from = "mean (95% UI)") -> df_age_tab

write_csv(df_age_tab, "~/prep_need/output_data/age_tab.csv")

df_age %>%
   filter(Allocation != "100% coverage (PrEP indications)") %>%
  ggplot(aes(x = age, y = mean_prep, fill = Allocation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), position = position_dodge(0.7),  width = 0.25) +
  labs(
    x = "Age",
    y = "Number of people") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    axis.text.y = element_text( hjust = 1, size=16),
    axis.title = element_text(size=16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16), 
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white") 
  ) +
  facet_wrap(~trnsm, ncol=4) + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#BDD7E7","#3182BD", "#FDAE61", "#F46D43" ) ) -> p4

ggsave(here("output_figures/epi-allocation-age.png"), plot = p4, width = 16, height =6, dpi = 300)

##########################
## Age rate

results2 %>%
  group_by(age, trnsm, simulation) %>%
  summarize(sum_prep100= sum(prep100, na.rm=T),
            sum_prep10 = sum(prep10, na.rm=T),
            sum_prep10.2 = sum(prep10.2, na.rm=T),
            sum_prep25 = sum(prep25, na.rm=T),
            sum_prep25.2 = sum(prep25.2, na.rm=T),
            sum_pop = sum(pop)) %>%
  ungroup() %>%
  filter(age!="13-24") %>%
  bind_rows(age_13_17, age_18_24) %>% 
  pivot_longer(cols = starts_with("sum_prep"), names_to = "Allocation", values_to = "prep_value") %>%
  group_by(age, trnsm, Allocation) %>%
  summarize(
    mean_prep = mean(prep_value, na.rm = TRUE),
    lower_95 = quantile(prep_value, 0.025, na.rm = TRUE),
    upper_95 = quantile(prep_value, 0.975, na.rm = TRUE),
    pop = mean(sum_pop),
    .groups = 'drop' ) %>%
  filter(age != "All") %>% # PWID do not have age strata
  mutate(Allocation = recode(Allocation,
                             "sum_prep100" = "100% coverage (PrEP indications)",
                             "sum_prep10" = "100% to top 90% HIV infections",
                             "sum_prep10.2" = "100% to top 90%, 5% to bottom 10%",
                             "sum_prep25" = "100% to top 75% HIV infections",
                             "sum_prep25.2" = "100% to top 75%, 5% to bottom 25%")) %>%
  mutate(Allocation = factor(Allocation, 
                             levels = c("Former CDC estimate",
                                        "Cost-benefit estimate",
                                        "100% coverage (PrEP indications)",
                                        "100% to top 90% HIV infections",
                                        "100% to top 90%, 5% to bottom 10%",
                                        "100% to top 75% HIV infections",
                                        "100% to top 75%, 5% to bottom 25%"))) %>%
  select(age, trnsm, Allocation, mean_prep, lower_95, upper_95, pop) -> df_age2

df_age2  %>%
  mutate(
    mean_prep_fmt = format_sigfig2(100*mean_prep/pop),
    lower_95_fmt = format_sigfig2(100*lower_95/pop),
    upper_95_fmt = format_sigfig2(100*upper_95/pop) ) %>%
  mutate( mean_CI = sprintf("%s (%s-%s)", mean_prep_fmt, lower_95_fmt, upper_95_fmt) ) %>%
  arrange(trnsm, Allocation) %>%
  select(`Transmission Risk` = trnsm,  Age = age, Allocation, `mean (95% UI)` = mean_CI)  %>%
  pivot_wider(id_cols = c("Transmission Risk", "Age"), names_from = "Allocation", values_from = "mean (95% UI)")  -> df_age2_tab

write_csv(df_age2_tab, "~/prep_need/output_data/age_rate_tab.csv")

df_age2 %>%
  # filter(Allocation != "100% coverage (PrEP indications)") %>%
  ggplot(aes(x = age, y = 100*mean_prep/pop, fill = Allocation)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = 100*lower_95/pop, ymax = 100*upper_95/pop), position = position_dodge(0.7),  width = 0.25) +
  labs(
    x = "Age",
    y = "Rate per 100 people") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    axis.text.y = element_text( hjust = 1, size=16),
    axis.title = element_text(size=16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16), 
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white") 
  ) +
  facet_wrap(~trnsm, ncol=4) + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("purple", "#BDD7E7","#3182BD", "#FDAE61", "#F46D43" ) ) -> p4.2

ggsave(here("output_figures/epi-allocation-age-rate.png"), plot = p4.2, width = 16, height =6, dpi = 300)


####### 
## Extra figure - MSM age distribution by R/E

results2 %>%
  filter(trnsm == "msm" & simulation=="1") %>%
  select(state, age, race, pop) %>%
  group_by(age, race) %>%
  summarize(pop=sum(pop)) %>%
  ungroup() %>%
  group_by(race) %>%
  mutate(prop_pop = pop / sum(pop),
         check = sum(prop_pop)) %>%
  ungroup() %>% 
  ggplot(aes(x=age, y=100*prop_pop, fill=race)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") ) +
  ylab("percentage") +
  theme_bw() -> pextra

ggsave(here("output_figures/msm_age_race.png"), plot = pextra, width = 6, height =3, dpi = 300)

