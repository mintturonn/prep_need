
#        rm(list = ls())
#        .rs.restartR()

library(here)
library(tidyverse)
library(tidycensus)
library(readxl)
library(scales)
# source("~/prep_need/code/imis_fun.R")

source(here('code/acs_fun.R')) ## old ACS definition
source("~/prep_need/code/prep_model_pars_sir.R")
source("~/prep_need/code/calibration_data.R")
source("~/prep_need/code/prior_sir_funs.R")
source("~/prep_need/code/prep_model_sir_fun.R")
source("~/prep_need/code/ll_sir_fun.R")

source("~/prep_need/code/heatmap_priors_sample.R")
source("~/prep_need/code/heatmap_post_sample.R")

# populations sizes needed for the model outpus
read_csv(here("data/acs5_state_sex_age_race.csv")) %>%
  mutate(state = gsub("_", " ", state)) -> pop_asr

pop_asr %>%
  filter(sex=="Male") %>%
  left_join(msmpr, by = ("state")) %>%
  mutate( pop = n *prev_extract ) %>%
  select(-n, -n_se, -prev_extract) -> asr_msm

pop_asr %>%
  filter(sex=="Male") %>%
  left_join(msmpr, by = ("state")) %>%
  mutate( pop = n *(1-prev_extract) ) %>%
  select(-n, -n_se, -prev_extract) -> asr_msw

pop_asr %>%
  filter(sex=="Female") %>%
  rename(pop = n) %>%
  select(-n_se) -> asr_wsm


priors2 <- NULL
priors2$params_nat <- as.matrix(read_csv(here("output_data/pnat.csv")))
priors2$params_st_msm_pr <- as.matrix(read_csv(here("output_data/pr_msm.csv")))
priors2$params_st_wsm_pr <- as.matrix(read_csv(here("output_data/pr_wsm.csv")))
priors2$params_st_msw_pr <- as.matrix(read_csv(here("output_data/pr_msw.csv")))
priors2$params_st_msm_vs <- as.matrix(read_csv(here("output_data/vs_msm.csv")))
priors2$params_st_wsm_vs <- as.matrix(read_csv(here("output_data/vs_wsm.csv")))
priors2$params_st_msw_vs <- as.matrix(read_csv(here("output_data/vs_msw.csv")))

numbr=50

df1 <- heat_priors(numbr=50, nsim=1000)
df2 <- heat_post(priors2, numbr=50)

# Combine datasets
df <- bind_rows(df1, df2) %>%
  mutate(
    trnsm = as.factor(trnsm),
    prev_bin = cut(prev_adjusted, breaks = quantile(prev_adjusted, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE, right = FALSE),
    inc_bin = cut(inc_value, breaks = quantile(inc_value, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE),
    contact_bin = cut(contact, breaks = numbr, labels = FALSE),
    c_nact_bin = cut(c.nact, breaks = numbr, labels = FALSE)
  )

#######################

# Calculate midpoints for each bin
calc_midpoints <- function(breaks) {
  breaks <- gsub("\\s*\\(\\s*", "", breaks)
  breaks <- gsub("\\s*\\[\\s*", "", breaks)
  breaks <- gsub("\\s*\\)\\s*", "", breaks)
  breaks <- gsub("\\s*\\]\\s*", "", breaks)
  breaks <- strsplit(breaks, ",")
  midpoints <- sapply(breaks, function(x) mean(as.numeric(x)))
  return(midpoints)
}

contact_breaks <- cut(df$contact, breaks = numbr, include.lowest = TRUE)
c_nact_breaks <- cut(df$c.nact, breaks = numbr, include.lowest = TRUE)

contact_midpoints <- calc_midpoints(levels(contact_breaks))
c_nact_midpoints <- calc_midpoints(levels(c_nact_breaks))

# factor
# df$contact_bin <- as.numeric(as.character(factor(cut(df$contact, breaks = numbr, labels = contact_midpoints), levels = contact_midpoints)
# df$c_nact_bin <- factor(cut(df$c.nact, breaks = numbr, labels = c_nact_midpoints), levels = c_nact_midpoints)

 df$contact_bin_num <- as.numeric(as.character(factor(cut(df$contact, breaks = numbr, labels = contact_midpoints), levels = contact_midpoints), labels = contact_midpoints))
 df$c_nact_bin_num <-  as.numeric(as.character(factor(cut(df$c.nact, breaks = numbr, labels = c_nact_midpoints), levels = c_nact_midpoints), labels = contact_midpoints))

# # Convert factors to numeric midpoints and handle NA values
# df <- df %>%
#   mutate(
#     contact_bin_num = ifelse(is.na(contact_bin), NA, as.numeric(as.character(factor(contact_bin, levels = seq_along(contact_midpoints), labels = contact_midpoints)))),
#     c_nact_bin_num = ifelse(is.na(c_nact_bin), NA, as.numeric(as.character(factor(c_nact_bin, levels = seq_along(c_nact_midpoints), labels = c_nact_midpoints)))),
#     inc_log10 = log10(inc_value + 1e-9)  # Transform to log scale, adding a small constant to handle zeros
#   )

# Select every other midpoint for axis labels
x_breaks <- round(contact_midpoints[seq(1, length(contact_midpoints), by = 5)],0)
y_breaks <- round(c_nact_midpoints[seq(1, length(c_nact_midpoints), by = 5)],0)

# Define a custom color gradient with more colors
colors <- c("darkblue", "blue", "lightblue", "lightgreen", "gray60", "orange", "red", "darkred")

# is this ccorrect?
collims <- c(10^-9, 10^-4, 10^-3, 10^-2, 10^-1, 0.95)
values <- rescale(log10(collims))


ggplot(df, aes(x = contact_bin_num, y = c_nact_bin_num, fill = inc_log10)) +
  geom_tile(color = "white") +
  geom_tile(data = subset(df, sample == "posterior"), fill = NA, color = "black", size = 0.5) + 
  scale_fill_gradientn(
    colors = colors, 
    values = values,
    limits = c(min(df$inc_log10, na.rm = TRUE), max(df$inc_log10, na.rm = TRUE)),
    breaks = log10(collims),       # 10-fold differences
    labels =  round(100* c(collims), 4),
    oob = scales::squish) +  
  labs( x = "Number of partners annually", y = "Number of condomless acts per partnership", fill = "Average incidence (%)") +
  scale_x_continuous(breaks = x_breaks, expand = c(0, 0)) +  # Set fewer x-axis breaks to show midpoints
  scale_y_continuous(breaks = y_breaks, expand = c(0, 0)) +  # Set fewer y-axis breaks to show midpoints
  facet_grid(trnsm ~ prev_bin) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 18),  # Rotate and adjust x-axis labels
    axis.text.y = element_text(size = 18),                         # Adjust y-axis labels size
    axis.title.x = element_text(size = 18),                        # Adjust x-axis title size
    axis.title.y = element_text(size = 18),                        # Adjust y-axis title size
    strip.text = element_text(size = 18),                           # Adjust facet label titles size
    panel.grid.major = element_line(color = "grey80", size = 0.45),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 18, vjust=1),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, "cm")
  ) -> p

# Save the plot
ggsave(here("output/heatmap_incidence-new.png"), 
       plot = p, 
       width = 24, 
       height = 16, 
       dpi = 500)





