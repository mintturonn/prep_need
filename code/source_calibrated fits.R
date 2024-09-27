
library(here)
library(tidyverse)
# library(tidycensus)
library(readxl)
library(ggstance)

source("~/prep_need/code/prep_model_sir_fun.R")

pnat <- as.matrix(read_csv(here("output_data/pnat.csv")))
pr.msm <- as.matrix(read_csv(here("output_data/pr_msm.csv")))
pr.wsm <- as.matrix(read_csv(here("output_data/pr_wsm.csv")))
pr.msw <- as.matrix(read_csv(here("output_data/pr_msw.csv")))
pr.pwid <- as.matrix(read_csv(here("output_data/pr_pwid.csv")))

vs.msm <- as.matrix(read_csv(here("output_data/vs_msm.csv")))
vs.wsm <- as.matrix(read_csv(here("output_data/vs_wsm.csv")))
vs.msw <- as.matrix(read_csv(here("output_data/vs_msw.csv")))
vs.pwid <- as.matrix(read_csv(here("output_data/vs_pwid.csv")))

source("~/prep_need/code/calibration_figures.R")

