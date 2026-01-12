rm(list=ls())

library(tidyverse)
library(mgcv)
library(splines)
library(zoo)
library(cowplot)
library(cdcfluview)
library(data.table)
library(ggh4x)
library(parallel)

'%notin%' = Negate("%in%")

source("R/model_functions.R")

dat = readRDS("data/data_public.rds") 

ages <- c("<1", "1to4", "5to49", "50to64", "65+")
include_viruses = c("rsv_proxy_alternative","hmpv_proxy","rhino_proxy","covid_proxy")


# Respiratory Any Position ------------------------------------------------
resp_any = bootstrap_block_residual(dat=dat,
                                      y_value="hosp_resp_any_rate", 
                                      ages=ages, 
                                      lags = c(rep(0,5)),
                                      include_viruses_A = include_viruses,
                                      include_viruses_B = include_viruses,
                                      include_viruses_C = include_viruses,
                                      include_viruses_D = include_viruses,
                                      include_viruses_E = include_viruses,
                                      block_length=6,
                                      n_boot = 1000) %>% 
  mutate(cause = 'Respiratory (Any Position)')
saveRDS(resp_any,"model_output/hosp_resp_any.rds")

# Respiratory First Position ----------------------------------------------
resp_first = bootstrap_block_residual(dat=dat,
                                      y_value="hosp_resp_1st_rate", 
                                      ages=ages, 
                                      lags = c(rep(0,5)),
                                      include_viruses_A = include_viruses,
                                      include_viruses_B = include_viruses,
                                      include_viruses_C = include_viruses,
                                      include_viruses_D = include_viruses,
                                      include_viruses_E = include_viruses,
                                      block_length=6,
                                      n_boot = 1000) %>% 
  mutate(cause = 'Respiratory (First Position)')
saveRDS(resp_first,"model_output/hosp_resp_1st.rds")

# Respiratory or Circulatory First Position -------------------------------
rc_first = bootstrap_block_residual(dat=dat,
                                      y_value="hosp_respcirc_1st_rate", 
                                      ages=ages, 
                                      lags = c(rep(0,5)),
                                      include_viruses_A = include_viruses,
                                      include_viruses_B = include_viruses,
                                      include_viruses_C = include_viruses,
                                      include_viruses_D = include_viruses,
                                      include_viruses_E = include_viruses,
                                      block_length=6,
                                      n_boot = 1000) %>% 
  mutate(cause = 'Respiratory + Circulatory (First Position)')
saveRDS(rc_first,"model_output/hosp_respcirc_1st.rds")

