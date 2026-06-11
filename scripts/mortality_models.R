rm(list=ls())

library(tidyverse)
library(mgcv)
library(splines)
library(zoo)
library(cowplot)
library(cdcfluview)
library(data.table)
library(ggh4x)

'%notin%' = Negate("%in%")

source("R/model_functions.R")

dat = readRDS("data/data_public.rds") 

ages <- c("<1", "1to4", "5to49", "50to64", "65+")

include_viruses = c("rsv_proxy_pediatric","hmpv_proxy","rhino_proxy","covid_proxy_alternative")


# Respiratory Any Position ------------------------------------------------
resp_mc = bootstrap_block_residual(dat=dat,
                                    y_value="mort_resp_any_rate_smooth", 
                                    ages=ages, 
                                    lags = c(1,2,2,3,2),
                                    include_viruses_A = include_viruses,
                                    include_viruses_B = include_viruses,
                                    include_viruses_C = include_viruses,
                                    include_viruses_D = include_viruses,
                                    include_viruses_E = include_viruses,
                                    block_length=6,
                                    n_boot = 1000) %>% 
  mutate(cause = 'Respiratory (MC)')

saveRDS(resp_mc,"main_results/mort_resp_any.rds")

# Respiratory First Position ----------------------------------------------
resp_uc = bootstrap_block_residual(dat=dat,
                                      y_value="mort_resp_underlying_rate_smooth", 
                                      ages=ages, 
                                      lags = c(1,2,2,3,2),
                                      include_viruses_A = include_viruses,
                                      include_viruses_B = include_viruses,
                                      include_viruses_C = include_viruses,
                                      include_viruses_D = include_viruses,
                                      include_viruses_E = include_viruses,
                                      block_length=6,
                                      n_boot = 1000) %>% 
  mutate(cause = 'Respiratory (UC)')


saveRDS(resp_uc,"main_results/mort_resp_underlying.rds")

# Respiratory or Circulatory First Position -------------------------------
rc_uc= bootstrap_block_residual(dat=dat,
                                    y_value="mort_respcirc_underlying_rate_smooth", 
                                    ages=ages, 
                                    lags = c(1,2,2,3,2),
                                    include_viruses_A = include_viruses,
                                    include_viruses_B = include_viruses,
                                    include_viruses_C = include_viruses,
                                    include_viruses_D = include_viruses,
                                    include_viruses_E = include_viruses,
                                    block_length=6,
                                    n_boot = 1000) %>% 
  mutate(cause = 'Respiratory + Circulatory (UC)')


saveRDS(rc_uc,"main_results/mort_respcirc_underlying.rds")

