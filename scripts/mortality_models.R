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
ages_children <- c("<1", "1to4")
ages_adults <- c("5to49", "50to64", "65+")


include_viruses_children = c("rsv_proxy_alternative","hmpv_proxy","rhino_proxy","covid_proxy")
include_viruses_adults = c("rsv_proxy_alternative","hmpv_proxy","covid_proxy")



# Respiratory Any Position ------------------------------------------------
resp_mc_children = bootstrap_block_residual(dat=dat,
                                    y_value="mort_resp_any_rate", 
                                    ages=ages_children, 
                                    lags = c(1,2,3,3,2),
                                    include_viruses_A = include_viruses_children,
                                    include_viruses_B = include_viruses_children,
                                    include_viruses_C = include_viruses_children,
                                    include_viruses_D = include_viruses_children,
                                    include_viruses_E = include_viruses_children,
                                    block_length=6,
                                    n_boot = 1000) %>% 
  mutate(cause = 'Respiratory (MC)')

resp_mc_adults = bootstrap_block_residual(dat=dat,
                                            y_value="mort_resp_any_rate", 
                                            ages=ages_adults, 
                                          lags = c(1,2,3,3,2),
                                            include_viruses_A = include_viruses_adults,
                                            include_viruses_B = include_viruses_adults,
                                            include_viruses_C = include_viruses_adults,
                                            include_viruses_D = include_viruses_adults,
                                            include_viruses_E = include_viruses_adults,
                                            block_length=6,
                                            n_boot = 1000) %>% 
  mutate(cause = 'Respiratory (MC)')

resp_mc = rbind(resp_mc_children,resp_mc_adults)
saveRDS(resp_mc,"model_output/mort_resp_any.rds")

# Respiratory First Position ----------------------------------------------
resp_uc_children = bootstrap_block_residual(dat=dat,
                                      y_value="mort_resp_underlying_rate", 
                                      ages=ages_children, 
                                      lags = c(1,2,3,3,2),
                                      include_viruses_A = include_viruses_children,
                                      include_viruses_B = include_viruses_children,
                                      include_viruses_C = include_viruses_children,
                                      include_viruses_D = include_viruses_children,
                                      include_viruses_E = include_viruses_children,
                                      block_length=6,
                                      n_boot = 1000) %>% 
  mutate(cause = 'Respiratory (UC)')

resp_uc_adults = bootstrap_block_residual(dat=dat,
                                   y_value="mort_resp_underlying_rate", 
                                   ages=ages_adults, 
                                   lags = c(1,2,3,3,2),
                                   include_viruses_A = include_viruses_adults,
                                   include_viruses_B = include_viruses_adults,
                                   include_viruses_C = include_viruses_adults,
                                   include_viruses_D = include_viruses_adults,
                                   include_viruses_E = include_viruses_adults,
                                   block_length=6,
                                   n_boot = 1000) %>% 
  mutate(cause = 'Respiratory (UC)')

resp_uc = rbind(resp_uc_children, resp_uc_adults)
saveRDS(resp_uc,"model_output/mort_resp_underlying.rds")

# Respiratory or Circulatory First Position -------------------------------
rc_uc_children = bootstrap_block_residual(dat=dat,
                                    y_value="mort_respcirc_underlying_rate", 
                                    ages=ages_children, 
                                    lags = c(1,2,3,3,2),
                                    include_viruses_A = include_viruses_children,
                                    include_viruses_B = include_viruses_children,
                                    include_viruses_C = include_viruses_children,
                                    include_viruses_D = include_viruses_children,
                                    include_viruses_E = include_viruses_children,
                                    block_length=6,
                                    n_boot = 1000) %>% 
  mutate(cause = 'Respiratory + Circulatory (UC)')

rc_uc_adults = bootstrap_block_residual(dat=dat,
                                          y_value="mort_respcirc_underlying_rate", 
                                          ages=ages_adults, 
                                          lags = c(1,2,3,3,2),
                                          include_viruses_A = include_viruses_adults,
                                          include_viruses_B = include_viruses_adults,
                                          include_viruses_C = include_viruses_adults,
                                          include_viruses_D = include_viruses_adults,
                                          include_viruses_E = include_viruses_adults,
                                          block_length=6,
                                          n_boot = 1000) %>% 
  mutate(cause = 'Respiratory + Circulatory (UC)')
rc_uc = rbind(rc_uc_children, rc_uc_adults)
saveRDS(rc_uc,"model_output/mort_respcirc_underlying.rds")


